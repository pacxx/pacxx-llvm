// Created by Michael Haidl and lars

#define PACXX_PASS_NAME "PACXXNativeLinker"
#include "Log.h"

#include "ModuleHelper.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "../../IR/ConstantsContext.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace llvm {

    struct PACXXNativeLinker : public ModulePass {
        static char ID;

        PACXXNativeLinker() : ModulePass(ID) {}

        virtual ~PACXXNativeLinker() {}

        virtual bool runOnModule(Module &M);

    private:
      void createSharedMemoryBuffer(Module &M, Function * wrapper, Function *kernel, Value *sm_size);
      void createInternalSharedMemoryBuffer(Module &M, set<GlobalVariable*> &globals, BasicBlock *sharedMemBB);
      void createExternalSharedMemoryBuffer(Module &M, set<GlobalVariable*> &globals, Value *sm_size,
                                                             BasicBlock *sharedMemBB);
      set<GlobalVariable *> getSMGlobalsUsedByKernel(Module &M, Function *kernel, bool internal);

    private:
        bool _barrier;
    };

    bool PACXXNativeLinker::runOnModule(Module &M) {

      auto &ctx = M.getContext();
      auto foo = M.getFunction("foo");
      auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

      for (auto &F : kernels) {

        _barrier = false;

        // if the kernel has a barrier use a special foo function
        if (F->hasFnAttribute("barrier")) {
          _barrier = true;
          foo = M.getFunction("__barrier__foo__" + F->getName().str());
        }

        SmallVector<Type *, 8> Params;
        Params.push_back(IntegerType::getInt32Ty(ctx));
        Params.push_back(IntegerType::getInt32Ty(ctx));
        Params.push_back(IntegerType::getInt32Ty(ctx));
        Params.push_back(IntegerType::getInt32Ty(ctx));
        Params.push_back(IntegerType::getInt32Ty(ctx));
        Params.push_back(IntegerType::getInt32Ty(ctx));
        Params.push_back(IntegerType::getInt32Ty(ctx));
        Params.push_back(PointerType::getInt8PtrTy(ctx));

        // we can always use void, because we work with valid cuda kernels and they need to return void
        FunctionType *FTy = FunctionType::get(Type::getVoidTy(ctx), Params, false);
        auto wrappedF =
                Function::Create(FTy, GlobalValue::LinkageTypes::ExternalLinkage,
                                 std::string("__wrapped__") + F->getName().str(), &M);

        auto argIt = wrappedF->arg_begin();
        (argIt++)->setName("bidx");
        (argIt++)->setName("bidy");
        (argIt++)->setName("bidz");

        SmallVector<Value *, 1> args;
        args.push_back(&*argIt);
        (argIt++)->setName("maxidx");
        args.push_back(&*argIt);
        (argIt++)->setName("maxidy");
        args.push_back(&*argIt);
        (argIt++)->setName("maxidz");

        (argIt++)->setName("sm_size");

        (argIt++)->setName("args");

        BasicBlock *entry = BasicBlock::Create(ctx, "entry", wrappedF);


        auto term = ReturnInst::Create(ctx, entry);
        auto CI = CallInst::Create(foo, args, "", term);

        // now that we have a call inst inline it!
        InlineFunctionInfo IFI;
        InlineFunction(CI, IFI, nullptr, false);

        AllocaInst *idx = nullptr, *idy = nullptr, *idz = nullptr;

        for (auto &B : *wrappedF) {
          for (auto &I : B) {
            if (auto *alloca = dyn_cast<AllocaInst>(&I)) {
              if (alloca->getName().startswith_lower("__x") && !idx)
                idx = alloca;
              else if (alloca->getName().startswith_lower("__y") && !idy)
                idy = alloca;
              else if (alloca->getName().startswith_lower("__z") && !idz)
                idz = alloca;
            }
          }
        }


        // find the dummy kernel call and replace it with a call to the actual
        // kernel
        SmallVector<Value *, 8> kernel_args;
        Value *bidx = nullptr, *bidy = nullptr, *bidz = nullptr;
        Value *maxidx = nullptr, *maxidy = nullptr, *maxidz = nullptr;
        Value *sm_size = nullptr;
        Function *dummy;
        Function *seq_dummy = M.getFunction("__dummy_kernel");
        Function *vec_dummy = M.getFunction("__vectorized__dummy_kernel");
        // if the normal dummy call does not exist at this point we know that a sequential version is not needed
        // so we use the vectorized version
        if (seq_dummy)
          dummy = seq_dummy;
        else
          dummy = vec_dummy;

        CallInst *remove = nullptr;
        for (auto U : dummy->users()) {
          if ((CI = dyn_cast<CallInst>(U))) {
            if (CI->getParent()->getParent() == wrappedF) {
              auto argIt = wrappedF->getArgumentList().begin();
              bidx = &*argIt;
              ++argIt;
              bidy = &*argIt;
              ++argIt;
              bidz = &*argIt;
              ++argIt;
              maxidx = &*argIt;
              ++argIt;
              maxidy = &*argIt;
              ++argIt;
              maxidz = &*argIt;
              ++argIt;
              sm_size = &*argIt;
              ++argIt;

              // construct the kernel arguments from the char*
              BasicBlock *constructKernelArgs = BasicBlock::Create(ctx, "constructArgs", wrappedF, entry);


              auto int8ptr_type = Type::getInt8PtrTy(ctx);
              auto int8ptr_align = M.getDataLayout().getPrefTypeAlignment(int8ptr_type);

              auto *alloc_args = new AllocaInst(int8ptr_type, nullptr, int8ptr_align, (&*argIt)->getName(),
                                                constructKernelArgs);
              new StoreInst(&*argIt, alloc_args, false, int8ptr_align, constructKernelArgs);
              auto *args_load = new LoadInst(alloc_args, "args", false, int8ptr_align, constructKernelArgs);

              size_t offset = 0;

              for (auto &A : F->getArgumentList()) {

                if (A.getName() == "idx") {
                  kernel_args.push_back(new LoadInst(idx, "idx", CI));
                  continue;
                } else if (A.getName() == "idy") {
                  kernel_args.push_back(new LoadInst(idy, "idy", CI));
                  continue;
                } else if (A.getName() == "idz") {
                  kernel_args.push_back(new LoadInst(idz, "idz", CI));
                  continue;
                } else if (A.getName() == "native.struct") {
                  //ignore next mem because we already set it in the barrier pass
                  continue;
                }

                auto arg_size = M.getDataLayout().getTypeAllocSize(A.getType());
                auto arg_alignment = M.getDataLayout().getPrefTypeAlignment(A.getType());

                auto arg_offset = (offset + arg_alignment - 1) & ~(arg_alignment - 1);
                // consider offset in char array
                auto *elem_ptr = GetElementPtrInst::CreateInBounds(Type::getInt8Ty(ctx), args_load,
                                                                   ConstantInt::get(Type::getInt64Ty(ctx), arg_offset),
                                                                   "", constructKernelArgs);
                // Cast to apropriate size
                auto *casted = new BitCastInst(elem_ptr, PointerType::getUnqual(A.getType()), "", constructKernelArgs);
                auto *casted_load = new LoadInst(casted, A.getName(), false,
                                                 M.getDataLayout().getPrefTypeAlignment(casted->getType()),
                                                 constructKernelArgs);

                kernel_args.push_back(casted_load);

                offset = arg_offset + arg_size;
              }

              // branch to normal entry block
              llvm::BranchInst::Create(entry, constructKernelArgs);

              remove = CI;
              if (_barrier) {
                CallInst *actual_call = dyn_cast<CallInst>(&*(--remove->getIterator()));
                assert(actual_call && "cant find actual call");
                CI = actual_call;
                for (unsigned i = 0; i < kernel_args.size(); ++i)
                  CI->setArgOperand(i, kernel_args[i]);
              } else if (seq_dummy)
                CI = CallInst::Create(F, kernel_args, "", CI);
              else
                CI = CallInst::Create(M.getFunction("__vectorized__" + F->getName().str()), kernel_args, "", CI);
              break;
            }
          }
        }

        if (remove)
          remove->eraseFromParent();

        // time to inline the original kernel into the wrapper
        InlineFunction(CI, IFI, nullptr, false);

        //create shared memory buffer
        createSharedMemoryBuffer(M, wrappedF, F, sm_size);

        //If a vectorized version and a sequential version of the kernel exists
        // the vectorized version also needs to be inlined
        if (seq_dummy && vec_dummy) {
          CallInst *remove_vec = nullptr;
          Function *vec_kernel = M.getFunction("__vectorized__" + F->getName().str());
          for (auto U : vec_dummy->users()) {
            if ((CI = dyn_cast<CallInst>(U))) {
              if (CI->getParent()->getParent() == wrappedF) {
                remove_vec = CI;
                if (_barrier) {
                  CallInst *actual_call = dyn_cast<CallInst>(&*(--remove_vec->getIterator()));
                  assert(actual_call && "cant find actual call");
                  CI = actual_call;
                  for (unsigned i = 0; i < kernel_args.size(); ++i)
                    CI->setArgOperand(i, kernel_args[i]);
                }
                else
                  CI = CallInst::Create(vec_kernel, kernel_args, "", CI);
                break;
              }
            }
          }

          if (remove_vec)
            remove_vec->eraseFromParent();

          InlineFunction(CI, IFI, nullptr, false);
        }


        vector<CallInst *> dead_calls;
        for (auto &B : *wrappedF) {
          for (auto &I : B) {
            if (auto CI = dyn_cast<CallInst>(&I)) {
              auto called = CI->getCalledFunction();
              if (called && called->isIntrinsic()) {
                auto intrin_id = called->getIntrinsicID();
                if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_tid_x) {
                  auto LI = new LoadInst(idx, "idx", CI);
                  CI->replaceAllUsesWith(LI);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_tid_y) {
                  auto LI = new LoadInst(idy, "idy", CI);
                  CI->replaceAllUsesWith(LI);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_tid_z) {
                  auto LI = new LoadInst(idz, "idz", CI);
                  CI->replaceAllUsesWith(LI);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ctaid_x) {
                  CI->replaceAllUsesWith(bidx);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ctaid_y) {
                  CI->replaceAllUsesWith(bidy);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ctaid_z) {
                  CI->replaceAllUsesWith(bidz);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ntid_x) {
                  CI->replaceAllUsesWith(maxidx);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ntid_y) {
                  CI->replaceAllUsesWith(maxidy);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ntid_z) {
                  CI->replaceAllUsesWith(maxidz);
                  dead_calls.push_back(CI);
                }

              }
            }
          }
        }


        for (auto I : dead_calls)
          I->eraseFromParent();
      }

      __verbose("Cleaning up");

      foo->eraseFromParent();

      //cleanup
      for (auto F : kernels) {
        Function *vec_F = M.getFunction("__vectorized__" + F->getName().str());
        F->eraseFromParent();
        if (vec_F)
          vec_F->eraseFromParent();
      }

      // if we still have a foo function erase it at this point
      // this can happen if we only have kernels with barriers
      if (foo = M.getFunction("foo"))
        foo->eraseFromParent();

      return true;
    }

    void PACXXNativeLinker::createSharedMemoryBuffer(Module &M, Function *wrapper, Function *kernel, Value *sm_size) {
      auto internal_sm = getSMGlobalsUsedByKernel(M, wrapper, true);
      auto external_sm = getSMGlobalsUsedByKernel(M, wrapper, false);

      BasicBlock *entry = &wrapper->front();
      BasicBlock *sharedMemBB = BasicBlock::Create(wrapper->getContext(), "shared mem", wrapper, entry);

      if(!internal_sm.empty()) {
        __verbose("internal shared memory found\n");
        createInternalSharedMemoryBuffer(M, internal_sm, sharedMemBB);
      }

      if(!external_sm.empty()) {
        __verbose("external shared memory found\n");
        createExternalSharedMemoryBuffer(M, external_sm, sm_size, sharedMemBB);
      }

      BranchInst::Create(entry, sharedMemBB);

      __verbose("created shared memory");
    }

    set<GlobalVariable *> PACXXNativeLinker::getSMGlobalsUsedByKernel(Module &M, Function *kernel, bool internal) {
      set<GlobalVariable *> sm;
      for (auto &GV : M.globals()) {
        if (internal ? GV.hasInternalLinkage() : GV.hasExternalLinkage() && GV.getType()->getAddressSpace() == 3) {
          for (User *GVUsers : GV.users()) {
            if (Instruction *Inst = dyn_cast<Instruction>(GVUsers)) {
              if (Inst->getFunction() == kernel) {
                sm.insert(&GV);
              }
            }

            if (ConstantExpr *constExpr = dyn_cast<ConstantExpr>(GVUsers))
              for (User *CExpUser : constExpr->users())
                if (Instruction *Inst = dyn_cast<Instruction>(CExpUser))
                  if (Inst->getFunction() == kernel)
                    sm.insert(&GV);
          }
        }
      }
      return sm;
    }

    void PACXXNativeLinker::createInternalSharedMemoryBuffer(Module &M, set<GlobalVariable*> &globals,
                                                             BasicBlock *sharedMemBB) {
      for (auto GV : globals) {
        SmallVector<Instruction *, 8> remove;
        Type *sm_type = GV->getType()->getElementType();
        AllocaInst *sm_alloc = new AllocaInst(sm_type, nullptr,
                                              M.getDataLayout().getABITypeAlignment(sm_type), "internal_sm",
                                              sharedMemBB);
        if (GV->hasInitializer() && !isa<UndefValue>(GV->getInitializer()))
          new StoreInst(GV->getInitializer(), sm_alloc, sharedMemBB);

        for (auto *GVUser : GV->users()) {
          if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(GVUser)) {
            SmallVector<Value *, 4> ValueOperands(GEP->op_begin(), GEP->op_end());
            ArrayRef<Value *> Ops(ValueOperands);
            GetElementPtrInst *newGEP = GEP->isInBounds() ?
                                        GetElementPtrInst::CreateInBounds(sm_type, sm_alloc, Ops.slice(1), "", GEP) :
                                        GetElementPtrInst::Create(sm_type, sm_alloc, Ops.slice(1), "", GEP);

            for (auto *GEPUser : GEP->users()) {
              if (Instruction *cast = dyn_cast<AddrSpaceCastInst>(GEPUser)) {
                cast->replaceAllUsesWith(newGEP);
                remove.push_back(cast);
              }
              else
                GEPUser->replaceUsesOfWith(GEP, newGEP);
            }
            remove.push_back(GEP);
          }
        }
        for (auto inst : remove) {
          inst->eraseFromParent();
        }
        GV->eraseFromParent();
      }
    }

    void PACXXNativeLinker::createExternalSharedMemoryBuffer(Module &M, set<GlobalVariable*> &globals, Value *sm_size,
                                                             BasicBlock *sharedMemBB) {
      for (auto GV : globals) {
        Type *sm_type = GV->getType()->getElementType()->getPointerElementType();
        Value *typeSize = ConstantInt::get(Type::getInt32Ty(M.getContext()),
                                           M.getDataLayout().getTypeSizeInBits(sm_type) / 8);
        //calc number of elements
        BinaryOperator *div = BinaryOperator::CreateUDiv(sm_size, typeSize, "numElem", sharedMemBB);
        AllocaInst *sm_alloc = new AllocaInst(sm_type, div, M.getDataLayout().getABITypeAlignment(sm_type),
                                              "external_sm", sharedMemBB);

        // handle special constExpr cast
        SmallVector<ConstantExpr *, 4> const_user;
        SmallVector<Instruction *, 4> user;
        for (auto *U : GV->users()) {
          if (isa<ConstantExpr>(U)) {
            const_user.push_back(cast<ConstantExpr>(U));
          } else if (isa<Instruction>(U))
            user.push_back(cast<Instruction>(U));
        }

        SmallVector<Value *, 4> UUsers;
        for (auto *U : const_user) {
          UUsers.clear();
          for (auto *UU : U->users())
            UUsers.push_back(UU);
          for (auto *UU : UUsers) {
            if (LoadInst *UI = dyn_cast<LoadInst>(UU)) {
              UI->replaceAllUsesWith(sm_alloc);
              UI->eraseFromParent();
            }
          }
        }

        for (auto *U: user) {
          U->replaceUsesOfWith(GV, sm_alloc);
        }

        GV->eraseFromParent();
      }
    }


    char PACXXNativeLinker::ID = 0;
    static RegisterPass<PACXXNativeLinker>
            X("pacxx_native", "Inlines functions into kernels", false, false);

    Pass *createPACXXNativeLinkerPass() { return new PACXXNativeLinker(); }
}
