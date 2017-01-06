#define PACXX_PASS_NAME "PACXXNativeLinker"
#define USE_STANDALONE 1

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
      GetElementPtrInst* createSMGEP(Value *value, Type *type, GetElementPtrConstantExpr *constantGEP);
      set<GlobalVariable *> getSMGlobalsUsedByKernel(Module &M, Function *kernel, bool internal);
    };

    bool PACXXNativeLinker::runOnModule(Module &M) {

      auto &ctx = M.getContext();
      auto foo = M.getFunction("foo");
      auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

      for (auto &F : kernels) {

        // if the kernel has a barrier use a special foo function
        if(F->hasFnAttribute("barrier")) {
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

        FunctionType *FTy = FunctionType::get(F->getReturnType(), Params, false);
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
        InlineFunction(CI, IFI);

        AllocaInst *idx = nullptr, *idy = nullptr, *idz = nullptr;

        for (auto &B : *wrappedF) {
          for (auto &I : B) {
            if (auto *alloca = dyn_cast<AllocaInst>(&I)) {
              if (alloca->getName().startswith_lower("__x"))
                idx = alloca;
              else if (alloca->getName().startswith_lower("__y"))
                idy = alloca;
              else if (alloca->getName().startswith_lower("__z"))
                idz = alloca;
            }
          }
        }

        // find the dummy kernel call and replace it with a call to the actual
        // kernel
        SmallVector<Value *, 8> kernel_args;
        Value *bidx = nullptr, *bidy = nullptr, *bidz = nullptr;
        Value *maxidx = nullptr, *maxidy = nullptr, *maxidz = nullptr;
        Value* sm_size = nullptr;
        Function* dummy;
        Function* seq_dummy = M.getFunction("__dummy_kernel");
        Function* vec_dummy = M.getFunction("__vectorized__dummy_kernel");
        // if the normal dummy call does not exist at this point we know that a sequential version is not needed
        // so we use the vectorized version
        if(seq_dummy)
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

              auto* alloc_args = new AllocaInst(int8ptr_type, nullptr, int8ptr_align, (&*argIt)->getName(), constructKernelArgs);
              new StoreInst(&*argIt, alloc_args, false, int8ptr_align, constructKernelArgs);
              auto* args_load = new LoadInst(alloc_args, "args", false, int8ptr_align, constructKernelArgs);

              size_t offset = 0;

              for(auto &A : F->getArgumentList()) {

                if(A.getName() == "idx") {
                  kernel_args.push_back(new LoadInst(idx, "idx", CI));
                  continue;
                }
                else if (A.getName() == "idy") {
                  kernel_args.push_back(new LoadInst(idy, "idy", CI));
                  continue;
                }
                else if (A.getName() == "idz") {
                  kernel_args.push_back(new LoadInst(idz, "idz", CI));
                  continue;
                }

                auto arg_size = M.getDataLayout().getTypeAllocSize(A.getType());
                auto arg_alignment = M.getDataLayout().getPrefTypeAlignment(A.getType());

                auto arg_offset = (offset + arg_alignment - 1) & ~(arg_alignment - 1);
                // consider offset in char array
                auto *elem_ptr = GetElementPtrInst::CreateInBounds(Type::getInt8Ty(ctx), args_load,
                                                                   ConstantInt::get(Type::getInt64Ty(ctx), arg_offset), "", constructKernelArgs);
                // Cast to apropriate size
                auto *casted = new BitCastInst(elem_ptr, PointerType::getUnqual(A.getType()), "", constructKernelArgs);
                auto *casted_load = new LoadInst(casted, A.getName(), false,
                                                 M.getDataLayout().getPrefTypeAlignment(casted->getType()), constructKernelArgs);

                kernel_args.push_back(casted_load);

                offset = arg_offset + arg_size;
              }

              // branch to normal entry block
              llvm::BranchInst::Create(entry, constructKernelArgs);

              remove = CI;
              if(seq_dummy)
                CI = CallInst::Create(F,
                                      kernel_args,
                                      "",
                                      CI);
              else
                CI = CallInst::Create(M.getFunction("__vectorized__" + F->getName().str()),
                                      kernel_args,
                                      "",
                                      CI);
              break;
            }
          }
        }

        if (remove)
          remove->eraseFromParent();

        // time to inline the original kernel into the wrapper
        InlineFunction(CI, IFI);

        //create shared memory buffer
        createSharedMemoryBuffer(M, wrappedF, F, sm_size);

        //If a vectorized version and a sequential version of the kernel exists
        // the vectorized version also needs to be inlined
        if(seq_dummy && vec_dummy) {
          CallInst *remove_vec = nullptr;
          Function *vec_kernel = M.getFunction("__vectorized__" + F->getName().str());
          for (auto U : vec_dummy->users()) {
            if ((CI = dyn_cast<CallInst>(U))) {
              if (CI->getParent()->getParent() == wrappedF) {
                remove_vec = CI;
                CI = CallInst::Create(vec_kernel, kernel_args, "", CI);
                break;
              }
            }
          }

          if (remove_vec)
            remove_vec->eraseFromParent();

          InlineFunction(CI, IFI);
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
                }else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ntid_x) {
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

      foo->eraseFromParent();

      //cleanup
      for (auto F : kernels) {
        Function* vec_F = M.getFunction("__vectorized__"+F->getName().str());
        F->eraseFromParent();
        if(vec_F)
          vec_F->eraseFromParent();
      }

      return true;
    }

    void PACXXNativeLinker::createSharedMemoryBuffer(Module &M, Function *wrapper, Function *kernel, Value *sm_size) {
      auto internal_sm = getSMGlobalsUsedByKernel(M, kernel, true);
      auto external_sm = getSMGlobalsUsedByKernel(M, kernel, false);

      BasicBlock *entry = &wrapper->front();
      BasicBlock *sharedMemBB = BasicBlock::Create(kernel->getContext(), "shared mem", wrapper, entry);

      if(!internal_sm.empty())
        createInternalSharedMemoryBuffer(M, internal_sm, sharedMemBB);

      if(!external_sm.empty())
        createExternalSharedMemoryBuffer(M, external_sm, sm_size, sharedMemBB);

      BranchInst::Create(entry, sharedMemBB);
    }

    set<GlobalVariable *> PACXXNativeLinker::getSMGlobalsUsedByKernel(Module &M, Function *kernel, bool internal) {
      set<GlobalVariable *> sm;
      for (auto &GV : M.globals()) {
        if (internal ? GV.hasInternalLinkage() : GV.hasExternalLinkage() && GV.getType()->getAddressSpace() == 3) {
          for (User *U : GV.users()) {
            if (Instruction *Inst = dyn_cast<Instruction>(U)) {
              if (Inst->getParent()->getParent() == kernel) {
                sm.insert(&GV);
              }
            }
          }
        }
      }
      return sm;
    }

    void PACXXNativeLinker::createInternalSharedMemoryBuffer(Module &M, set<GlobalVariable*> &globals,
                                                             BasicBlock *sharedMemBB) {
      for (auto GV : globals) {
        Type *sm_type = GV->getType()->getElementType();
        AllocaInst *sm_alloc = new AllocaInst(sm_type, nullptr,
                                              M.getDataLayout().getABITypeAlignment(sm_type), "internal_sm", sharedMemBB);
        if (GV->hasInitializer() && !isa<UndefValue>(GV->getInitializer()))
          new StoreInst(GV->getInitializer(), sm_alloc, sharedMemBB);

        // replacing special GEP used for GlobalVariables
        SmallVector<GetElementPtrConstantExpr *, 4> const_user;
        SmallVector<Instruction *, 4> user;
        for (auto *U : GV->users()) {
          if (isa<GetElementPtrConstantExpr>(U)) {
            const_user.push_back(cast<GetElementPtrConstantExpr>(U));
          } else if (isa<Instruction>(U))
            user.push_back(cast<Instruction>(U));
        }

        SmallVector<Value *, 4> UUsers;
        for (auto *U : const_user) {
          UUsers.clear();
          for (auto *UU : U->users())
            UUsers.push_back(UU);
          for (auto *UU : UUsers) {
            if (Instruction *UI = dyn_cast<Instruction>(UU)) {
              Instruction *NewU = createSMGEP(sm_alloc, sm_type, U);
              NewU->insertBefore(UI);
              UI->replaceUsesOfWith(U, NewU);
            }
          }
          U->dropAllReferences();
        }

        for (auto *U: user) {
          U->replaceUsesOfWith(GV, sm_alloc);
        }

        GV->eraseFromParent();
      }
    }

    GetElementPtrInst* PACXXNativeLinker::createSMGEP(Value *value, Type *type,
                                                      GetElementPtrConstantExpr * constantGEP) {
      SmallVector<Value *, 4> ValueOperands(constantGEP->op_begin(), constantGEP->op_end());
      ArrayRef<Value*> Ops(ValueOperands);
      auto *GO = cast<GEPOperator>(constantGEP);

      if(GO->isInBounds())
        return GetElementPtrInst::CreateInBounds(type, value, Ops.slice(1), "sm_gep");
      else
        return GetElementPtrInst::Create(type, value, Ops.slice(1), "sm_gep");
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
        SmallVector<LoadInst *,4> remove;
        for (auto *U : GV->users()) {
          if (isa<LoadInst>(U)) {
            remove.push_back(cast<LoadInst>(U));
            U->replaceAllUsesWith(sm_alloc);
          }
        }

        for(auto load : remove) {
          load->eraseFromParent();
        }

        GV->eraseFromParent();
      }
    }


    char PACXXNativeLinker::ID = 0;
    static RegisterPass<PACXXNativeLinker>
            X("pacxx_native", "Inlines functions into kernels", false, false);

    Pass *createPACXXNativeLinker() { return new PACXXNativeLinker(); }
}
