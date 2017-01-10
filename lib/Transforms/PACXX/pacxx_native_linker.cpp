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
      Instruction *createLivingValueArg(CallInst *CI ,
                                        Value * id_x, Value *id_y, Value *id_z,
                                        Value *max_x, Value *max_y, Value *max_z);
      AllocaInst *findLivingValueMem(CallInst *CI);
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
        InlineFunction(CI, IFI);

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
        LoadInst *load_x, *load_y, *load_z;
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

              load_x = new LoadInst(idx, "idx", CI);
              load_y = new LoadInst(idy, "idy", CI);
              load_z = new LoadInst(idz, "idz", CI);

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
                  kernel_args.push_back(load_x);
                  continue;
                } else if (A.getName() == "idy") {
                  kernel_args.push_back(load_y);
                  continue;
                } else if (A.getName() == "idz") {
                  kernel_args.push_back(load_z);
                  continue;
                }
                else if (A.getName() == "native.struct" && _barrier) {
                  kernel_args.push_back(createLivingValueArg(CI,
                                                             load_x, load_y, load_z,
                                                             maxidx, maxidy, maxidz));
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
              if (seq_dummy)
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
        if (seq_dummy && vec_dummy) {
          CallInst *remove_vec = nullptr;
          Function *vec_kernel = M.getFunction("__vectorized__" + F->getName().str());
          for (auto U : vec_dummy->users()) {
            if ((CI = dyn_cast<CallInst>(U))) {
              if (CI->getParent()->getParent() == wrappedF) {
                remove_vec = CI;
                if(_barrier)
                  *(kernel_args.end() -1) =
                          createLivingValueArg(CI, load_x, load_y, load_z, maxidx, maxidy, maxidz);
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

      M.dump();

      return true;
    }

    Instruction *PACXXNativeLinker::createLivingValueArg(CallInst *CI,
                                                         Value * id_x, Value *id_y, Value *id_z,
                                                         Value *max_x, Value *max_y, Value *max_z) {
      LLVMContext &ctx = CI->getContext();
      AllocaInst *nextMem = findLivingValueMem(CI);
      assert(nextMem && "cant find mem for living values");
      // calculate blockId
      BinaryOperator *mul_y_maxx = BinaryOperator::CreateMul(id_y, max_x, "", CI);
      BinaryOperator *mul_maxx_maxy = BinaryOperator::CreateMul(max_x, max_y, "", CI);
      BinaryOperator *mul_mulmaxxmaxy_z = BinaryOperator::CreateMul(mul_maxx_maxy, id_z, "", CI);
      BinaryOperator *add = BinaryOperator::CreateAdd(mul_y_maxx, mul_mulmaxxmaxy_z, "", CI);
      BinaryOperator *blockId = BinaryOperator::CreateAdd(id_x, add, "", CI);

      GetElementPtrInst *GEP = GetElementPtrInst::Create(nullptr, nextMem, blockId, "", CI);
      CastInst *cast = CastInst::Create(CastInst::BitCast, GEP, Type::getInt8PtrTy(ctx), "", CI);
      return cast;
    }

    AllocaInst *PACXXNativeLinker::findLivingValueMem(CallInst *CI) {
      BasicBlock *BB = CI->getParent();
      for(auto I = ++CI->getIterator(), IE = BB->end(); I != IE; ++I) {
        Instruction *inst = &*(I);
        if (isa<LoadInst>(inst) && inst->getName().startswith_lower("dummy")) {
          LoadInst *load = cast<LoadInst>(inst);
          AllocaInst *alloca = dyn_cast<AllocaInst>(load->getPointerOperand());
          load->eraseFromParent();
          return alloca;
        }
      }
      return nullptr;
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

    Pass *createPACXXNativeLinkerPass() { return new PACXXNativeLinker(); }
}
