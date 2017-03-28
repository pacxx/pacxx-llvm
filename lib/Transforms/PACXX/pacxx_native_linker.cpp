// Created by Michael Haidl and lars

#define PACXX_PASS_NAME "PACXXNativeLinker"
#include "Log.h"
#include "ModuleHelper.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace llvm {

    struct PACXXNativeLinker : public ModulePass {
        static char ID;

        PACXXNativeLinker() : ModulePass(ID) {}

        virtual ~PACXXNativeLinker() {}

        virtual bool runOnModule(Module &M);

        struct Value3 {

            Value3() {}

            Value3(Value *x, Value *y, Value *z) : _x(x), _y(y), _z(z) {}

            ~Value3() {}

            Value *_x;
            Value *_y;
            Value *_z;

        };

    private:
        Function *getFooFunction(Function *kernel, bool vectorized, bool barrier);
        Function *createWrapper(Function *kernel, TerminatorInst **term, SmallVector<Value *, 8> &wrapperArgs);
        SmallVector<Value *,8> createKernelArgs(Function *wrapper, Function *kernel,
                                                Value3 &blockId, Value3 &maxBlock, Value3 &maxId);
        void replaceDummyWithKernelIfNeeded(Function *wrapper, Function *kernel, SmallVector<Value *, 8> &kernelArgs,
                                            bool vectorized, bool barrier);
        void removeDeadCalls(Function *wrapper, Value3 &blockId, Value3 &maxBlock, Value3 &maxId);

        void markWrapperAsKernel(Module &M, Function *wrapper);
    };

    bool PACXXNativeLinker::runOnModule(Module &M) {

      auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

      for (auto &F : kernels) {

        bool vectorized = F->hasFnAttribute("vectorized") ? true : false;
        bool barrier = F->hasFnAttribute("barrier") ? true : false;

        SmallVector<Value *, 8> wrapperArgs;
        Value3 blockId;
        Value3 maxBlock;
        Value3 maxId;

        TerminatorInst *term;

        Function *foo = getFooFunction(F, vectorized, barrier);

        Function *wrapper = createWrapper(F, &term, wrapperArgs);

        SmallVector<Value *, 8> kernelArgs = createKernelArgs(wrapper, F, blockId, maxBlock, maxId);

        SmallVector<Value *, 8> fooArgs;
        fooArgs.insert(fooArgs.end(), wrapperArgs.begin(), wrapperArgs.end());
        fooArgs.insert(fooArgs.end(), kernelArgs.begin(), kernelArgs.end());

        // now that we have a call inst inline it!
        auto CI = CallInst::Create(foo, fooArgs, "", term);
        InlineFunctionInfo IFI;
        InlineFunction(CI, IFI, nullptr, false);

        replaceDummyWithKernelIfNeeded(wrapper, F, kernelArgs, vectorized, barrier);

        removeDeadCalls(wrapper, blockId, maxBlock, maxId);

        __verbose("Cleaning up");
        if(vectorized) {
          Function *vec_F = M.getFunction("__vectorized__" + F->getName().str());
          vec_F->eraseFromParent();
          // if the kernel has been vectorized and has a barrier, we neeed to remove the vectorized wrapper because we
          // will use the barrier version of the wrapper
          if(barrier) {
            Function *vecFoo = M.getFunction("__vectorized__foo__" + F->getName().str());
            vecFoo->eraseFromParent();
          }
        }

        F->eraseFromParent();
        foo->eraseFromParent();

        // finally mark the created wrapper as a kernel
        markWrapperAsKernel(M, wrapper);
      }

      Function *origFoo = M.getFunction("foo");
      if(origFoo)
        origFoo->eraseFromParent();

      return true;
    }

    Function* PACXXNativeLinker::getFooFunction(Function *kernel, bool vectorized, bool barrier) {

      __verbose("Getting correct foo function");

      Module *M = kernel->getParent();
      LLVMContext &ctx = M->getContext();
      Function *origFoo = M->getFunction("foo");
      Function *foo = nullptr;


      // if the kernel has been vectorized
      if (vectorized && !barrier) {
        foo = M->getFunction("__vectorized__foo__" + kernel->getName().str());
      }
      // if the kernel has a barrier use a special foo function
      if (barrier) {
        foo = M->getFunction("__barrier__foo__" + kernel->getName().str());
      }
      //if the kernel has not been vectorized and contains no barriers
      if(!vectorized && !barrier) {
        SmallVector<Type *, 8> Params;
        for (auto &arg : origFoo->args()) {
          Params.push_back(arg.getType());
        }
        for (auto &arg : kernel->args()) {
          Params.push_back(arg.getType());
        }

        FunctionType *FTy = FunctionType::get(Type::getVoidTy(ctx), Params, false);
        foo = Function::Create(FTy, origFoo->getLinkage(), "clonedFoo", M);
        auto DestI = foo->arg_begin();
        ValueToValueMapTy VMap;
        for (auto I = origFoo->arg_begin(); I != origFoo->arg_end(); ++I) {
          DestI->setName(I->getName());
          VMap[cast<Value>(I)] = cast<Value>(DestI++);
        }
        SmallVector<ReturnInst *, 8> returns;
        CloneFunctionInto(foo, origFoo, VMap, true, returns);
      }
      return foo;
    }

    Function* PACXXNativeLinker::createWrapper(Function *kernel,
                                               TerminatorInst **term,
                                               SmallVector<Value *, 8> &wrapperArgs) {
      Module *M = kernel->getParent();
      LLVMContext &ctx = M->getContext();

      SmallVector<Type *, 8> Params;

      //block id
      Params.push_back(IntegerType::getInt32Ty(ctx));
      Params.push_back(IntegerType::getInt32Ty(ctx));
      Params.push_back(IntegerType::getInt32Ty(ctx));

      // max blocks
      Params.push_back(IntegerType::getInt32Ty(ctx));
      Params.push_back(IntegerType::getInt32Ty(ctx));
      Params.push_back(IntegerType::getInt32Ty(ctx));

      // max threads
      Params.push_back(IntegerType::getInt32Ty(ctx));
      Params.push_back(IntegerType::getInt32Ty(ctx));
      Params.push_back(IntegerType::getInt32Ty(ctx));

      //shared memory
      Params.push_back(IntegerType::getInt32Ty(ctx));

      //arguments
      Params.push_back(PointerType::getInt8PtrTy(ctx));

      // we can always use void, because we work with valid cuda kernels and they need to return void
      FunctionType *FTy = FunctionType::get(Type::getVoidTy(ctx), Params, false);
      auto wrappedF = Function::Create(FTy, GlobalValue::LinkageTypes::ExternalLinkage,
                                 std::string("__wrapped__") + kernel->getName().str(), M);

      auto wrappedArgs = wrappedF->arg_begin();
      (wrappedArgs++)->setName("bidx");
      (wrappedArgs++)->setName("bidy");
      (wrappedArgs++)->setName("bidz");

      (wrappedArgs++)->setName("maxblockx");
      (wrappedArgs++)->setName("maxblocky");
      (wrappedArgs++)->setName("maxblockz");

      wrapperArgs.push_back(&*wrappedArgs);
      (wrappedArgs++)->setName("maxidx");
      wrapperArgs.push_back(&*wrappedArgs);
      (wrappedArgs++)->setName("maxidy");
      wrapperArgs.push_back(&*wrappedArgs);
      (wrappedArgs++)->setName("maxidz");

      (wrappedArgs++)->setName("sm_size");

      (wrappedArgs++)->setName("args");

      BasicBlock *entry = BasicBlock::Create(ctx, "entry", wrappedF);

      *term = ReturnInst::Create(ctx, entry);



      return wrappedF;
    }

    SmallVector<Value *,8> PACXXNativeLinker::createKernelArgs(Function *wrapper, Function *kernel,
                                                               Value3 &blockId, Value3 &maxBlock, Value3 &maxId) {

      SmallVector<Value *,8> kernelArgs;

      Module *M = wrapper->getParent();
      LLVMContext &ctx = M->getContext();

      Value *bidx = nullptr, *bidy = nullptr, *bidz = nullptr;
      Value *maxidx = nullptr, *maxidy = nullptr, *maxidz = nullptr;
      Value *maxblockx = nullptr, *maxblocky = nullptr, *maxblockz = nullptr;

      BasicBlock *entry = &wrapper->front();

      // create the arguments from the char*
      auto argIt = wrapper->arg_begin();

      //blockids
      bidx = &*argIt;
      ++argIt;
      bidy = &*argIt;
      ++argIt;
      bidz = &*argIt;
      ++argIt;
      // max blocks
      maxblockx = &*argIt;
      ++argIt;
      maxblocky = &*argIt;
      ++argIt;
      maxblockz = &*argIt;
      ++argIt;
      //max threads
      maxidx = &*argIt;
      ++argIt;
      maxidy = &*argIt;
      ++argIt;
      maxidz = &*argIt;
      ++argIt;
      //shared memory
      ++argIt;

      blockId = Value3(bidx, bidy, bidz);
      maxBlock = Value3(maxblockx, maxblocky, maxblockz);
      maxId = Value3(maxidx, maxidy, maxidz);

      // construct the kernel arguments from the char*
      BasicBlock *constructKernelArgs = BasicBlock::Create(ctx, "constructArgs", wrapper, entry);

      auto int8ptr_type = Type::getInt8PtrTy(ctx);
      auto int8ptr_align = M->getDataLayout().getPrefTypeAlignment(int8ptr_type);

      auto *alloc_args = new AllocaInst(int8ptr_type, nullptr, int8ptr_align, (&*argIt)->getName(),
                                          constructKernelArgs);
      new StoreInst(&*argIt, alloc_args, false, int8ptr_align, constructKernelArgs);
      auto *args_load = new LoadInst(alloc_args, "args", false, int8ptr_align, constructKernelArgs);

      size_t offset = 0;

      for(auto I = kernel->arg_begin(), E = kernel->arg_end(); I != E; ++I) {

        auto &A = *I;

        auto arg_size = M->getDataLayout().getTypeAllocSize(A.getType());
        auto arg_alignment = M->getDataLayout().getPrefTypeAlignment(A.getType());

        auto arg_offset = (offset + arg_alignment - 1) & ~(arg_alignment - 1);
        // consider offset in char array
        auto *elem_ptr = GetElementPtrInst::CreateInBounds(Type::getInt8Ty(ctx), args_load,
                                                           ConstantInt::get(Type::getInt64Ty(ctx), arg_offset),
                                                           "", constructKernelArgs);
        // Cast to apropriate size
        auto *casted = new BitCastInst(elem_ptr, PointerType::getUnqual(A.getType()), "", constructKernelArgs);
        auto *casted_load = new LoadInst(casted, A.getName(), false,
                                         M->getDataLayout().getPrefTypeAlignment(casted->getType()),
                                         constructKernelArgs);

        kernelArgs.push_back(casted_load);

        offset = arg_offset + arg_size;
      }

      // branch to normal entry block
      llvm::BranchInst::Create(entry, constructKernelArgs);

      return kernelArgs;
    }

    void PACXXNativeLinker::replaceDummyWithKernelIfNeeded(Function *wrapper, Function *kernel,
                                                           SmallVector<Value *, 8> &kernelArgs,
                                                           bool vectorized, bool barrier) {

      Module *M = wrapper->getParent();
      CallInst *CI = nullptr;

      if(!barrier && !vectorized) {
        Function *dummy = M->getFunction("__dummy_kernel");

        CallInst *remove = nullptr;
        for (auto U : dummy->users()) {
          if ((CI = dyn_cast<CallInst>(U))) {
            if (CI->getParent()->getParent() == wrapper) {
              remove = CI;
              CI = CallInst::Create(kernel, kernelArgs, "", CI);
              break;
            }
          }
        }

        if (remove)
          remove->eraseFromParent();

        // time to inline the original kernel into the wrapper
        __verbose("Inline kernel \n");
        InlineFunctionInfo IFI;
        InlineFunction(CI, IFI, nullptr, false);
      }
    }

    void PACXXNativeLinker::removeDeadCalls(Function *wrapper, Value3 &blockId, Value3 &maxBlock, Value3 &maxId) {

      AllocaInst *idx = nullptr, *idy = nullptr, *idz = nullptr;

      for (auto &B : *wrapper) {
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

      __verbose("replacing dead calls \n");
        vector<CallInst *> dead_calls;
        for (auto &B : *wrapper) {
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
                  CI->replaceAllUsesWith(blockId._x);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ctaid_y) {
                  CI->replaceAllUsesWith(blockId._y);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ctaid_z) {
                  CI->replaceAllUsesWith(blockId._z);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_nctaid_x) {
                  CI->replaceAllUsesWith(maxBlock._x);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_nctaid_y) {
                  CI->replaceAllUsesWith(maxBlock._y);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_nctaid_z) {
                  CI->replaceAllUsesWith(maxBlock._z);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ntid_x) {
                  CI->replaceAllUsesWith(maxId._x);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ntid_y) {
                  CI->replaceAllUsesWith(maxId._y);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ntid_z) {
                  CI->replaceAllUsesWith(maxId._z);
                  dead_calls.push_back(CI);
                }
              }
            }
          }
        }

        __verbose("deleting \n");
        for (auto I : dead_calls)
          I->eraseFromParent();
    }

    void PACXXNativeLinker::markWrapperAsKernel(Module &M, Function *wrapper) {
      LLVMContext &ctx = M.getContext();
      NamedMDNode *MD = M.getOrInsertNamedMetadata("nvvm.annotations");
      SmallVector<Metadata *, 3> MDVals;
      MDVals.push_back(ConstantAsMetadata::get(wrapper));
      MDVals.push_back(MDString::get(ctx, "kernel"));
      MDVals.push_back(ConstantAsMetadata::get(ConstantInt::get(Type::getInt32Ty(ctx), 1)));

      MD->addOperand(MDNode::get(ctx, MDVals));
    }

    char PACXXNativeLinker::ID = 0;
    static RegisterPass<PACXXNativeLinker>
            X("pacxx_native", "Inlines functions into kernels", false, false);

    Pass *createPACXXNativeLinkerPass() { return new PACXXNativeLinker(); }
}
