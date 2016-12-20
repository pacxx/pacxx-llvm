#include "ModuleHelper.h"
#include "CallVisitor.h"

#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {
struct PACXXNativeLinker : public ModulePass {
  static char ID;
  PACXXNativeLinker() : ModulePass(ID) {}
  virtual ~PACXXNativeLinker() {}

  virtual bool runOnModule(Module &M) {
    auto &ctx = M.getContext();
    auto foo = M.getFunction("foo");
    auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");
    for (auto &F : kernels) {
      SmallVector<Type *, 8> Params;
      Params.push_back(IntegerType::getInt32Ty(ctx));
      Params.push_back(IntegerType::getInt32Ty(ctx));
      Params.push_back(IntegerType::getInt32Ty(ctx));
      Params.push_back(IntegerType::getInt32Ty(ctx));
      Params.push_back(IntegerType::getInt32Ty(ctx));
      Params.push_back(IntegerType::getInt32Ty(ctx));

      for (auto &A : F->getArgumentList())
        Params.push_back(A.getType());

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
      for (auto &A : F->args())
        (argIt++)->setName(A.getName());

      BasicBlock *entry = BasicBlock::Create(ctx, "entry", wrappedF);
      auto term = ReturnInst::Create(ctx, entry);
      auto CI = CallInst::Create(foo, args, "", term);

      // now that we have a call inst inline it!

      InlineFunctionInfo IFI;
      InlineFunction(CI, IFI);

      // find the dummy kernel call and replace it with a call to the actual
      // kernel

      Value *bidx = nullptr, *bidy = nullptr, *bidz = nullptr;
      Value *maxidx = nullptr, *maxidy = nullptr, *maxidz = nullptr;
      auto dummy = M.getFunction("__dummy_kernel");
      CallInst *remove = nullptr;
      for (auto U : dummy->users()) {
        if ((CI = dyn_cast<CallInst>(U))) {
          if (CI->getParent()->getParent() == wrappedF) {
            SmallVector<Value *, 8> kernel_args;
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

            for (; argIt != wrappedF->getArgumentList().end(); ++argIt)
              kernel_args.push_back(&*argIt);

            remove = CI;
            CI = CallInst::Create(F, kernel_args, "", CI);
            break;
          }
        }
      }

      if (remove)
        remove->eraseFromParent();

      // time to inline the original kernel into the wrapper
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

      wrappedF->dump();
    }
    foo->eraseFromParent();    
    //cleanup 
      for (auto F : kernels)
        F->eraseFromParent(); 
    return true;
  }
};

char PACXXNativeLinker::ID = 0;
static RegisterPass<PACXXNativeLinker>
    X("pacxx_native", "Inlines functions into kernels", false, false);
}

namespace llvm {
Pass *createPACXXNativeLinker() { return new PACXXNativeLinker(); }
}
