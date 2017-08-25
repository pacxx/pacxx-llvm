#include "ModuleHelper.h"

#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {
struct PACXXInliner : public ModulePass {
  static char ID;
  PACXXInliner() : ModulePass(ID) {}
  virtual ~PACXXInliner() {}

  virtual bool runOnModule(Module &M) {
    auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");
    for (auto &F : kernels) {
      KernelInliner ki(M, InliningStrategy::Aggressive);
      ki.runOn(*F);
    }
    return true;
  }

private:
  enum class InliningStrategy {
    Soft,      // inline only direct calls from the kernel
    Aggressive // inline all and everything
  };

  class KernelInliner : public InstVisitor<KernelInliner> {
  public:
    KernelInliner(Module& M, InliningStrategy S) : S(S), reflects(pacxx::getTagedFunctions(&M, "pacxx.reflection", "")) {}

    void runOn(Function &F) {
      if (S == InliningStrategy::Aggressive) {
        unsigned inlined = 0;
        do {
          // auto& cur = *I->F;
          visit(F);
          inlined = finalize(F);
        } while (inlined != 0);

      } else if (S == InliningStrategy::Soft) {
        visit(F);
        finalize(F);
      }
    }

    void visitCallInst(CallInst &I) { gatherCallsToInline(I); }

  private:
    void gatherCallsToInline(CallInst &I) {
      auto Caller = I.getParent()->getParent();

      if (!canBeInlined(I))
        return;

      calls[Caller].push_back(&I);
    }

    bool canBeInlined(CallInst &I) {
      if (I.isInlineAsm())
        return false;
      if (!I.getCalledFunction())
        return false;

      auto F = I.getCalledFunction();

      if (F->isDeclaration() || F->isIntrinsic())
        return false;

      if (find(reflects.begin(), reflects.end(), F) != reflects.end())
        return false; // never inline reflections

      //if (F->hasFnAttribute(llvm::Attribute::NoInline)) // we cannot honor noinline becasue -O0 sets noinline everywhere
      //  return false;

      return true;
    }

    unsigned finalize(Function &F) {
      unsigned counter = 0;
      for (auto CI : calls[&F]) {

        InlineFunctionInfo IFI;
        InlineFunction(CI, IFI);
        counter++;
      }
      calls[&F].clear();
      return counter;
    }

  private:
    map<Function *, vector<CallInst *>> calls;
    InliningStrategy S;
    set<Function *> reflects;
  };
};

char PACXXInliner::ID = 0;
static RegisterPass<PACXXInliner>
    X("pacxx_inline", "Inlines functions into kernels", false, false);
}

namespace llvm {
Pass *createPACXXInlinerPass() { return new PACXXInliner(); }
}
