/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2014
*/

#include <algorithm>
#include <vector>

#define DEBUG_TYPE "pacxx_emit_select"

#include "llvm/IR/Argument.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Transforms/PACXXTransforms.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Analysis/TargetTransformInfo.h"

#include "ModuleHelper.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {

struct PACXXSelectEmitter : public ModulePass {
  static char ID;
  PACXXSelectEmitter() : ModulePass(ID) { initializePACXXSelectEmitterPass(*PassRegistry::getPassRegistry()); }
  virtual ~PACXXSelectEmitter() {}
  virtual bool runOnModule(Module &M) override;
  virtual void getAnalysisUsage(AnalysisUsage &AU) const override;
};

bool PACXXSelectEmitter::runOnModule(Module &M) {
  bool modified = true;

  struct IntrinsicVisitor : public InstVisitor<IntrinsicVisitor> {

    void visitCallInst(CallInst &CI) {

      auto F = CI.getCalledFunction();

      if (F && F->isIntrinsic()) {
        if (F->getIntrinsicID() == Intrinsic::masked_load) {
          if (!TTI->isLegalMaskedLoad(CI.getArgOperand(0)->getType())) {
            // declare <N x T> @llvm.masked.load(<N x T>* <ptr>, i32 <alignment>, <N x i1> <mask>, <N x T> <passthru>)
            ConstantInt *constint = cast<ConstantInt>(CI.getArgOperand(1));
            unsigned int alignment = constint->getZExtValue();
            auto load = new LoadInst(CI.getType(), CI.getArgOperand(0), "unmasked_load", false, alignment, &CI);
            auto select = SelectInst::Create(CI.getArgOperand(2), load, CI.getArgOperand(3), "selected_load", &CI);
            CI.replaceAllUsesWith(select);
            dead.push_back(&CI);
          }
        }

        if (F->getIntrinsicID() == Intrinsic::masked_store) {
          if (!TTI->isLegalMaskedStore(CI.getArgOperand(0)->getType())) {
            // declare void @llvm.masked.store (<N x T> <value>, <N x T>* <ptr>, i32 <alignment>, <N x i1> <mask>)
            ConstantInt *constint = cast<ConstantInt>(CI.getArgOperand(2));
            unsigned int alignment = constint->getZExtValue();
            auto select = SelectInst::Create(CI.getArgOperand(3),
                                             CI.getArgOperand(0),
                                             UndefValue::get(CI.getArgOperand(0)->getType()),
                                             "selected_store",
                                             &CI);
            new StoreInst(select, CI.getArgOperand(1), false, alignment, &CI);
            dead.push_back(&CI);
          }
        }
      }

    }

    void finalize() {
      for (auto I : dead)
        I->eraseFromParent();
    }

    std::vector<Instruction *> dead;
    TargetTransformInfo *TTI;
  } visitor;

  auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

  for (auto &F : M) {
    visitor.TTI = &getAnalysis<TargetTransformInfoWrapperPass>().getTTI(F);
    visitor.visit(F);
  }

  visitor.finalize();
  return modified;
}

void PACXXSelectEmitter::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<TargetLibraryInfoWrapperPass>();
  AU.addRequired<TargetTransformInfoWrapperPass>();
}

}

char PACXXSelectEmitter::ID = 0;

INITIALIZE_PASS_BEGIN(PACXXSelectEmitter, "pacxx_emit_select",
                      "PACXXSelectEmitter: transform masked intrinsics to selects", true, true)
  INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
  INITIALIZE_PASS_DEPENDENCY(TargetTransformInfoWrapperPass)
INITIALIZE_PASS_END(PACXXSelectEmitter, "pacxx_emit_select",
                    "PACXXSelectEmitter: transform masked intrinsics to selects", true, true)

namespace llvm {
Pass *createPACXXSelectEmitterPass() {
  return new PACXXSelectEmitter();
}
}
