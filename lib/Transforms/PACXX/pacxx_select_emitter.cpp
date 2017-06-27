/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2014
*/

#include <algorithm>
#include <vector>
#include <llvm/Analysis/TargetTransformInfo.h>

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
      auto alignment = 4; // M.getDataLayout().getPrefTypeAlignment(CI.getType());

      if (F && F->isIntrinsic()) {
        if (F->getIntrinsicID() == Intrinsic::masked_gather) {
          if (!TTI->isLegalMaskedGather(CI.getArgOperand(0)->getType())) {

            llvm::errs() << "replace gather\n";
            auto extr = ExtractElementInst::Create(CI.getArgOperand(0),
                                                   ConstantInt::get(Type::getInt32Ty(CI.getContext()), 0),
                                                   "extr_ptr",
                                                   &CI);
            auto cast = new BitCastInst(extr, CI.getType()->getPointerTo(0), "cast_ptr", &CI);
            auto load = new LoadInst(CI.getType(), cast, "unsave_load", false, alignment, &CI);
            auto select = SelectInst::Create(CI.getArgOperand(2), load, CI.getArgOperand(3), "selected_load", &CI);

            CI.replaceAllUsesWith(select);
            dead.push_back(&CI);
          }
        }

        if (F->getIntrinsicID() == Intrinsic::masked_scatter) {
          if (!TTI->isLegalMaskedScatter(CI.getArgOperand(0)->getType())) {
            llvm::errs() << "replace scatter\n";

            auto extr = ExtractElementInst::Create(CI.getArgOperand(1),
                                                   ConstantInt::get(Type::getInt32Ty(CI.getContext()), 0),
                                                   "extr_ptr",
                                                   &CI);
            auto cast = new BitCastInst(extr, CI.getArgOperand(0)->getType()->getPointerTo(0), "cast_ptr", &CI);
            auto undef = UndefValue::get(CI.getArgOperand(0)->getType());
            auto select = SelectInst::Create(CI.getArgOperand(3), CI.getArgOperand(0), undef, "selected_store", &CI);
            auto store = new StoreInst(select, cast, false, alignment, &CI);
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
