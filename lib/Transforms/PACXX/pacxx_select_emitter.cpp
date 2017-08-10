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
        if (F->getIntrinsicID() == Intrinsic::masked_load || F->getIntrinsicID() == Intrinsic::masked_store) {
      //    if (!TTI->isLegalMaskedLoad(CI.getArgOperand(0)->getType())
      //        || !TTI->isLegalMaskedStore(CI.getArgOperand(0)->getType()))
            dead.push_back(&CI);
        }
      }
    }

    void transform() {
      for (auto CI : dead) {

        auto F = CI->getCalledFunction();

        if (F && F->isIntrinsic()) {
          if (F->getIntrinsicID() == Intrinsic::masked_load) {
            // declare <N x T> @llvm.masked.load(<N x T>* <ptr>, i32 <alignment>, <N x i1> <mask>, <N x T> <passthru>)
            ConstantInt *constint = cast<ConstantInt>(CI->getArgOperand(1));
            unsigned int alignment = constint->getZExtValue();

            auto mask = CI->getArgOperand(2);

            IRBuilder<> builder(CI);
            auto Ty = cast<VectorType>(mask->getType());
            auto cast = builder.CreateBitCast(mask, Type::getIntNTy(builder.getContext(), Ty->getVectorNumElements()));
            auto cmp = builder.CreateICmpNE(cast, ConstantInt::get(cast->getType(), 0));
            auto BB = CI->getParent();
            auto GuardBB = BB->splitBasicBlock(CI);
            auto I = BB->getTerminator();
            IRBuilder<> bb_builder(I);

            auto unmasked_load = builder.CreateLoad(CI->getArgOperand(0));
            unmasked_load->setAlignment(alignment);
            auto select = builder.CreateSelect(mask, unmasked_load, CI->getArgOperand(3));
            auto Successor = GuardBB->splitBasicBlock(CI);

            bb_builder.CreateCondBr(cmp, GuardBB, Successor);
            I->eraseFromParent();

            IRBuilder<> succ_builder(CI);
            auto phi = succ_builder.CreatePHI(select->getType(), 2);
            phi->addIncoming(select, GuardBB);
            phi->addIncoming(UndefValue::get(select->getType()), BB);
            CI->replaceAllUsesWith(phi);
          }

          if (F->getIntrinsicID() == Intrinsic::masked_store) {
            // declare void @llvm.masked.store (<N x T> <value>, <N x T>* <ptr>, i32 <alignment>, <N x i1> <mask>)

            ConstantInt *constint = cast<ConstantInt>(CI->getArgOperand(2));
            unsigned int alignment = constint->getZExtValue();

            auto mask = CI->getArgOperand(3);

            IRBuilder<> builder(CI);
            auto Ty = cast<VectorType>(mask->getType());
            auto cast = builder.CreateBitCast(mask, Type::getIntNTy(builder.getContext(), Ty->getVectorNumElements()));
            auto cmp = builder.CreateICmpNE(cast, ConstantInt::get(cast->getType(), 0));

            auto BB = CI->getParent();
            auto GuardBB = BB->splitBasicBlock(CI);
            auto I = BB->getTerminator();
            IRBuilder<> bb_builder(I);

            auto load = builder.CreateLoad(CI->getArgOperand(1));
            load->setAlignment(alignment);
            auto select = builder.CreateSelect(CI->getArgOperand(3),
                                               CI->getArgOperand(0),
                                               load);//UndefValue::get(CI.getArgOperand(0)->getType()));
            auto store = builder.CreateStore(select, CI->getArgOperand(1));
            store->setAlignment(alignment);
            auto Successor = GuardBB->splitBasicBlock(CI);
            bb_builder.CreateCondBr(cmp, GuardBB, Successor);
            I->eraseFromParent();
          }
        }
      }

    }

    void finalize() {
      for (auto I : dead)
        I->eraseFromParent();
      dead.clear();
    }

    std::vector<CallInst *> dead;
    TargetTransformInfo *TTI;
  } visitor;

  auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

  for (auto &F : M) {
    visitor.TTI = &getAnalysis<TargetTransformInfoWrapperPass>().getTTI(F);
    visitor.visit(F);
  }

  visitor.transform();
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
