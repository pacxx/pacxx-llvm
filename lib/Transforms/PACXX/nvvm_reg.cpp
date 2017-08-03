/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2014
*/

//#include <iostream>
#include <vector>
#include <cassert>
#include <algorithm>

#define DEBUG_TYPE "nvvm_reg"

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/CFG.h"

#include "ModuleHelper.h"

#define GLOBAL_ID_PATTERN "mad.lo.u32 $0, $1, $2, $3;"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {

struct NVVMRegPass : public ModulePass {
  static char ID;
  NVVMRegPass(bool runtime = false) : ModulePass(ID), runtime(runtime) {}
  virtual ~NVVMRegPass() {}

  virtual bool runOnModule(Module &M) {
    bool modified = true;

    auto kernels = getTagedFunctions(&M, "nvvm.annotations", "kernel");


    BaseOpts opt(&M);
    for (auto &F : M.getFunctionList()) {
      opt.initialize(F);
      opt.visit(F);
      opt.finalize();
    }

    return modified;
  }

private:

  class BaseOpts : public InstVisitor<BaseOpts> {
  public:
    BaseOpts(Module *module) : M(module) {}

    void visitCallInst(CallInst &CI) {

      if (!CI.getCalledFunction())
        return; // discard inline ASM

      if (!CI.getCalledFunction()->isIntrinsic()) {
        Function *reflect = M->getFunction("__nvvm_reflect");
        if (CI.getCalledFunction() == reflect) {
          CI.replaceAllUsesWith(ConstantInt::get(CI.getType(), 0));
          dead.push_back(&CI);
        }
      }
    }

    void visitExtractElementInst(ExtractElementInst &I) {
      for (auto u : I.users()) {
        if (auto uI = dyn_cast<Instruction>(u)) {
          if (uI->getParent() == I.getParent()) {
            return;
          }
        }
      }
      extracts.push_back(&I);
    }

    void initialize(Function &F) {
      dead.clear();
      extracts.clear();
      if (F.isDeclaration())
        return;
    }

    void finalize() {
      for (auto I : extracts) {
        vector<pair<BasicBlock *, ExtractElementInst *>> cloned_into;
        for (auto u : I->users()) {

          if (auto uI = dyn_cast<Instruction>(u)) {
            auto found = std::find_if(
                cloned_into.begin(), cloned_into.end(),
                [&](auto p) { return p.first == uI->getParent(); });
            ExtractElementInst *clone = nullptr;
            if (found == cloned_into.end()) {
              clone = cast<ExtractElementInst>(I->clone());
              clone->insertBefore(uI->getParent()->getFirstNonPHI());
              cloned_into.push_back(make_pair(uI->getParent(), clone));
            }
            if (clone == nullptr)
              clone = found->second;
            for (unsigned i = 0; i < uI->getNumOperands(); ++i) {
              if (uI->getOperand(i) == I) {
                uI->setOperand(i, clone);
              }
            }
          }
        }
        if (I->hasNUses(0))
          I->eraseFromParent();
      }

      for (auto &d : dead) {
        d->replaceAllUsesWith(UndefValue::get(d->getType()));
        d->eraseFromParent();
      }
    }

  private:
    Module *M;
    vector<ExtractElementInst *> extracts;
    vector<Instruction *> dead;
  };

  bool runtime;
};

char NVVMRegPass::ID = 0;
static RegisterPass<NVVMRegPass>
    X("nvvm_reg", "PACXX: path to reduce register preasure", false, false);
}

namespace llvm {
Pass *createPACXXNvvmRegPass(bool runtime) { return new NVVMRegPass(runtime); }
}
