/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2013-2014
*/

#include "../lib/IR/LLVMContextImpl.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/PACXXTransforms.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include <cassert>
#include <iostream>
#include <set>
#include <sstream>
#include <vector>

#include "llvm/IR/Dominators.h"
#include <string>

#include "CallVisitor.h"
#include "ModuleHelper.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {
class IntrinsicScheduler : public InstVisitor<IntrinsicScheduler> {
public:
  IntrinsicScheduler() {}

  void initialize() { intrinsicClones.clear(); }

  void finalize() {
    for (auto I : intrinsicClones) {
      auto *clone = I.second->clone();
      clone->insertBefore(I.first);
      for (unsigned i = 0; i != I.first->getNumOperands(); ++i) {
        auto op = I.first->getOperand(i);
        if (op == I.second) {
          I.first->setOperand(i, clone);
        }
      }
    }
  }

  void visitCallInst(CallInst &CI) {
    if (auto II = dyn_cast<IntrinsicInst>(&CI)) {
      if (isPACXXIntrinsic(II->getIntrinsicID())) {
        for (auto u : CI.users()) {
          if (Instruction *I = dyn_cast<Instruction>(u)) {
            // clone them if they are not in the same basic block
            if (!isa<PHINode>(I) && I->getParent() != CI.getParent()) {
              intrinsicClones.push_back(make_pair(I, &CI));
            }
          }
        }
      }
    }
  }

  static bool isPACXXIntrinsic(Intrinsic::ID id){
    switch(id)
    {
    case Intrinsic::pacxx_barrier0:
    case Intrinsic::pacxx_read_ntid_x:
    case Intrinsic::pacxx_read_ntid_y:
    case Intrinsic::pacxx_read_ntid_z:
    case Intrinsic::pacxx_read_ntid_w:
    case Intrinsic::pacxx_read_tid_x:
    case Intrinsic::pacxx_read_tid_y:
    case Intrinsic::pacxx_read_tid_z:
    case Intrinsic::pacxx_read_tid_w:
    case Intrinsic::pacxx_read_ctaid_x:
    case Intrinsic::pacxx_read_ctaid_y:
    case Intrinsic::pacxx_read_ctaid_z:
    case Intrinsic::pacxx_read_ctaid_w:
    case Intrinsic::pacxx_read_nctaid_x:
    case Intrinsic::pacxx_read_nctaid_y:
    case Intrinsic::pacxx_read_nctaid_z:
    case Intrinsic::pacxx_read_nctaid_w:
      return true;
    default:
      break;
    }
    return false;
  }

private:
  vector<pair<Instruction *, CallInst *>> intrinsicClones;
  vector<CallInst *> dead;
  vector<pair<CallInst *, BinaryOperator *>> repl;
};


struct PACXXIntrinsicSchedulerPass : public ModulePass {
  static char ID;
  PACXXIntrinsicSchedulerPass() : ModulePass(ID) {}
  virtual ~PACXXIntrinsicSchedulerPass() {}

  virtual bool runOnModule(Module &M) {
    IntrinsicScheduler scheduler;

    for (auto &F : M.getFunctionList()) {
      scheduler.initialize();
      scheduler.visit(F);
      scheduler.finalize();
    }

    return true;
  }
};

char PACXXIntrinsicSchedulerPass::ID = 0;
static RegisterPass<PACXXIntrinsicSchedulerPass> X("pacxx_intrinsic_scheduler", "PACXX intrinsic scheduling pass", false, false);
}

namespace llvm {
Pass *createPACXXIntrinsicSchedulerPass() { return new PACXXIntrinsicSchedulerPass(); }
}
