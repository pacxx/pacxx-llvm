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
      CallInst *clone = nullptr;

      SmallVector<Value *, 1> args;
      args.push_back(I.second->getOperand(0));
      clone = CallInst::Create(I.second->getCalledFunction(), args, "", I.first);
      clone->setTailCall(I.second->isTailCall());

      for (unsigned i = 0; i != I.first->getNumOperands(); ++i) {
        auto op = I.first->getOperand(i);
        if (op == I.second) {
          I.first->setOperand(i, clone);
        }
      }
    }
  }

  void visitCallInst(CallInst &CI) {
    if (CI.isInlineAsm())
      return;
    if (!isa<Function>(CI.getCalledValue())) {
      return;
    }
    if (isaSregIntrinsic(CI.getCalledFunction())) {
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

  bool isaSregIntrinsic(Function *F) {
    if (!F)
      return false;
    bool found = false;
    auto name = F->getName();
    if (name.find("get_local_id") != StringRef::npos)
      found = true;
    if (name.find("get_global_id") != StringRef::npos)
      found = true;
    if (name.find("get_local_size") != StringRef::npos)
      found = true;
    if (name.find("get_group_id") != StringRef::npos)
      found = true;
    if (name.find("get_group_size") != StringRef::npos)
      found = true;

    if (found) {
      F->addFnAttr(Attribute::NoUnwind);
      F->addFnAttr(Attribute::ReadNone);
      return true;
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
