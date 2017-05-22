/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2014
*/

#include <vector>
#include <cassert>
#include <algorithm>

#define DEBUG_TYPE "pacxx_classify"

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
#include "llvm/Transforms/PACXXTransforms.h"

#include "ModuleHelper.h"


using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {

struct PACXXReflectionRemover : public ModulePass {
  static char ID;
  PACXXReflectionRemover() : ModulePass(ID) {}
  virtual ~PACXXReflectionRemover() {}

  virtual bool runOnModule(Module &M) {
    bool modified = true;

    std::map<Function *, SmallVector<Metadata *, 3>> reflects;
    Function *PRF = M.getFunction("__pacxx_reflect");
    if (PRF) {
      for (auto U : PRF->users()) {
        SmallVector<Metadata *, 3> MDArgs;
        if (CallInst *I = dyn_cast<CallInst>(U)) {
          Function *K = I->getParent()->getParent();
          if (reflects[K].size() == 0) {
            reflects[K].push_back(
                MDString::get(M.getContext(), "reflected calls"));
          }
          reflects[K].push_back(llvm::ValueAsMetadata::get(I->getOperand(0)));
        }
      }

      for (auto I : reflects) {
        if (NamedMDNode *MD = M.getNamedMetadata("pacxx.kernel." +
                                                 I.first->getName().str())) {
          MD->addOperand(MDNode::get(M.getContext(), I.second));
        }
      }
    }

    cleanFromReflections(M);
    return modified;
  }

private:

   void cleanFromReflections(Module &M) {
    auto reflections = pacxx::getTagedFunctions(&M, "pacxx.reflection", "");
  
    for (auto F : reflections) {
      F->replaceAllUsesWith(UndefValue::get(F->getType()));
      F->eraseFromParent();
    }

    vector<GlobalValue *> globals;
    for (auto &G : M.getGlobalList()) {
        if (GlobalVariable* GV = dyn_cast<GlobalVariable>(&G)){
            if (GV->getVisibility() == GlobalValue::ProtectedVisibility){
                G.removeDeadConstantUsers();
                G.replaceAllUsesWith(UndefValue::get(G.getType()));
                globals.push_back(&G);

            }
        }
    }

    for (auto G : globals)
        G->eraseFromParent();

  }

};

char PACXXReflectionRemover::ID = 0;
static RegisterPass<PACXXReflectionRemover> X("pacxx_classify",
                                            "PACXXAccessClassifer: path "
                                            "classify memory access patterns "
                                            "on kernel parameters",
                                            false, false);
}

namespace llvm {
Pass *createPACXXReflectionRemoverPass() { return new PACXXReflectionRemover(); }
}
