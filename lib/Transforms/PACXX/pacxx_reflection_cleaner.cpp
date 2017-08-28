/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2014
*/

#include <algorithm>
#include <vector>

#define DEBUG_TYPE "pacxx_reflection"

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

namespace pacxx {

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

struct PACXXReflectionCleaner : public ModulePass {
  static char ID;
  PACXXReflectionCleaner() : ModulePass(ID) {}
  virtual ~PACXXReflectionCleaner() {}
  virtual bool runOnModule(Module &M);

private:
  void cleanFromKerneles(Module &M);
};

bool PACXXReflectionCleaner::runOnModule(Module &M) {
  bool modified = true;

  M.setTargetTriple(sys::getProcessTriple());
  string Error;
  auto HostTarget = TargetRegistry::lookupTarget(M.getTargetTriple(), Error);
  if (!HostTarget) {
    assert(false);
  }

  TargetOptions Options;
  auto RM = Optional<Reloc::Model>();
  auto HostMachine =
      HostTarget->createTargetMachine(M.getTargetTriple(), "", "", Options, RM);
  M.setDataLayout(HostMachine->createDataLayout().getStringRepresentation());

  cleanFromKerneles(M);

  auto reflects = pacxx::getTagedFunctions(&M, "pacxx.reflection", "");
  for (auto& F : M) {
    F.setCallingConv(CallingConv::C);
    F.setAttributes({});
    F.addFnAttr(Attribute::AlwaysInline);
    if (!F.isIntrinsic() && !F.isDeclaration())
    if (std::find(reflects.begin(), reflects.end(), &F) == reflects.end())
      F.setLinkage(llvm::GlobalValue::LinkageTypes::InternalLinkage);
  }

  struct IntrinsicVisitor : public InstVisitor<IntrinsicVisitor> {

    void visitCallInst(CallInst &CI) {

      if (auto II = dyn_cast<IntrinsicInst>(&CI)) {
        if (isPACXXIntrinsic(II->getIntrinsicID())) {
          dead.push_back(II);
        }
      }
    }

    void finalize(){
      std::for_each(dead.begin(), dead.end(), [](auto I){
        I->replaceAllUsesWith(UndefValue::get(I->getType()));
        I->eraseFromParent();
      });
      dead.clear();
    }


  private:
    SmallVector<IntrinsicInst*, 8> dead;

  } visitor;

  std::for_each(reflects.begin(), reflects.end(), [&](auto F){
    visitor.visit(F);
    visitor.finalize();
  });

  return modified;
}

void PACXXReflectionCleaner::cleanFromKerneles(Module &M) {
  auto kernels = pacxx::getKernels(&M);

  for (auto F : kernels) {
    F->replaceAllUsesWith(UndefValue::get(F->getType()));
    F->eraseFromParent();
  }
}

char PACXXReflectionCleaner::ID = 0;
static RegisterPass<PACXXReflectionCleaner>
    X("pacxx_reflection_cleaner",
      "PACXXReflectionCleaner: "
      "finalizes the reflection module by cleaning up",
      false, false);
}

namespace llvm {
Pass *createPACXXReflectionCleanerPass() {
  return new PACXXReflectionCleaner();
}
}
