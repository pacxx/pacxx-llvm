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
  for (auto &F : M.getFunctionList()) {
    F.setCallingConv(CallingConv::C);

    F.setAttributes({});
    F.addFnAttr(Attribute::AlwaysInline);
  }

  return modified;
}

void PACXXReflectionCleaner::cleanFromKerneles(Module &M) {
  auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

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
