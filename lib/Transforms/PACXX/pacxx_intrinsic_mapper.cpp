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
#include "llvm/IR/Intrinsics.h"
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

static Function* mapPACXXIntrinsic(Module* M, Intrinsic::ID id)
{
  switch(id)
  {
  case Intrinsic::pacxx_barrier0:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_barrier0);
  case Intrinsic::pacxx_read_ntid_x:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_ntid_x);
  case Intrinsic::pacxx_read_ntid_y:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_ntid_y);
  case Intrinsic::pacxx_read_ntid_z:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_ntid_z);
  case Intrinsic::pacxx_read_ntid_w:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_ntid_w);
  case Intrinsic::pacxx_read_tid_x:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_tid_x);
  case Intrinsic::pacxx_read_tid_y:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_tid_y);
  case Intrinsic::pacxx_read_tid_z:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_tid_z);
  case Intrinsic::pacxx_read_tid_w:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_tid_w);
  case Intrinsic::pacxx_read_ctaid_x:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_ctaid_x);
  case Intrinsic::pacxx_read_ctaid_y:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_ctaid_y);
  case Intrinsic::pacxx_read_ctaid_z:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_ctaid_z);
  case Intrinsic::pacxx_read_ctaid_w:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_ctaid_w);
  case Intrinsic::pacxx_read_nctaid_x:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_nctaid_x);
  case Intrinsic::pacxx_read_nctaid_y:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_nctaid_y);
  case Intrinsic::pacxx_read_nctaid_z:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_nctaid_z);
  case Intrinsic::pacxx_read_nctaid_w:
    return Intrinsic::getDeclaration(M, Intrinsic::nvvm_read_ptx_sreg_nctaid_w);
  default:
    break;
  }
  return nullptr;
}

struct PACXXIntrinsicMapper : public ModulePass {
  static char ID;
  PACXXIntrinsicMapper() : ModulePass(ID) { initializePACXXIntrinsicMapperPass(*PassRegistry::getPassRegistry()); }
  virtual ~PACXXIntrinsicMapper() {}
  virtual bool runOnModule(Module &M) override;
  virtual void getAnalysisUsage(AnalysisUsage &AU) const override;
};

bool PACXXIntrinsicMapper::runOnModule(Module &M) {
  bool modified = true;

  struct IntrinsicVisitor : public InstVisitor<IntrinsicVisitor> {

    void visitCallInst(CallInst &CI) {

      if (auto II = dyn_cast<IntrinsicInst>(&CI)){
        if (isPACXXIntrinsic(II->getIntrinsicID()))
        {
          if (auto mappedIntrin = mapPACXXIntrinsic(M, II->getIntrinsicID()))
          {
            II->setCalledFunction(mappedIntrin);
          }
        }
      }
    }

    Module* M;
    TargetTransformInfo *TTI;
  } visitor;

  auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");
  visitor.M = &M;
  for (auto &F : M) {
    visitor.TTI = &getAnalysis<TargetTransformInfoWrapperPass>().getTTI(F);
    visitor.visit(F);
  }
  return modified;
}

void PACXXIntrinsicMapper::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<TargetLibraryInfoWrapperPass>();
  AU.addRequired<TargetTransformInfoWrapperPass>();
}

}

char PACXXIntrinsicMapper::ID = 0;

INITIALIZE_PASS_BEGIN(PACXXIntrinsicMapper, "pacxx-intrin-mapper",
                      "PACXXSelectEmitter: transform masked intrinsics to selects", true, true)
  INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
  INITIALIZE_PASS_DEPENDENCY(TargetTransformInfoWrapperPass)
INITIALIZE_PASS_END(PACXXIntrinsicMapper, "pacxx-emit-select",
                    "PACXXIntrinsicMapper: transform pacxx intrinsics to target dependend intrinsics", true, true)

namespace llvm {
Pass *createPACXXIntrinsicMapperPass() {
  return new PACXXIntrinsicMapper();
}
}
