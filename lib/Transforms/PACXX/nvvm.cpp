/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2014
*/

#include <cassert>
#include <iostream>
#include <vector>

#define DEBUG_TYPE "nvvm-transformation"

#include "llvm/IR/Argument.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InlineAsm.h"
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

#include "CallVisitor.h"
#include "ModuleHelper.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {

struct NVVMPass : public ModulePass {
  static char ID;
  NVVMPass() : ModulePass(ID) {}
  virtual ~NVVMPass() {}

  virtual bool runOnModule(Module &M) {
    _M = &M;
    bool modified = true;
    vector<Function *> deleted_functions;

    unsigned ptrSize = 64; // M.getDataLayout()->getPointerSizeInBits();
    M.setTargetTriple("nvptx64-unknown-unknown");

    if (ptrSize == 64)
      M.setDataLayout("e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:"
                      "64-f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-"
                      "v128:128:128-n16:32:64");
    else
      M.setDataLayout("e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:"
                      "64-f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-"
                      "v128:128:128-n16:32:64");

    auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

    auto visitor = make_CallVisitor([](CallInst *I) {
      if (I->isInlineAsm()) {
        if (auto ASM = dyn_cast<InlineAsm>(I->getCalledValue())) {
          if (ASM->getConstraintString().find("memory") != string::npos) {
            I->eraseFromParent();
            return;
          }
        }
      }
    });

    for (auto &F : kernels)
      visitor.visit(F);

    auto called_functions = visitor.get();

    cleanupDeadCode(&M);

    for (auto &F : M.getFunctionList()) {
      AttributeList attrs = F.getAttributes();
      AttributeList new_attrs;
      int idx = 0;
      for (unsigned x = 0; x != attrs.getNumSlots(); ++x)
        for (auto i = attrs.begin(x), e = attrs.end(x); i != e; ++i) {
          if (i->isEnumAttribute()) {
            switch (i->getKindAsEnum()) {
            case Attribute::AlwaysInline:
            case Attribute::InlineHint:
            case Attribute::NoInline:
            case Attribute::NoUnwind:
            case Attribute::ReadNone:
            case Attribute::ReadOnly:
            case Attribute::OptimizeForSize:
            case Attribute::NoReturn:
              new_attrs.addAttribute(M.getContext(), idx, i->getKindAsEnum());
              idx++;
              break;
            default:
              break;
            }
          }
        }

      F.setAlignment(0);
      //      F.setAttributes(new_attrs);
      //      F.setCallingConv(CallingConv::C);
      F.setLinkage(GlobalValue::LinkageTypes::ExternalLinkage);
      F.setVisibility(GlobalValue::VisibilityTypes::DefaultVisibility);
    }

    for (auto &F : kernels) {
      F->setCallingConv(CallingConv::PTX_Kernel);
      AttributeList new_attrs;
      F->setAttributes(new_attrs);
    }

    return modified;
  }

private:
  Module *_M;
};

char NVVMPass::ID = 0;
static RegisterPass<NVVMPass> X("nvvm", "LLVM to NVVM IR pass", false, false);
}

namespace llvm {
Pass *createPACXXNvvmPass() { return new NVVMPass(); }
}
