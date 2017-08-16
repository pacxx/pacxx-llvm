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
template <typename T>
Instruction *getFirstInstructionForConstantExpr(T &kernels, ConstantExpr &CE) {
  for (auto CEU : CE.users()) {
    if (auto I = dyn_cast<Instruction>(CEU)) {
      if (I->getParent()) {
        return I;
      }
    }
    if (auto nextCE = dyn_cast<ConstantExpr>(CEU)) {
      return getFirstInstructionForConstantExpr(kernels, *nextCE);
    }
  }
  return nullptr;
}

template <typename T>
Function *getParentKernel(T &kernels, GlobalVariable &GV) {
  Function *F = nullptr;
  for (auto U : GV.users()) {
    Instruction *I = nullptr;
    if (isa<Instruction>(U)) {
      I = cast<Instruction>(U);
    }
    if (auto CE = dyn_cast<ConstantExpr>(U)) {
      I = getFirstInstructionForConstantExpr(kernels, *CE);
    }

    if (I && I->getParent()) {
      F = I->getParent()->getParent();
    }
    if (find(begin(kernels), end(kernels), F) != end(kernels)) {
      break;
    }
  }
  return F;
}

struct SPIRPass : public ModulePass {
  static char ID;
  SPIRPass() : ModulePass(ID) {}
  virtual ~SPIRPass() {}

  virtual bool runOnModule(Module &M) {
    _M = &M;

    bool modified = true;

    unsigned ptrSize = M.getDataLayout().getPointerSizeInBits();

    if (ptrSize == 64) {
      M.setTargetTriple("spir64-unknown-unknown");

      M.setDataLayout("e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:"
                      "64-f32:32:32-f64:64:64-v16:16:16-v24:32:32-v32:32:32-"
                      "v48:64:64-v64:64:64-v96:128:128-v128:128:128-v192:256:"
                      "256-v256:256:256-v512:512:512-v1024:1024:1024");
    } else {
      M.setTargetTriple("spir-unknown-unknown");

      M.setDataLayout("e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:"
                      "64-f32:32:32-f64:64:64-v16:16:16-v24:32:32-v32:32:32-"
                      "v48:64:64-v64:64:64-v96:128:128-v128:128:128-v192:256:"
                      "256-v256:256:256-v512:512:512-v1024:1024:1024");
    }

    kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");
    auto barrier = M.getFunction("_Z7barrierj");
    if (barrier) {
      auto tb =
          M.getOrInsertFunction("tmp_barrier", barrier->getFunctionType());

      barrier->replaceAllUsesWith(tb);
    }

    auto visitor = make_CallVisitor([&](CallInst *I) {
      if (!I)
        return;

      if (!I->isInlineAsm()) {

        if (!isa<Function>(I->getCalledValue())) {
          I->dump();
        }
      }
    });

    // handle parameters to bring them into AS 1
    for (auto &F : kernels) {
      visitor.visit(F);

      // Mutate pointer types to bring them into AS 1
      auto &BB = F->getBasicBlockList().front();
      auto II = &BB.getInstList().front();
      bool mutate = false;
      for (auto &arg : F->args()) {
        if (arg.getType()->isPointerTy()) {
          if (arg.getType()->getPointerAddressSpace() == 0) {
            auto AL = new AllocaInst(arg.getType(), 0, "", II);
            auto SI = new StoreInst(&arg, AL, II);
            auto LI = new LoadInst(AL, "", II);
            arg.replaceAllUsesWith(LI);
            arg.mutateType(
                arg.getType()->getPointerElementType()->getPointerTo(1));

            auto ASC = new AddrSpaceCastInst(
                &arg, arg.getType()->getPointerElementType()->getPointerTo(0),
                "", II);
            LI->replaceAllUsesWith(ASC);
            LI->eraseFromParent();
            SI->eraseFromParent();
            AL->eraseFromParent();
            mutate = true;
          }
        }
      }

      if (mutate) {
        SmallVector<Type *, 8> Params;
        for (auto &arg : F->args())
          Params.push_back(arg.getType());

        Type *RetTy = F->getReturnType();
        FunctionType *NFTy = FunctionType::get(RetTy, Params, false);
        auto name = F->getName().str();
        F->setName("undead");
        auto NF = Function::Create(NFTy, F->getLinkage(), name, &M);
        auto DestI = NF->arg_begin();
        ValueToValueMapTy VMap;
        for (auto I = F->arg_begin(); I != F->arg_end(); ++I) {
          DestI->setName(I->getName());
          VMap[cast<Value>(I)] = cast<Value>(DestI++);
        }
        SmallVector<ReturnInst *, 8> returns;
        CloneFunctionInto(NF, F, VMap, true, returns);
        if (auto MD = M.getNamedMetadata("nvvm.annotations")) {
          for (unsigned i = 0; i != MD->getNumOperands(); ++i) {
            auto Op = MD->getOperand(i);
            if (Op->getOperand(0)) {
              if (auto *KF = dyn_cast<Function>(
                  dyn_cast<ValueAsMetadata>(Op->getOperand(0).get())
                      ->getValue())) {
                if (KF == F) {
                  Op->replaceOperandWith(0, ValueAsMetadata::get(NF));
                }
              }
            }
          }
        }

        // F->replaceAllUsesWith(NF);
        ReplaceUnsafe(F, NF);
        //  F->eraseFromParent();
      }
    }

    kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");
    // handle shared memory declarations
    map<Function *, unsigned> SMMapping;
    map<GlobalVariable *, Constant *> repGV;
    for (auto &GV : M.globals()) {
      if (GV.getMetadata("pacxx.as.shared") != nullptr &&
          GV.getType()->getPointerAddressSpace() != 3) {
          auto F = getParentKernel(kernels, GV);
          string newName = GV.getName().str() + ".sm";
          Type *elemType = GV.getType()->getPointerElementType();

          auto newGV = new GlobalVariable(
                  M, elemType, false,
                  llvm::GlobalValue::LinkageTypes::ExternalLinkage,
                 nullptr, // ConstantAggregateZero::get(elemType),
                  newName, &GV, GV.getThreadLocalMode(), 3, false);

          Constant *access = ConstantExpr::getAddrSpaceCast(newGV, GV.getType());

        repGV[&GV] = access;
        if (F) {
          unsigned i = SMMapping[F];
          newName = F->getName().str() + ".sm" + to_string(i);
          newGV->setName(newName);
          SMMapping[F] = i + 1;
        }
      }
    }

    for (const auto &p : repGV) {
      p.first->replaceAllUsesWith(p.second);
      p.first->dropAllReferences();
    }

    auto called = visitor.get();

    if (auto TB = M.getFunction("tmp_barrier")) {
      TB->replaceAllUsesWith(barrier);
    }

    // delete old kernel functions
    cleanupDeadCode(_M);
    return modified;
  }

private:
  set<Function *> kernels;
  Module *_M;
};

char SPIRPass::ID = 0;
static RegisterPass<SPIRPass> X("spir", "LLVM to SPIR IR pass", false, false);
}

namespace llvm {
Pass *createPACXXSpirPass() { return new SPIRPass(); }
}
