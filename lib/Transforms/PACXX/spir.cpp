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

    kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

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

    // test code for alloca in AS 3
    struct AllocaRewriter : public InstVisitor<AllocaRewriter>{
      void visitAllocaInst(AllocaInst& I){
        if (I.getMetadata("pacxx.as.shared")) {
          IRBuilder<> builder(&I);
          //auto alloca = builder.CreateAlloca(I.getAllocatedType(), 3, I.getArraySize(), "sharedMem");
          //alloca->setAlignment(I.getAlignment());
          auto M = I.getParent()->getParent()->getParent();
          auto GV = new GlobalVariable(*M, I.getAllocatedType(), false,
                                       GlobalValue::ExternalLinkage, nullptr, "sm", nullptr, GlobalValue::NotThreadLocal, 3, false);
          auto cast = builder.CreateAddrSpaceCast(GV, I.getAllocatedType()->getPointerTo(0));
          I.replaceAllUsesWith(cast);
        }
      }
    } allocaRewriter;

    for (auto F : kernels)
      allocaRewriter.visit(F);

    for (const auto &p : repGV) {
      p.first->replaceAllUsesWith(p.second);
      p.first->dropAllReferences();
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
