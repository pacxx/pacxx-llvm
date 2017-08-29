/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2014
*/

#include <vector>
#include <cassert>
#include <algorithm>

#define DEBUG_TYPE "pacxx_reflection"

#include "llvm/Transforms/PACXXTransforms.h"
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
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/Support/Host.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/LegacyPassManager.h"

#include "ModuleHelper.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace pacxx {

PACXXReflection::PACXXReflection()
    : ModulePass(ID) {}
PACXXReflection::~PACXXReflection() {}

bool PACXXReflection::runOnModule(Module &M) {
  auto modified =  runOnModuleAtCompileTime(M);

  RM = CloneModule(&M);

  return modified;
}

std::unique_ptr<Module> PACXXReflection::getReflectionModule() {
  return std::move(RM);
}

bool PACXXReflection::runOnModuleAtCompileTime(Module &M) {

  bool modified = true;

  ReflectionHandler H(&M);

  for (auto &F : M.getFunctionList()) {
    H.visit(F);
  }

  H.finalize();

  return modified;
}

void PACXXReflection::cleanFromKerneles(Module &M) {
  auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

  for (auto F : kernels) {
    F->replaceAllUsesWith(UndefValue::get(F->getType()));
    F->eraseFromParent();
  }

  vector<Function *> Fs;
  for (auto &F : M.getFunctionList()) {
    if (F.getName().startswith("__pacxx"))
      Fs.push_back(&F);
    else if (F.isDeclaration())
      Fs.push_back(&F);
  }

  for (auto F : Fs)
    F->eraseFromParent();

  auto &MDs = M.getNamedMDList();

  vector<NamedMDNode *> nMDs;

  for (auto &MD : MDs) {
    if (MD.getName().startswith("opencl") || MD.getName().startswith("nvvm") ||
        MD.getName().startswith("pacxx.kernel"))
      nMDs.push_back(&MD);
  }

  for (auto MD : nMDs)
    MD->eraseFromParent();
}

void PACXXReflection::cleanFromReflections(Module &M) {
  auto reflections = pacxx::getTagedFunctions(&M, "pacxx.reflection", "");

  for (auto F : reflections) {
    F->replaceAllUsesWith(UndefValue::get(F->getType()));
    F->eraseFromParent();
  }

  auto &MDs = M.getNamedMDList();

  vector<NamedMDNode *> nMDs;

  for (auto &MD : MDs) {
    if (MD.getName().startswith("pacxx"))
      nMDs.push_back(&MD);
  }

  for (auto MD : nMDs)
    MD->eraseFromParent();
}

PACXXReflection::ReflectionHandler::ReflectionHandler(Module *module)
    : M(module), reflects(pacxx::getTagedFunctions(M, "pacxx.reflection", "")),
      replacements(), count(0) {}

void PACXXReflection::ReflectionHandler::visitCallInst(CallInst &CI) {

  Function *F = CI.getCalledFunction();

  if (F) {
    if (find(reflects.begin(), reflects.end(), F) != reflects.end()) {
      Function *PRF = M->getFunction("__pacxx_reflect");
      if (!PRF) {
        SmallVector<Type *, 1> Params;
        Params.push_back(IntegerType::getInt32Ty(CI.getContext()));
        FunctionType *FTy =
            FunctionType::get(F->getReturnType(), Params, false);
        PRF = Function::Create(FTy, GlobalValue::LinkageTypes::ExternalLinkage,
                               "__pacxx_reflect", M);
        PRF->setAttributes(AttributeList{});
      }

      SmallVector<Value *, 1> args;
      args.push_back(
          ConstantInt::get(IntegerType::getInt32Ty(M->getContext()), ++count));

      CallInst *reflectCall = CallInst::Create(PRF, args);
      replacements.insert(make_pair(&CI, reflectCall));

      stubs.push_back(make_pair(&CI, count));
    }
  }
}

Function *PACXXReflection::ReflectionHandler::createCallStub(CallInst &CI,
                                                             int c) {
  auto &Ctx = CI.getContext();

  Function *kernel = CI.getParent()->getParent();
  const auto &M = kernel->getParent();

  ValueToValueMapTy VMap;
  vector<Type *> argTy;
  for (auto I = kernel->arg_begin(), E = kernel->arg_end(); I != E; ++I) {
    auto &arg = *I;
    argTy.push_back(arg.getType());
  }

  auto FTy = FunctionType::get(CI.getType(), argTy, false);

  Function *F = cast<Function>(M->getOrInsertFunction(
      std::string("__pacxx_reflection_stage") + std::to_string(c), FTy));

  SmallVector<ReturnInst *, 3> rets;
  for (auto &BB : kernel->getBasicBlockList())
    for (auto &I : BB.getInstList()) {
      if (auto r = dyn_cast<ReturnInst>(&I)) {
        rets.push_back(r);
      }
    }

  auto DestI = F->arg_begin();
  int i = 0;
  for (auto I = kernel->arg_begin(); I != kernel->arg_end(); ++I) {
    DestI->setName(string("arg") + to_string(++i));
    VMap[cast<Value>(I)] = cast<Value>(DestI++);
  }

  CloneFunctionInto(F, kernel, VMap, false, rets);

  F->setCallingConv(CallingConv::C);

  bool found = false;
  vector<Instruction *> dead;
  CallInst *clonedCI;
  for (auto &BB : F->getBasicBlockList()) {
    found = false;
    for (auto &I : BB.getInstList()) {
      if (auto cCI = dyn_cast<CallInst>(&I)) {
        if (CI.getCalledFunction() == cCI->getCalledFunction()) {
          clonedCI = cCI;
          found = true;
        } else {
          if (cCI->getCalledFunction() &&
              isOpenCLFunction(cCI->getCalledFunction()->getName())) {
            cCI->replaceAllUsesWith(ConstantInt::get(cCI->getType(), 0));
          }
        }
        continue;
      }
      if (found) {
        if (auto BR = dyn_cast<BranchInst>(&I)) {
          for (auto succ : BR->successors())
            succ->removePredecessor(BR->getParent());

          dead.push_back(BR);
        } else {
          I.replaceAllUsesWith(UndefValue::get(I.getType()));
          dead.push_back(&I);
        }
      }
    }
  }
  for (auto I : dead)
    I->eraseFromParent();

  auto exitBB = BasicBlock::Create(Ctx, "exit", F);
  // auto unreach = new UnreachableInst(Ctx, clonedCI->getNextNode());
  auto exitBR = BranchInst::Create(exitBB, clonedCI->getParent());

  ReturnInst::Create(Ctx, clonedCI, exitBB);

  // vector<BasicBlock *> deadBB;
  // for (auto &BB : F->getBasicBlockList()) {
  //  if (&BB == &F->getBasicBlockList().front())
  //    continue;

  //  if (!BB.getSinglePredecessor()) {
  //    deadBB.push_back(&BB);
  //  }
  //}

  // for (auto BB : deadBB)
  //  BB->eraseFromParent();

  /* if (CI.getNumArgOperands() >= 1){
     if (ConstantExpr *ce = dyn_cast<ConstantExpr>(CI.getArgOperand(1))) {

       NamedMDNode *MD = M->getOrInsertNamedMetadata("pacxx.reflection");
       SmallVector<Metadata *, 3> MDArgs;
       MDArgs.push_back(llvm::ConstantAsMetadata::get(ce->getOperand(0)));
       MDArgs.push_back(MDString::get(Ctx, "reflected"));
       MDArgs.push_back(llvm::ConstantAsMetadata::get(
         ConstantInt::get(IntegerType::getInt32Ty(Ctx), c)));
       MD->addOperand(MDNode::get(Ctx, MDArgs));
       MDArgs.clear();
     }
   }
 */

  NamedMDNode *MD = M->getOrInsertNamedMetadata("pacxx.reflection");
  SmallVector<Metadata *, 3> MDArgs;
  MDArgs.push_back(llvm::ConstantAsMetadata::get(F));
  MDArgs.push_back(MDString::get(Ctx, "stage"));
  MDArgs.push_back(llvm::ConstantAsMetadata::get(
      ConstantInt::get(IntegerType::getInt32Ty(Ctx), c)));
  MD->addOperand(MDNode::get(Ctx, MDArgs));

  fixVectorOffset(F, 32);

  ReturnVisitor RV;
  RV.runOn(*F, exitBR);

  // DeadInstructionHandler DIH;

  // DIH.visit(F);
  // DIH.finalize();

  return createCallWrapper(F, c);
}

Function *PACXXReflection::ReflectionHandler::createCallWrapper(Function *F,
                                                                int c) {
  const auto &M = F->getParent();
  auto &Ctx = M->getContext();

  auto FTy =
      FunctionType::get(F->getReturnType(), Type::getInt8PtrTy(Ctx), false);
  auto wrapper = cast<Function>(M->getOrInsertFunction(
      std::string("__pacxx_reflection_stub") + std::to_string(c), FTy));

  auto BB = BasicBlock::Create(Ctx, "enter", wrapper);

  SmallVector<Value *, 5> callArgs;
  auto &input = *wrapper->arg_begin();
  input.setName("arg0");

  unsigned argBufferSize = 0;
  unsigned offset = 0;
  for (auto I = F->arg_begin(), E = F->arg_end(); I != E; ++I) {
    auto &argument = *I;
    SmallVector<Value *, 3> idx;
    APInt off(64, offset);
    idx.push_back(
        ConstantInt::get(static_cast<Type *>(Type::getInt64Ty(Ctx)), off));
    Value *address = GetElementPtrInst::Create(
        input.getType()->getPointerElementType(), &input, idx, "", BB);
    address = BitCastInst::Create(Instruction::CastOps::BitCast, address,
                                  argument.getType()->getPointerTo(), "", BB);
    Value *load = new LoadInst(address, "", false, BB);
    callArgs.push_back(load);
    auto size = M->getDataLayout().getTypeAllocSize(argument.getType());
    offset += size;
    argBufferSize += size;
  }

  auto call = CallInst::Create(F, callArgs, "call", BB);
  ReturnInst::Create(Ctx, call, BB);
  wrapper->setMetadata("pacxx.reflection.argBufferSize",
                       MDNode::get(Ctx, llvm::ConstantAsMetadata::get(ConstantInt::get(
                           IntegerType::getInt32Ty(Ctx), argBufferSize))));

  NamedMDNode *MD = M->getOrInsertNamedMetadata("pacxx.reflection");
  SmallVector<Metadata *, 3> MDArgs;
  MDArgs.push_back(llvm::ConstantAsMetadata::get(wrapper));
  MDArgs.push_back(MDString::get(Ctx, "stub"));
  MDArgs.push_back(llvm::ConstantAsMetadata::get(
      ConstantInt::get(IntegerType::getInt32Ty(Ctx), c)));
  MD->addOperand(MDNode::get(Ctx, MDArgs));
  return wrapper;
}

void PACXXReflection::ReflectionHandler::finalize() {
  for (auto &c : stubs) {
    createCallStub(*c.first, c.second);
  }
  for (auto &p : replacements) {
    ReplaceInstWithInst(p.first, p.second);
    auto it = std::find_if(stubs.begin(), stubs.end(),
                           [&](auto &s) { return s.first == p.first; });
    if (it != stubs.end())
      p.second->setMetadata(
          "pacxx.reflect.stage",
          MDNode::get(
              M->getContext(),
              llvm::ConstantAsMetadata::get(ConstantInt::get(
                  IntegerType::getInt32Ty(M->getContext()), it->second))));
  }
}

char PACXXReflection::ID = 0;
}

namespace llvm {
Pass *createPACXXReflectionPass() {
  return new PACXXReflection();
}
}
