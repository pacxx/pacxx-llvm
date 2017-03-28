/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2014
*/

//#include <iostream>
#include <vector>
#include <cassert>
#include <algorithm>

#define DEBUG_TYPE "spir_vector_fixer"

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
#include "llvm/IR/Dominators.h"

#include "ModuleHelper.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

#define STD_VECTOR_TYPE "class.std::__1::vector"

namespace {

struct SPIRVectorFixer : public ModulePass {
  static char ID;
  SPIRVectorFixer(size_t head_offset = 0)
      : ModulePass(ID), offset(head_offset) {}
  virtual ~SPIRVectorFixer() {}

  virtual bool runOnModule(Module &M) {

    bool modified = true;

    auto &ctx = M.getContext();

    // FIXME: barrier fix for INTEL OpenCL SDK
    SmallVector<Type *, 1> bargs;
    bargs.push_back(Type::getInt32Ty(ctx));
    auto BT = FunctionType::get(Type::getVoidTy(ctx), bargs, false);
    Function *B = cast<Function>(M.getOrInsertFunction("_Z7barrierj", BT));

    if (B) {
      B->setCallingConv(CallingConv::SPIR_FUNC);
      B->setAttributes(AttributeList{});
    }

    vector<CallInst *> calls;

    auto visitor = make_CallVisitor([&](CallInst *I) {
      if (I->isInlineAsm()) {
        calls.push_back(I);
      }
    });

    auto kernels = getTagedFunctions(&M, "nvvm.annotations", "kernel");

    for (auto F : kernels)
      visitor.visit(F);

    for (auto p : calls) {
      SmallVector<Value *, 1> args;
      args.push_back(ConstantInt::get(Type::getInt32Ty(ctx), 1));
      auto C = CallInst::Create(B, args, "", p);
      C->setCallingConv(CallingConv::SPIR_FUNC);
      p->eraseFromParent();
    }

	// FIXME: end of barrier fix for INTEL OpenCL SDK


	// FIXME: endless recursion fix for AMD OpenCL SDK

	vector<Function*> remove;
	for (auto& F : M.getFunctionList())
	{
		if (F.isDeclaration())
		{
			if (F.hasNUses(0))
				remove.push_back(&F);
		}
	}

	for (auto F : remove)
		F->eraseFromParent();

	// FIXME: endless recursion fix for AMD OpenCL SDK


    if (offset == 0) // nothing to do
      return modified;

	for (auto F : kernels)
		fixVectorOffset(F, offset);

    return modified;
  }

private:
  size_t offset;
};

char SPIRVectorFixer::ID = 0;
static RegisterPass<SPIRVectorFixer>
    X("spir_vector_fix",
      "PACXX: fixes index calculations on std::vector type in SPIR code", false,
      false);
}

namespace llvm {
Pass *createPACXXSPIRVectorFixerPass(size_t offset) {
  return new SPIRVectorFixer(offset);
}
}
