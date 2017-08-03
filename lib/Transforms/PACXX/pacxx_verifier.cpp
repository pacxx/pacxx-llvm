/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2015
*/

//#include <iostream>
#include <vector>
#include <cassert>
#include <algorithm>

#define DEBUG_TYPE "pacxx_verifier"

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

using namespace llvm;
using namespace std;
//using namespace pacxx;

namespace {

struct PACXXVerifier : public ModulePass {
  static char ID;
  PACXXVerifier(bool runtime = false) : ModulePass(ID) {}
  virtual ~PACXXVerifier() {}

  virtual bool runOnModule(Module &M) {
	auto pacxxReflect = M.getFunction("__pacxx_reflect");

	for (auto& F : M.getFunctionList())
	{
		if (&F == pacxxReflect)
			continue;
		if (F.getName().equals("__printf") || F.getName().equals("vprintf")) // from syscall to device functions
			continue;

		if (F.isDeclaration() && !F.isIntrinsic() && F.hasNUsesOrMore(1)) {
			//__error("Undefined reference to function ", F.getName().str(), " found!");
			//__message("This can result from a call to a function library (e.g., printf)");
			exit(1);
		}
	}
	

    return false;
  }
};

char PACXXVerifier::ID = 0;
static RegisterPass<PACXXVerifier>
    X("pacxx_verifier", "PACXX: verifier to check if the llvm module is valid for the PACXX RT.", false,
      false);
}

namespace llvm {
Pass *createPACXXVerifier() { return new PACXXVerifier(); }
}
