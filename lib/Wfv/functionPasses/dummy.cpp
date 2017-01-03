/**
 * @file   dummy.cpp
 * @date   28.03.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */

#include "wfv/functionPasses/dummy.h"
#include "wfv/wfvInfo.h"
#include "wfv/wfvConfig.h"

#include <stdexcept>

using namespace llvm;


char Dummy::ID = 0;
// NOTE: The order of initialized dependencies is important
//       to prevent 'Unable to schedule' errors!
INITIALIZE_PASS_BEGIN(Dummy, "dummy", "Dummy", false, false)
INITIALIZE_PASS_DEPENDENCY(WFVInfo)
INITIALIZE_PASS_END(Dummy, "dummy", "Dummy", false, false)

// Public interface to the Dummy pass
FunctionPass*
llvm::createDummyPass()
{
	return new Dummy();
}



Dummy::Dummy()
    : FunctionPass(ID), mInfo(nullptr)
{
    initializeDummyPass(*PassRegistry::getPassRegistry());
}

Dummy::~Dummy()
{
    mInfo = nullptr; // Deleted by PassManager.
}

void
Dummy::releaseMemory()
{
}

void
Dummy::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<WFVInfo>();
    AU.addPreserved<WFVInfo>();
}

bool
Dummy::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
Dummy::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
Dummy::runOnFunction(Function& F)
{
    mInfo = &getAnalysis<WFVInfo>();

    // If an error occurred in one of the previous phases, abort.
    assert (mInfo->mFailure);
    if (*mInfo->mFailure) return false;

    return false;
}

void
Dummy::print(raw_ostream& O, const Module* M) const
{
}

