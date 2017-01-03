/**
 * @file   linearizationVerifier.cpp
 * @date   02.06.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */

#include "wfv/analysis/linearizationVerifier.h"
#include "wfv/wfvConfig.h"
#include "wfv/analysis/vectorizationAnalysis.h"
#include "wfv/utils/metadata.h"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Instructions.h"

#include <stdexcept>

using namespace llvm;



char LinearizationVerifier::ID = 0;
INITIALIZE_PASS_BEGIN(LinearizationVerifier, "linearizationVerifier", "LinearizationVerifier", false, true)
INITIALIZE_PASS_DEPENDENCY(WFVInfo)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_END(LinearizationVerifier, "linearizationVerifier", "LinearizationVerifier", false, true)

// Public interface to the LinearizationVerifier pass
FunctionPass*
llvm::createLinearizationVerifierPass()
{
	return new LinearizationVerifier();
}



LinearizationVerifier::LinearizationVerifier() : FunctionPass(ID), mInfo(NULL)
{
    initializeLinearizationVerifierPass(*PassRegistry::getPassRegistry());
}

LinearizationVerifier::~LinearizationVerifier()
{
    mInfo = NULL; // Deleted by PassManager.
}

void
LinearizationVerifier::releaseMemory()
{
}

void
LinearizationVerifier::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<WFVInfo>();
    AU.addRequired<LoopInfoWrapperPass>();

    AU.setPreservesAll();
}

bool
LinearizationVerifier::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
LinearizationVerifier::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
LinearizationVerifier::runOnFunction(Function& F)
{
    mInfo     = &getAnalysis<WFVInfo>();
    mLoopInfo = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

    // If an error occurred in one of the previous phases, abort.
    assert (mInfo->mFailure);
    if (*mInfo->mFailure) return false;

    const bool verified = verify(F);
    *mInfo->mFailure |= !verified;

    // Function was not changed.
    return false;
}

void
LinearizationVerifier::print(raw_ostream& O, const Module* M) const
{
}

bool
LinearizationVerifier::verify(const Function& F) const
{
    bool verified    = true;
    bool returnFound = false;

    for (auto &BB : F)
    {
        verified &= verify(BB);

        if (isa<ReturnInst>(BB.getTerminator()))
        {
            if (returnFound)
            {
                // If the block is ALWAYS_BY_ALL or ALWAYS_BY_ALL_OR_NONE,
                // this is okay.
                if (!WFV::hasMetadata(&BB, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) &&
                    !WFV::hasMetadata(&BB, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE))
                {
                    errs() << "ERROR: Function has multiple return blocks that "
                            << "can be reached by divergent control flow!\n";
                    verified = false;
                }
            }
            else
            {
                returnFound = true;
            }
        }

        for (auto &I : BB)
        {
            verified &= verify(I);
        }
    }

    for (auto &L : *mLoopInfo)
    {
        verified &= verify(*L);
    }

    return verified;
}

bool
LinearizationVerifier::verify(const BasicBlock& block) const
{
    bool verified = true;

    const Loop* loop = mLoopInfo->getLoopFor(&block);

    // Blocks are only allowed to have an outgoing *conditional* branch
    // if at least one successor is OPTIONAL or if exactly one of the outgoing edges
    // is a loop back edge.
    if (const BranchInst* branch = dyn_cast<BranchInst>(block.getTerminator()))
    {
        if (branch->isConditional() &&
            !branch->getCondition()->getName().startswith("wfv.switch.cond"))
        {
            bool optionalFound = false;
            bool backedgeFound = false;
            for (unsigned i=0, e=branch->getNumSuccessors(); i<e; ++i)
            {
                const BasicBlock* succBB = branch->getSuccessor(i);

                optionalFound |= WFV::hasMetadata(succBB, WFV::WFV_METADATA_OPTIONAL);
                backedgeFound |= loop && loop->getHeader() == succBB;
            }

            if (!optionalFound && !backedgeFound)
            {
                errs() << "WARNING: Block '" << block.getName() << "' should not end with "
                    << "conditional branch if only MANDATORY successors (can happen for "
                    << "multi-rewires, though).\n";
                //verified = false;
            }
        }
    }

    // Blocks are only allowed to have an outgoing switch if the successors
    // are all OPTIONAL, or if the switch itself is OP_UNIFORM.
    if (const SwitchInst* switchInst = dyn_cast<SwitchInst>(block.getTerminator()))
    {
        if (!WFV::hasMetadata(switchInst, WFV::WFV_METADATA_OP_UNIFORM))
        {
            for (unsigned i=0, e=switchInst->getNumSuccessors(); i<e; ++i)
            {
                const BasicBlock* succBB = switchInst->getSuccessor(i);
                if (!WFV::hasMetadata(succBB, WFV::WFV_METADATA_OPTIONAL))
                {
                    errs() << "ERROR: Block '" << block.getName()
                        << "' with switch statement has MANDATORY successor: "
                        << succBB->getName() << "\n";
                    verified = false;
                }
            }
        }
    }

    return verified;
}

bool
LinearizationVerifier::verify(const Instruction& inst) const
{
    bool verified = true;

    return verified;
}

bool
LinearizationVerifier::verify(const Loop& loop) const
{
    bool verified = true;

    for (auto &SL : loop)
    {
        verified &= verify(*SL);
    }

    return verified;
}

