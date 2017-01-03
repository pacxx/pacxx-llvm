/**
 * @file   loopExitCanonicalizer.cpp
 * @date   23.10.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */

#include "wfv/functionPasses/loopExitCanonicalizer.h"
#include "wfv/wfvInfo.h"
#include "wfv/wfvConfig.h"
#include "wfv/utils/wfvTools.h"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Instructions.h"

#include <stdexcept>

using namespace llvm;


char LoopExitCanonicalizer::ID = 0;
// NOTE: The order of initialized dependencies is important
//       to prevent 'Unable to schedule' errors!
INITIALIZE_PASS_BEGIN(LoopExitCanonicalizer, "loop-exit-canonicalizer", "LoopExitCanonicalizer", false, false)
INITIALIZE_PASS_DEPENDENCY(WFVInfo)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_END(LoopExitCanonicalizer, "loop-exit-canonicalizer", "LoopExitCanonicalizer", false, false)

// Public interface to the LoopExitCanonicalizer pass
FunctionPass*
llvm::createLoopExitCanonicalizerPass()
{
	return new LoopExitCanonicalizer();
}



LoopExitCanonicalizer::LoopExitCanonicalizer()
    : FunctionPass(ID), mInfo(nullptr), mLoopInfo(nullptr)
{
    initializeLoopExitCanonicalizerPass(*PassRegistry::getPassRegistry());
}

LoopExitCanonicalizer::~LoopExitCanonicalizer()
{
    mInfo = nullptr; // Deleted by PassManager.
}

void
LoopExitCanonicalizer::releaseMemory()
{
}

void
LoopExitCanonicalizer::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<WFVInfo>();
    AU.addPreserved<WFVInfo>();

    AU.addRequired<LoopInfoWrapperPass>();
    AU.addPreserved<LoopInfoWrapperPass>();
}

bool
LoopExitCanonicalizer::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
LoopExitCanonicalizer::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
LoopExitCanonicalizer::runOnFunction(Function& F)
{
    mInfo     = &getAnalysis<WFVInfo>();
    mLoopInfo = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

    // If an error occurred in one of the previous phases, abort.
    assert (mInfo->mFailure);
    if (*mInfo->mFailure) return false;

    for (auto &L : *mLoopInfo) {
        if(!canonicalizeLoop(L))
            *mInfo->mFailure = true;
    }

    return false;
}

void
LoopExitCanonicalizer::print(raw_ostream& O, const Module* M) const
{
}

bool
LoopExitCanonicalizer::canonicalizeLoop(Loop* loop) const
{
    assert (loop);

    for (auto &SL : *loop)
    {
        canonicalizeLoop(SL);
    }

    SmallVector<BasicBlock*, 2> exitBlocks;
    loop->getExitBlocks(exitBlocks);

    for (const auto &exitBB : exitBlocks)
    {
        Loop* outerLoop = mLoopInfo->getLoopFor(exitBB);
        assert (outerLoop != loop);

        if (exitBB->getUniquePredecessor())
        {
            assert (loop->isLoopExiting(exitBB->getUniquePredecessor()));
            continue;
        }

        if(mInfo->mVerbose) {
            outs() << "loop exit has more than one incoming edge: '";
            outs() << exitBB->getName() << "' - canonicalizing...\n";
        }

        SmallVector<BasicBlock*, 2> exitingBlocks;
        WFV::getExitingBlocks(exitBB, *mLoopInfo, exitingBlocks);

        for (auto &exitingBB : exitingBlocks)
        {
            assert (mLoopInfo->getLoopFor(exitingBB) == loop &&
                    "exiting blocks from different loop go to same exit block - not in LCSSA?");

            BasicBlock* newExitBB = createIntermediateBlock(exitingBB, exitBB);
            if (outerLoop)
            {
                outerLoop->addBasicBlockToLoop(newExitBB, *mLoopInfo);
            }
        }
    }

    if(mInfo->mVerbose) {
        exitBlocks.clear();
        loop->getExitBlocks(exitBlocks);
        for (const auto &exitBB : exitBlocks) {
            assert(exitBB->getUniquePredecessor());
        }
    }
    return true;
}

BasicBlock*
LoopExitCanonicalizer::createIntermediateBlock(BasicBlock* source,
                                               BasicBlock* target) const
{
    assert (source && target);
    assert (!target->getUniquePredecessor());

    BasicBlock* newTarget = BasicBlock::Create(target->getContext(),
                                               "loop.exit.dedicated",
                                               target->getParent(),
                                               target);

    // Adjust edge.
    replaceTarget(source, target, newTarget);

    // Create edge to from new to old target.
    BranchInst::Create(target, newTarget);

    // Adjust phis.
    adjustPhis(source, target, newTarget);

    return newTarget;
}

void
LoopExitCanonicalizer::adjustPhis(BasicBlock* source,
                                  BasicBlock* target,
                                  BasicBlock* newTarget) const
{
    assert (source && target && newTarget);

    for (auto &I : *target)
    {
        if (!isa<PHINode>(I)) break;
        PHINode* phi = cast<PHINode>(&I);

        PHINode* newPhi = PHINode::Create(phi->getType(),
                                          2,
                                          "lcssa.phi",
                                          newTarget->getFirstNonPHI());

        newPhi->addIncoming(phi->getIncomingValueForBlock(source), source);

        phi->removeIncomingValue(source, false /* DeletePhiIfEmpty */);
        phi->addIncoming(newPhi, newTarget);
    }
}

void
LoopExitCanonicalizer::replaceTarget(BasicBlock* source,
                                     BasicBlock* target,
                                     BasicBlock* newTarget) const
{
    assert (source && target && newTarget);

    TerminatorInst* terminator = source->getTerminator();

    bool replaced = false; WFV_UNUSED(replaced);
    for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
    {
        BasicBlock* succBB = terminator->getSuccessor(i);
        if (succBB != target) continue;

        assert (!replaced && "block must not have multiple edges going to the same target!");
        terminator->setSuccessor(i, newTarget);
        replaced = true;
    }

    assert (replaced);
}
