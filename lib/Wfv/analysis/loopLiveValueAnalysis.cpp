/**
 * @file   loopLiveValueAnalysis.cpp
 * @date   22.06.2011
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2011, 2012 Saarland University
 *
 */

#include "wfv/analysis/loopLiveValueAnalysis.h"
#include "wfv/wfvInfo.h"
#include "wfv/wfvConfig.h"
#include "wfv/utils/metadata.h"

#include "llvm/IR/Instructions.h"

#include <stdexcept>

using namespace llvm;

char LoopLiveValueAnalysis::ID = 0;
// NOTE: The order of initialized dependencies is important
//       to prevent 'Unable to schedule' errors!
INITIALIZE_PASS_BEGIN(LoopLiveValueAnalysis, "loop live value analysis", "loop live value analysis", false, false)
INITIALIZE_PASS_DEPENDENCY(WFVInfo)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_END(LoopLiveValueAnalysis, "loop live value analysis", "loop live value analysis", false, false)

// Public interface to the LoopLiveValueAnalysis pass
FunctionPass*
llvm::createLoopLiveValueAnalysisPass()
{
    return new LoopLiveValueAnalysis();
}



LoopLiveValueAnalysis::LoopLiveValueAnalysis()
    : FunctionPass(ID), mInfo(nullptr), mLoopInfo(nullptr)
{
    initializeLoopLiveValueAnalysisPass(*PassRegistry::getPassRegistry());
}

LoopLiveValueAnalysis::~LoopLiveValueAnalysis()
{
    mInfo     = nullptr; // Deleted by PassManager.
    mLoopInfo = nullptr; // Deleted by PassManager.
}

void
LoopLiveValueAnalysis::releaseMemory()
{
    for (auto &it : mLiveValueMaps)
    {
        delete it.second;
    }
}

void
LoopLiveValueAnalysis::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<WFVInfo>();
    AU.addRequired<LoopInfoWrapperPass>();

    AU.setPreservesAll();
}

bool
LoopLiveValueAnalysis::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
LoopLiveValueAnalysis::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
LoopLiveValueAnalysis::runOnFunction(Function& F)
{
    mInfo     = &getAnalysis<WFVInfo>();
    mLoopInfo = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

    // If an error occurred in one of the previous phases, abort.
    assert (mInfo->mFailure);
    if (*mInfo->mFailure) return false;

    // Do stuff.
    if(mInfo->mVerbose) outs() << "analyzing function for values live across loop boundaries\n";
    for (auto &L : *mLoopInfo)
    {
        findAllLoopLiveValues(L);
    }
	if(mInfo->mVerbose) outs() << "loop live value analysis finished.\n";

    return false;
}

void
LoopLiveValueAnalysis::print(raw_ostream& O, const Module* M) const
{
    if (mLiveValueMaps.empty())
    {
        O << "\nLoop live value set is empty!\n\n";
        return;
    }

    O << "\nLoop live value sets:\n";
    for (auto &it : mLiveValueMaps)
    {
        O << "\nLoop: " << it.first->getHeader()->getName() << "\n";
        it.second->printValues();
    }
    O << "\n\n";
}


void
LoopLiveValueAnalysis::printLoopLiveValues(const Loop* loop) const
{
    assert (loop);
    assert (mLiveValueMaps.count(loop));
    mLiveValueMaps.find(loop)->second->printValues();
}

LoopLiveValueAnalysis::LoopLiveValueInfo*
LoopLiveValueAnalysis::getLiveValueInfo(const Loop* loop) const
{
    assert (loop);
    assert (mLiveValueMaps.count(loop) && "live value set for loop not found!");

    return mLiveValueMaps.find(loop)->second;
}

// Remove value from ALL loops.
void
LoopLiveValueAnalysis::removeLiveValue(Instruction* value)
{
    assert (value);

    for (auto &it : mLiveValueMaps)
    {
        if (!it.second->hasValue(value)) continue;
        it.second->remove(value);
    }
}

void
LoopLiveValueAnalysis::updateLiveValue(Instruction* value,
                                       Instruction* newValue)
{
    assert (value && newValue);

    for (auto &it : mLiveValueMaps)
    {
        if (!it.second->hasValue(value)) continue;
        it.second->update(value, newValue);
    }
}

void
LoopLiveValueAnalysis::setLoopResults(const Loop* loop, LoopResults* results)
{
    assert (loop && results);
    assert (!hasLoopResults(loop));
    mLoopResultMap[loop] = results;
}

bool
LoopLiveValueAnalysis::hasLoopResults(const Loop* loop) const
{
    assert (loop);
    return mLoopResultMap.count(loop);
}

LoopResults*
LoopLiveValueAnalysis::getLoopResults(const Loop* loop) const
{
    assert (loop);
    assert (mLoopResultMap.count(loop));
    return mLoopResultMap.find(loop)->second;
}


void
LoopLiveValueAnalysis::findAllLoopLiveValues(Loop* loop)
{
    assert (loop);

    LoopLiveValueInfo* liveValueMap = new LoopLiveValueInfo();

    findLoopLiveValues(loop, *liveValueMap);

    mLiveValueMaps[loop] = liveValueMap;

    for (auto &SL : *loop)
    {
        findAllLoopLiveValues(SL);
    }
}

void
LoopLiveValueAnalysis::findLoopLiveValues(Loop*              loop,
                                          LoopLiveValueInfo& liveValueMap)
{
    assert (loop);
    if(mInfo->mVerbose) outs() << "\ncollecting all values that are live across loop boundaries...\n";

    for (Loop::block_iterator BB=loop->block_begin(); BB!=loop->block_end(); ++BB)
    {
        BasicBlock* curBB = *BB;

        for (auto &I : *curBB)
        {
            // Masks are generated correctly, so we don't consider them as live values.
            if (WFV::hasMetadata(&I, WFV::WFV_METADATA_MASK)) continue;

            Instruction* useI = findUseOutsideLoop(&I, loop);
            if (!useI) continue;

            if(mInfo->mVerbose) {
                outs() << "  found live value: " << I << "\n";
                outs() << "    with use outside loop-boundary: " << *useI << "\n";
            }

            liveValueMap.insert(&I);
        }
    }

    if(mInfo->mVerbose) liveValueMap.printValues();
}

// Returns instruction that uses 'inst' outside the loop.
Instruction*
LoopLiveValueAnalysis::findUseOutsideLoop(Instruction* inst,
                                          const Loop*  loop) const
{
    if(mInfo->mVerbose) {
        for (Instruction::use_iterator U = inst->use_begin(); U != inst->use_end(); ++U) {
            Instruction *useI = cast<Instruction>(*U);
            if (loop->contains(useI->getParent())) continue;
            assert(isa<PHINode>(useI) && "not in LCSSA?!");
        }
    }
    for (Instruction::use_iterator U=inst->use_begin(); U!=inst->use_end(); ++U)
    {
        assert (isa<Instruction>(*U) && "all uses have to be instructions (!?)");
        Instruction* useI = cast<Instruction>(*U);

        // Return the use if it is outside of the loop.
        if (!loop->contains(useI->getParent())) return useI;
    }

    return nullptr;
}


typedef LoopLiveValueAnalysis::LoopLiveValueInfo LLVI;

LLVI::LoopLiveValueInfo()
{
}

LLVI::~LoopLiveValueInfo()
{
}

void
LLVI::insert(Instruction* value)
{
    assert (value);
    mLiveValues.insert(value);
}

void
LLVI::remove(Instruction* value)
{
    assert (value);
    mLiveValues.remove(value);
}

void
LLVI::update(Instruction* value, Instruction* newValue)
{
    assert (value && newValue);
    remove(value);
    insert(newValue);
}

void
LLVI::printValues() const
{
    outs() << "\nValues live across loop boundaries:\n";
    for (const auto &it : mLiveValues)
    {
        outs() << "  * " << *it << "\n";
    }
    outs() << "\n";
}
