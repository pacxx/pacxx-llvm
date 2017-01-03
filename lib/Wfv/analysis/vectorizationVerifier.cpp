/**
 * @file   vectorizationVerifier.cpp
 * @date   02.06.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */

#include "wfv/analysis/vectorizationVerifier.h"
#include "wfv/wfvConfig.h"
#include "wfv/analysis/vectorizationAnalysis.h"
#include "wfv/utils/metadata.h"
#include "wfv/utils/wfvTools.h"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Instructions.h" // ReturnInst

#include <stdexcept>

using namespace llvm;


char VectorizationVerifier::ID = 0;
INITIALIZE_PASS_BEGIN(VectorizationVerifier, "vectorizationVerifier", "VectorizationVerifier", false, true)
INITIALIZE_PASS_DEPENDENCY(WFVInfo)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_END(VectorizationVerifier, "vectorizationVerifier", "VectorizationVerifier", false, true)

// Public interface to the VectorizationVerifier pass
FunctionPass*
llvm::createVectorizationVerifierPass()
{
	return new VectorizationVerifier();
}



VectorizationVerifier::VectorizationVerifier() : FunctionPass(ID), mInfo(NULL)
{
    initializeVectorizationVerifierPass(*PassRegistry::getPassRegistry());
}

VectorizationVerifier::~VectorizationVerifier()
{
    mInfo = NULL; // Deleted by PassManager.
}

void
VectorizationVerifier::releaseMemory()
{
}

void
VectorizationVerifier::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<WFVInfo>();
    AU.addRequired<LoopInfoWrapperPass>();

    // Fore some reason, the analysis does not have to be required -
    // it works if it is just added to the passmanager...
    //AU.addRequired<VectorizationAnalysis>();

    AU.setPreservesAll();
}

bool
VectorizationVerifier::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
VectorizationVerifier::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
VectorizationVerifier::runOnFunction(Function& F)
{
    mInfo     = &getAnalysis<WFVInfo>();
    mLoopInfo = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

    // If an error occurred in one of the previous phases, abort.
    assert (mInfo->mFailure);
    if (*mInfo->mFailure) return false;

    const bool verified = verify(F);
    if (!verified)
    {
        if (mInfo->mFailure) *mInfo->mFailure = true;
    }

    // Function was not changed.
    return false;
}

void
VectorizationVerifier::print(raw_ostream& O, const Module* M) const
{
}

bool
VectorizationVerifier::verify(const Function& F) const
{
    bool verified = true;
    unsigned vecInstCounter = 0;

    for (auto &BB : F)
    {
        verified &= verify(BB);

        for (auto &I : BB)
        {
            verified &= verify(I, vecInstCounter);
        }
    }

    for (auto &L : *mLoopInfo)
    {
        verified &= verify(*L);
    }

    return verified;
}

bool
VectorizationVerifier::verify(const BasicBlock& block) const
{
    bool verified = true;

    if (!WFV::hasMetadata(&block))
    {
        errs() << "ERROR: Block has no metadata: " << block.getName() << "\n";
        verified = false;
    }

    if (WFV::hasMetadata(&block, WFV::WFV_METADATA_DIVERGENT_TRUE) &&
        WFV::getNumIncomingEdges(block) < 2)
    {
        errs() << "ERROR: Block with less than two incoming edges can never be DIVERGENT: " << block.getName() << "\n";
        verified = false;
    }

    if (WFV::hasMetadata(&block, WFV::WFV_METADATA_DIVERGENT_TRUE))
    {
        if (!WFV::hasMetadata(&block, WFV::WFV_METADATA_DIVERGENCE_INFO))
        {
            errs() << "ERROR: DIVERGENT block has to have associated DIVERGENCE INFO: " << block.getName() << "\n";
            verified = false;
        }
    }

    if (!isa<ReturnInst>(block.getTerminator()) &&
        WFV::hasMetadata(block.getTerminator(), WFV::WFV_METADATA_OP_VARYING))
    {
        if (!WFV::hasMetadata(&block, WFV::WFV_METADATA_REWIRE_INFO))
        {
            errs() << "ERROR: Block with VARYING branch has to have associated REWIRE INFO: " << block.getName() << "\n";
            verified = false;
        }
    }

    //mLoopInfo->isLoopHeader(&block) requires block to be non-const...
    if (WFV::hasMetadata(&block, WFV::WFV_METADATA_DIVERGENT_TRUE) &&
        (mLoopInfo->getLoopFor(&block) && mLoopInfo->getLoopFor(&block)->getHeader() == &block))
    {
        errs() << "ERROR: Loop header can never be DIVERGENT: " << block.getName() << "\n";
        verified = false;
    }

    return verified;
}

bool
VectorizationVerifier::verify(const Instruction& inst, unsigned& vecInstCounter) const
{
    bool verified = true;

    if (!WFV::hasMetadata(&inst))
    {
        errs() << "ERROR: instruction has no metadata: " << inst << "\n";
        verified = false;
    }

    if (isa<TerminatorInst>(inst))
    {
        if (!WFV::hasMetadata(&inst, WFV::WFV_METADATA_OP_UNIFORM) &&
            !WFV::hasMetadata(&inst, WFV::WFV_METADATA_OP_VARYING))
        {
            errs() << "ERROR: block terminator is neither OP_UNIFORM "
                    << "nor OP_VARYING: " << inst << "\n";
            verified = false;
        }
    }

    if (WFV::hasMetadata(&inst, WFV::WFV_METADATA_INDEX_CONSECUTIVE))
    {
        if (WFV::hasMetadata(&inst, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
            WFV::hasMetadata(&inst, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
        {
            errs() << "ERROR: INDEX_CONSECUTIVE value must never be "
                    << "OP_SEQUENTIAL: " << inst << "\n";
            verified = false;
        }
    }

    if (WFV::hasMetadata(&inst, WFV::WFV_METADATA_RES_SCALARS))
    {
        if (!WFV::hasMetadata(&inst, WFV::WFV_METADATA_OP_SEQUENTIAL) &&
            !WFV::hasMetadata(&inst, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
        {
            errs() << "ERROR: RES_SCALARS value must be "
                    << "OP_SEQUENTIAL or OP_SEQUENTIAL_GUARDED: " << inst << "\n";
            verified = false;
        }
    }

    if (!WFV::hasMetadata(&inst, WFV::WFV_METADATA_RES_UNIFORM) &&
        WFV::isVectorizedType(*inst.getType()))
    {
        ++vecInstCounter;
        if (vecInstCounter <= 10)
        {
            errs() << "ERROR: Can not vectorize instruction that already has vectorized type:\n";
            errs() << "  " << inst << "\n";
            if (vecInstCounter == 10)
            {
                errs() << "Suppressing subsequent errors due to vectorized instructions...\n";
            }
        }
        verified = false;
    }

    return verified;
}

bool
VectorizationVerifier::verify(const Loop& loop) const
{
    bool verified = true;

    for (auto &SL : loop)
    {
        verified &= verify(*SL);
    }

    return verified;
}

