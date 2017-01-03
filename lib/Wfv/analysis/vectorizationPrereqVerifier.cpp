/**
 * @file   vectorizationPrereqVerifier.cpp
 * @date   02.06.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */

#include "wfv/analysis/vectorizationPrereqVerifier.h"
#include "wfv/wfvConfig.h"
#include "wfv/utils/wfvTools.h"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Instructions.h"

//#define WFV_DO_NOT_ALLOW_CRITICAL_EDGES
#ifdef WFV_DO_NOT_ALLOW_CRITICAL_EDGES
#include "llvm/Transforms/Utils/BasicBlockUtils.h" // isCriticalEdge()
#endif

#include <stdexcept>

using namespace llvm;


char VectorizationPrereqVerifier::ID = 0;
INITIALIZE_PASS_BEGIN(VectorizationPrereqVerifier, "vectorizationPrereqVerifier", "VectorizationPrereqVerifier", false, true)
INITIALIZE_PASS_DEPENDENCY(WFVInfo)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(SCCAnalyzer)
INITIALIZE_PASS_END(VectorizationPrereqVerifier, "vectorizationPrereqVerifier", "VectorizationPrereqVerifier", false, true)

// Public interface to the VectorizationPrereqVerifier pass
FunctionPass*
llvm::createVectorizationPrereqVerifierPass()
{
	return new VectorizationPrereqVerifier();
}



VectorizationPrereqVerifier::VectorizationPrereqVerifier()
    : FunctionPass(ID), mInfo(nullptr), mLoopInfo(nullptr), mSCCAnalyzer(nullptr), mDomTree(nullptr)
{
    initializeVectorizationPrereqVerifierPass(*PassRegistry::getPassRegistry());
}

VectorizationPrereqVerifier::~VectorizationPrereqVerifier()
{
    mInfo = nullptr; // Deleted by PassManager.
}

void
VectorizationPrereqVerifier::releaseMemory()
{
}

void
VectorizationPrereqVerifier::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<WFVInfo>();
    AU.addRequired<LoopInfoWrapperPass>();
    AU.addRequired<DominatorTreeWrapperPass>();
    AU.addRequired<SCCAnalyzer>();

    AU.setPreservesAll();
}

bool
VectorizationPrereqVerifier::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
VectorizationPrereqVerifier::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
VectorizationPrereqVerifier::runOnFunction(Function& F)
{
    mInfo        = &getAnalysis<WFVInfo>();
    mLoopInfo    = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    mDomTree     = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
    mSCCAnalyzer = &getAnalysis<SCCAnalyzer>();

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
VectorizationPrereqVerifier::print(raw_ostream& O, const Module* M) const
{
}

bool
VectorizationPrereqVerifier::verify(const Function& F) const
{
    bool verified = true;

    bool returnFound = false;
    for (auto &BB : F)
    {
        verified &= verify(BB);

        // This should be removed when we have code in place that can handle
        // uniform return blocks. Then, we either unify ourselves (if the blocks
        // may be reached by diverged instances), or leave the returns untouched.
        if (isa<ReturnInst>(BB.getTerminator()))
        {
            if (returnFound)
            {
                errs() << "ERROR: Function contains multiple return blocks! ";
                errs() << "Run UnifyFunctionExitNodesPass before WFV!\n";
                verified = false;
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

    // TODO: This describes a "from scratch" way of finding irreducible loops:
    // If we find a block that can be reached via a cycle during forward
    // DFS but it is not marked as a loop header by LLVM, we have found irreducible
    // control flow.
    if (containsIrreducibleLoop(F))
    {
#if 0
        errs() << "ERROR: Function contains irreducible loop with headers "
            << "'" << header1->getName() << "'"
            << " and "
            << "'" << header2->getName() << "'"
            << "! Convert to reducible control flow before running WFV!\n";
#else
        errs() << "ERROR: Function contains irreducible loop! "
            << "Convert to reducible control flow before running WFV!\n";
#endif
        verified = false;
    }

    for (auto &L : *mLoopInfo)
    {
        verified &= verify(*L);
    }

    return verified;
}

bool
VectorizationPrereqVerifier::verify(const BasicBlock& block) const
{
    bool verified = true;

    const TerminatorInst& terminator = *block.getTerminator();

    if (isa<SwitchInst>(terminator))
    {
        errs() << "ERROR: Support for switch statements not implemented yet (#33)!\n";
        errs() << "       Run LowerSwitch pass before WFV!\n";
        verified = false;
    }
    else if (!isa<BranchInst>(terminator) &&
             !isa<ReturnInst>(terminator) &&
             !isa<UnreachableInst>(terminator))
    {
        // indirectbr, invoke, resume
        errs() << "ERROR: Support for terminator statements other than br, ret, and ";
        errs() << "unreachable not implemented yet!\n";
        verified = false;
    }

    // Make sure there is only one edge from one block to another.
    SmallPtrSet<BasicBlock*, 2> succSet;
    for (unsigned i=0, e=terminator.getNumSuccessors(); i<e; ++i)
    {
        BasicBlock* succBB = terminator.getSuccessor(i);
        if (succSet.count(succBB))
        {
            errs() << "ERROR: Multiple redundant edges not allowed (" << block.getName() << " -> "
                    << succBB->getName() << ")!\n";
            verified = false;
        }
        succSet.insert(succBB);
    }

#ifdef WFV_DO_NOT_ALLOW_CRITICAL_EDGES
    for (unsigned i=0, e=terminator.getNumSuccessors(); i<e; ++i)
    {
        if (isCriticalEdge(&terminator, i))
        {
            errs() << "ERROR: Critical edge found (" << block.getName() << " -> "
                    << terminator.getSuccessor(i)->getName() << ")\n";
            errs() << "       Run BreakCriticalEdges pass before WFV!\n";
            verified = false;
        }
    }
#endif

    return verified;
}

bool
VectorizationPrereqVerifier::verify(const Instruction& inst) const
{
    bool verified = true;

    if (isa<FenceInst>(inst) ||
        isa<AtomicCmpXchgInst>(inst) ||
        isa<AtomicRMWInst>(inst) ||
        isa<PtrToIntInst>(inst) ||
        isa<IntToPtrInst>(inst) ||
        isa<VAArgInst>(inst) ||
        isa<LandingPadInst>(inst))
    {
        errs() << "ERROR: Support for instruction not implemented yet: " << inst << "\n";
        verified = false;
    }

    if (const CallInst* call = dyn_cast<CallInst>(&inst))
    {
        const Function* callee = call->getCalledFunction();
        const StringRef str = callee->getName();

        // NOTE: Quite a few of these may actually be unproblematic due to how
        //       we handle unknown functions. Before removal, however, we need
        //       tests for them.
        // TODO: It would be better to test for these intrinsics via Intrinsic::get or so...
        if (str.startswith("llvm.va_start") ||
            str.startswith("llvm.va_copy") ||
            str.startswith("llvm.va_end") ||
            str.startswith("llvm.gcroot") ||
            str.startswith("llvm.gcread") ||
            str.startswith("llvm.gcwrite") ||
            str.startswith("llvm.returnaddress") ||
            str.startswith("llvm.frameaddress") ||
            str.startswith("llvm.stacksave") ||
            str.startswith("llvm.stackrestore") ||
            str.startswith("llvm.prefetch") ||
            str.startswith("llvm.pcmarker") ||
            str.startswith("llvm.readcyclecounter") ||
            //str.startswith("llvm.memcpy") || test suite 2 (test_struct_store04, test_06)
            //str.startswith("llvm.memmove") || // Probably works, but should be handled natively (exploit consecutivity of pointer)
            //str.startswith("llvm.memset") || // Works (TestAOBench_Kernels), but should be handled natively (exploit consecutivity of pointer)
            //str.startswith("llvm.fma") ||                // Works, but should have native mapping.
            str.startswith("llvm.copysign") ||           // Works, but should have native mapping.
            str.startswith("llvm.rint") ||               // Works, but Should have native mapping.
            str.startswith("llvm.nearbyint") ||          // Works, but Should have native mapping.
            str.startswith("llvm.round") ||              // Works, but Should have native mapping.
            str.startswith("llvm.bswap") ||              // Works, but Should have native mapping.
            str.startswith("llvm.ctpop") ||              // Works, but Should have native mapping.
            str.startswith("llvm.ctlz") ||               // Works, but Should have native mapping.
            str.startswith("llvm.cttz") ||               // Works, but Should have native mapping.
            str.startswith("llvm.sadd.with.overflow") ||
            str.startswith("llvm.uadd.with.overflow") ||
            str.startswith("llvm.ssub.with.overflow") ||
            str.startswith("llvm.usub.with.overflow") ||
            str.startswith("llvm.smul.with.overflow") ||
            str.startswith("llvm.umul.with.overflow") ||
            //str.startswith("llvm.fmuladd") ||            // Works, but should have native mapping.
            str.startswith("llvm.convert.to.fp16") ||    // Works, but Should have native mapping.
            str.startswith("llvm.convert.from.fp16") ||  // Works, but Should have native mapping.
            str.startswith("llvm.init.trampoline") ||
            str.startswith("llvm.adjust.trampoline") ||
            str.startswith("llvm.lifetime.start") ||
            str.startswith("llvm.lifetime.end") ||
            str.startswith("llvm.invariant.start") ||
            str.startswith("llvm.invariant.end") ||
            str.startswith("llvm.var.annotation") ||
            str.startswith("llvm.ptr.annotation") ||
            str.startswith("llvm.annotation") ||
            str.startswith("llvm.trap") ||
            str.startswith("llvm.debugtrap") ||
            str.startswith("llvm.stackprotector") ||
            str.startswith("llvm.stackprotectorcheck") ||
            str.startswith("llvm.objectsize") ||
            str.startswith("llvm.expect") ||
            str.startswith("llvm.donothing"))
        {
            errs() << "ERROR: Support for intrinsic call to function '";
            errs() << str << "' not implemented yet!\n";
            verified = false;
        }

        if (str.startswith("llvm.dbg"))
        {
            errs() << "ERROR: Support for debug information not implemented yet!\n";
            verified = false;
        }

        if (str.startswith("llvm.eh"))
        {
            errs() << "ERROR: Support for exception handling not implemented yet!\n";
            verified = false;
        }
    }

    return verified;
}

bool
VectorizationPrereqVerifier::verify(const Loop& loop) const
{
    bool verified = true;

    if (!loop.isLoopSimplifyForm())
    {
        errs() << "ERROR: Loop with header '" << loop.getHeader()->getName()
                << "' is not simplified! Run LoopSimplify pass before WFV!\n";
        verified = false;
    }

    if (!loop.isLCSSAForm(*mDomTree))
    {
        errs() << "ERROR: Loop with header '" << loop.getHeader()->getName()
                << "' is not in loop-closed SSA form! Run LCSSA pass before WFV!\n";
        verified = false;
    }

    SmallVector<BasicBlock*, 2> exitBlocks;
    loop.getExitBlocks(exitBlocks);
    for (const auto &exitBB : exitBlocks)
    {
        if (!exitBB->getUniquePredecessor())
        {
            errs() << "ERROR: Loop with header '" << loop.getHeader()->getName() << "' has "
                << "exits with multiple incoming edges! Run canonicalization pass before WFV!\n";
            verified = false;
        }
    }

    for (auto &SL : loop)
    {
        verified &= verify(*SL);
    }

    return verified;
}

namespace {

bool
containsIrreducibleLoop(const SCCAnalyzer::SCC& scc)
{
    if (scc.getNumHeaders() > 1) return true;

    // Recurse into children.
    for (unsigned i=0, e=scc.getNumChildren(); i<e; ++i)
    {
        if (containsIrreducibleLoop(*scc.getChild(i))) return true;
    }

    return false;
}

} // unnamed namespace

bool
VectorizationPrereqVerifier::containsIrreducibleLoop(const Function& f) const
{
    for (const auto &scc : mSCCAnalyzer->getSCCs())
    {
        if (::containsIrreducibleLoop(*scc)) return true;
    }

    return false;
}
