/**
 * @file   wfvInterface.cpp
 * @date   30.03.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */

#include "wfv/wfvInterface.h"
#include "wfv/utils/metadata.h"
#include "wfv/wfvInfo.h"

// Region based WFV implementation
#include "wfv/wfvRegionBased.h"

// WFV utils
#include "wfv/utils/wfvTools.h"

// WFV passes
#include "wfv/analysis/vectorizationAnalysis.h"
#include "wfv/analysis/vectorizationPrereqVerifier.h"
#include "wfv/analysis/vectorizationVerifier.h"
#include "wfv/analysis/linearizationVerifier.h"
#include "wfv/functionPasses/maskGenerator.h"
#include "wfv/functionPasses/selectGenerator.h"
#include "wfv/functionPasses/cfgLinearizer.h"
#include "wfv/functionPasses/functionVectorizer.h"
#include "wfv/functionPasses/loopExitCanonicalizer.h"

// LLVM stuff
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"          // ReturnInst
#include "llvm/Transforms/Utils/Cloning.h" // CloneFunction
#include "llvm/IR/LegacyPassManager.h"              // FunctionPassManager
#include "llvm/IR/Verifier.h"        // verifyFunction(), createVerifierPass()
#include "llvm/Support/Timer.h"            // TimerGroup

// Preparatory passes
#include "llvm/Transforms/Scalar.h" // createLowerSwitchPass(), createBreakCriticalEdgesPass(), ...
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h" // createUnifyFunctionExitNodesPass()

#include <stdexcept>

#ifndef NDEBUG
#include <sstream>
#endif

using namespace llvm;

////////////////////////////////////////////////////////////////////////////////
// Wrapper functions to use whole function vectorization without
// linking LLVM (only link libWFV) (= implementation of wfvInterface.h)
////////////////////////////////////////////////////////////////////////////////

namespace WFVInterface {

extern int _kronos_debug_level;

WFV_API
WFVInterface::WFVInterface(Module*         module,
                           LLVMContext*    context,
                           Function* scalarFunction,
                           Function*       simdFunction,
                           TargetTransformInfo *TTI,
                           const unsigned  vectorizationFactor,
                           const int       maskPosition,
                           const bool      disableMemAccessAnalysis,
                           const bool      disableControlFlowDivAnalysis,
                           const bool      disableAllAnalyses,
                           const bool      verbose)
: mTimerGroup(new TimerGroup("WFV", "Whole-Function Vectorization")),
        mInfo(new WFVInfo(module,
                          context,
                          scalarFunction,
                          simdFunction,
                          TTI,
                          vectorizationFactor,
                          maskPosition,
                          disableAllAnalyses ? true : disableMemAccessAnalysis,
                          disableAllAnalyses ? true : disableControlFlowDivAnalysis,
                          disableAllAnalyses,
                          verbose,
                          mTimerGroup))
{
    // Set up metadata.
    WFV::setUpMetadata(module);

    // Add function mapping of to-be-vectorized function itself
    // to handle recursive functions.
    // NOTE: We have to conservatively expect the function to have
    //       side effects, so all recursive calls will be vectorized regardless
    //       of their arguments.
    // TODO: Allow mask argument and "mayHaveSideEffects" as input of vectorizer.
    //       This is required for region-based WFV.
    const bool mayHaveSideEffects = true;
    addSIMDMapping(*scalarFunction, *simdFunction, maskPosition, mayHaveSideEffects);
}

WFV_API
WFVInterface::~WFVInterface()
{
    // PassManager deleted mInfo already if run() was called.
    // Otherwise, we need to delete it ourselves.
    if (mInfo)
    {
        mInfo->releaseMemory();
        delete mInfo;
    }

    delete mTimerGroup;
}

WFV_API bool
WFVInterface::addSIMDMapping(const Function& scalarFunction,
                             const Function& simdFunction,
                             const int       maskPosition,
                             const bool      mayHaveSideEffects)
{
    assert (mInfo && "WFVInterface is not initialized correctly");
    return mInfo->addSIMDMapping(scalarFunction,
                                 simdFunction,
                                 maskPosition,
                                 mayHaveSideEffects);
}

WFV_API bool
WFVInterface::addCommonMappings(const bool useSSE,
                                const bool useSSE41,
                                const bool useSSE42,
                                const bool useAVX,
                                const bool useNEON)
{
    assert (mInfo && "WFVInterface is not initialized correctly");
    return mInfo->addCommonMappings(useSSE, useSSE41, useSSE42, useAVX, useNEON);
}


WFV_API bool
WFVInterface::addSIMDSemantics(const Function& f,
                               const bool      isOpUniform,
                               const bool      isOpVarying,
                               const bool      isOpSequential,
                               const bool      isOpSequentialGuarded,
                               const bool      isResultUniform,
                               const bool      isResultVector,
                               const bool      isResultScalars,
                               const bool      isAligned,
                               const bool      isIndexSame,
                               const bool      isIndexConsecutive)
{
    assert (mInfo && "WFVInterface is not initialized correctly");

    // We also map the function to itself so that instruction vectorization
    // "knows" it and replaces it by itself.
    const bool mayHaveSideEffects = !isOpUniform && !isResultUniform;
    addSIMDMapping(f, f, -1, mayHaveSideEffects);

    return mInfo->addSIMDSemantics(f,
                                   isOpUniform,
                                   isOpVarying,
                                   isOpSequential,
                                   isOpSequentialGuarded,
                                   isResultUniform,
                                   isResultVector,
                                   isResultScalars,
                                   isAligned,
                                   isIndexSame,
                                   isIndexConsecutive);
}

WFV_API bool
WFVInterface::addSIMDSemantics(const Argument& arg,
                               const bool      isResultUniform,
                               const bool      isResultVector,
                               const bool      isResultScalars,
                               const bool      isAligned,
                               const bool      isIndexSame,
                               const bool      isIndexConsecutive)
{
    assert (mInfo && "WFVInterface is not initialized correctly");

    return mInfo->addSIMDSemantics(arg,
                                   isResultUniform,
                                   isResultVector,
                                   isResultScalars,
                                   isAligned,
                                   isIndexSame,
                                   isIndexConsecutive);
}

WFV_API bool
WFVInterface::addSIMDSemantics(const GlobalVariable& var,
                               const bool         isOpUniform,
                               const bool         isOpVarying,
                               const bool         isOpSequential,
                               const bool         isOpSequentialGuarded,
                               const bool         isResultUniform,
                               const bool         isResultVector,
                               const bool         isResultScalars,
                               const bool         isAligned,
                               const bool         isIndexSame,
                               const bool         isIndexConsecutive)
{
    assert (mInfo && "WFVInterface is not initialized correctly");

    return mInfo->addSIMDSemantics(var,
                                   isOpUniform,
                                   isOpVarying,
                                   isOpSequential,
                                   isOpSequentialGuarded,
                                   isResultUniform,
                                   isResultVector,
                                   isResultScalars,
                                   isAligned,
                                   isIndexSame,
                                   isIndexConsecutive);
}


WFV_API bool
WFVInterface::addSIMDSemantics(const Instruction& inst,
                               const bool         isOpUniform,
                               const bool         isOpVarying,
                               const bool         isOpSequential,
                               const bool         isOpSequentialGuarded,
                               const bool         isResultUniform,
                               const bool         isResultVector,
                               const bool         isResultScalars,
                               const bool         isAligned,
                               const bool         isIndexSame,
                               const bool         isIndexConsecutive)
{
    assert (mInfo && "WFVInterface is not initialized correctly");

    return mInfo->addSIMDSemantics(inst,
                                   isOpUniform,
                                   isOpVarying,
                                   isOpSequential,
                                   isOpSequentialGuarded,
                                   isResultUniform,
                                   isResultVector,
                                   isResultScalars,
                                   isAligned,
                                   isIndexSame,
                                   isIndexConsecutive);
}

namespace {

void
removeUnusedWFVLibFunctions(Module* mod)
{
#define REMOVE_LIB_FN(name) \
    { \
        Function* fn = mod->getFunction(#name); \
        if (fn && fn->use_empty()) \
        { \
            fn->eraseFromParent(); \
        } \
    } \
    ((void)0)

    REMOVE_LIB_FN(log2_ps);
    REMOVE_LIB_FN(exp2_ps);
    REMOVE_LIB_FN(log_ps);
    REMOVE_LIB_FN(exp_ps);
    REMOVE_LIB_FN(sin_ps);
    REMOVE_LIB_FN(cos_ps);
    REMOVE_LIB_FN(sincos_ps);
    REMOVE_LIB_FN(fabs_ps);
    REMOVE_LIB_FN(pow_ps);
    REMOVE_LIB_FN(log2256_ps);
    REMOVE_LIB_FN(exp2256_ps);
    REMOVE_LIB_FN(log256_ps);
    REMOVE_LIB_FN(exp256_ps);
    REMOVE_LIB_FN(sin256_ps);
    REMOVE_LIB_FN(cos256_ps);
    REMOVE_LIB_FN(sincos256_ps);
    REMOVE_LIB_FN(fabs256_ps);
    REMOVE_LIB_FN(pow256_ps);

#undef REMOVE_LIB_FN
}

void
removeTempFunction(Module* mod, const std::string& name)
{
    assert (mod);
    if (Function* tmpFn = mod->getFunction(name))
    {
        assert (tmpFn->use_empty());
        tmpFn->eraseFromParent();
    }
}

}



// Create a PassManager and run all WFV passes with it.
// The WFVInfo Object is accessible by all phases via
// getAnalysis<WFVInfo>().
WFV_API bool
WFVInterface::run()
{

    if(!mInfo->mInitialized)
        return false;

    assert (mInfo && "WFVInterface is not initialized correctly");

    // Store some of the information mInfo holds for after it is destroyed.
    Module* mod = mInfo->mModule;
    const bool verbose = mInfo->mVerbose;

    const std::string& scalarName = mInfo->mScalarFunction->getName();
    const std::string& targetName = mInfo->mSimdFunction->getName();

#ifndef WFV_SILENT_MODE
    outs() << "\n### Whole-Function Vectorization of function '" << scalarName;
    outs() << "' (width " << mInfo->mVectorizationFactor << ", ";
    if (mInfo->mDisableAllAnalyses) outs() << "all analyses DISABLED)...\n";
    else
    {
        if (mInfo->mDisableControlFlowDivAnalysis && mInfo->mDisableMemAccessAnalysis)
        {
            outs() << "control flow div. analysis DISABLED, mem access analysis DISABLED)...\n";
        }
        else if (mInfo->mDisableControlFlowDivAnalysis)
        {
            outs() << "control flow div. analysis DISABLED)...\n";
        }
        else if (mInfo->mDisableMemAccessAnalysis)
        {
            outs() << "mem access analysis DISABLED)...\n";
        }
        else
        {
            outs() << "all analyses ENABLED)...\n";
        }
    }
#endif

    bool success = false;
    success = vectorizeFunction();
    if (!success)
    {
        // Something went wrong -> Rollback.
        Function* finalFn = mod->getFunction(targetName);
        if (finalFn && !finalFn->empty())
        {
            finalFn->deleteBody();
            assert (finalFn->empty());
        }
    }


    if (success)
    {
        Function* finalFn = mod->getFunction(targetName);
        assert (finalFn);
        assert (!finalFn->isDeclaration());
        WFV::writeModuleToFile(*mod, "vectorized.mod.ll");
        if(verbose)
            WFV::writeFunctionToFile(*finalFn, "vectorized.ll");
        WFV::removeAllMetadata(finalFn);
    }


    // Remove all functions that were linked in but are not used.
    // TODO: This is a very bad temporary hack to get the "noise" project
    //       running. We should add functions lazily.
    removeUnusedWFVLibFunctions(mod);

    // Remove temporary functions if inserted during mask generation.
    removeTempFunction(mod, "entryMaskUseFn");
    removeTempFunction(mod, "entryMaskUseFnSIMD");

    if (verbose) mTimerGroup->print(outs());

#ifndef WFV_SILENT_MODE
    outs() << "### Whole-Function Vectorization of function '" << scalarName
            << (success ? "' SUCCESSFUL!\n" : "' FAILED!\n");
#endif

    return success;
}

bool
WFVInterface::analyzeFunction(Function* scalarFn, Function* simdFn)
{
    assert (scalarFn && simdFn);
    assert (simdFn == mInfo->mSimdFunction);

    // If the function was already analyzed, do nothing.
    if (mInfo->mIsAnalyzed)
    {
        if(mInfo->mVerbose) outs() << "Function is already analyzed, skipping analyzeFunction().\n";
        return WFV::VerifyVectorizationAnalysis(*scalarFn);
    }


// FIXME only in debug builds
// Assign names to unnamed blocks
#ifndef NDEBUG
    unsigned anonBlockId = 0;
    for (auto & block : *scalarFn) {
    	if (block.getName().empty()) {
    		std::stringstream ss;
    		ss << "b" << anonBlockId;
    		block.setName(ss.str());
    		++anonBlockId;
    	}
    }
#endif
    // Create copy of WFVInfo to allow run() to be called after analyze().
    WFVInfo* copyInfo = new WFVInfo(*mInfo);

    bool failure = false;
    mInfo->mFailure = &failure;

    {
        legacy::FunctionPassManager FPM(mInfo->mModule);

        FPM.add(mInfo);

        // Canonicalize loop exits such that every exit block has exactly
        // one predecessor (not guaranteed by loop simplification!).
        FPM.add(createLoopExitCanonicalizerPass());

        // Breaking critical edges improves the results of CFG linearization,
        FPM.add(createBreakCriticalEdgesPass());

        // Run first verification phase to check properties that
        // are required for the vectorization analysis.
        FPM.add(createVectorizationPrereqVerifierPass());

        FPM.add(createVectorizationAnalysisPass(scalarFn,
                                                simdFn,
                                                mInfo->mVectorizationFactor,
                                                mInfo->mMaskPosition,
                                                mInfo->mTTI,
                                                &mInfo->mValueInfoMap,
                                                &mInfo->mFunctionInfoMap,
                                                mInfo->mDisableMemAccessAnalysis,
                                                mInfo->mDisableControlFlowDivAnalysis,
                                                mInfo->mDisableAllAnalyses,
                                                mInfo->mVerbose,
                                                mInfo->mFailure));

        // Run second verification phase that uses analysis results.
        FPM.add(createVectorizationVerifierPass());

        FPM.run(*scalarFn);
    }

    if(mInfo->mVerbose) {
        outs() << "after analysis";
        scalarFn->dump();
    }

    // Restore WFVInfo (deleted by PassManager);
    mInfo = copyInfo;

    // If there was a failure in any of the passes, the failure-flag is set.
    // Otherwise, mark the function as "analyzed".
    if (failure)
    {
        errs() << "ERROR: analyses failed!\n";
    }
    else
    {
        mInfo->mIsAnalyzed = true;
    }

    return !failure && WFV::VerifyVectorizationAnalysis(*scalarFn);
}

#ifndef WFV_ENABLE_OPTIONAL_BELOW_MANDATORY_LOOP_EXITS
namespace {

// Helper function. To be removed in the future (see comment in
// CFGLinearizer::getOutgoingEdgeType()).
bool
isOptionalAboveAllMandatoryLoopExits(const BasicBlock& exitBlock,
                                     const LoopInfo&   loopInfo)
{
    assert (WFV::hasMetadata(&exitBlock, WFV::WFV_METADATA_OPTIONAL));
    assert (WFV::isExitOfDivergentLoop(exitBlock, loopInfo));

    const BasicBlock* predBB = exitBlock.getUniquePredecessor();
    const Loop* loop = loopInfo.getLoopFor(predBB);

    // This does not work if some edges have already been removed!
    SmallVector<BasicBlock*, 2> exitBlocks;
    loop->getExitBlocks(exitBlocks);

    // Iterate over all MANDATORY exits and check if predBB is "below" it.
    // We say an exit E1 is "above" another exit E2, if the exiting block of E2
    // is reachable from the exiting block of E1.
    // TODO: This means we currently conservatively treat "neighboring" exits
    //       as "below" each other. To improve this, we would have to prove that
    //       both exits can only be reached on disjoint, fully uniform paths from
    //       the loop header. In this case, we could treat both exits as "above"
    //       each other.
    for (const auto &exitBB : exitBlocks)
    {
        if (!WFV::hasMetadata(exitBB, WFV::WFV_METADATA_MANDATORY)) continue;
        assert (&exitBlock != exitBB);

        const BasicBlock* exitingBB = exitBB->getUniquePredecessor();
        assert (exitingBB);

        // If we can't reach exitingBB from predBB, we can not be sure
        // that the current exit is above this one.
        if (!WFV::isReachable(exitingBB, predBB, nullptr, loop, loop)) return false;
    }

    return true;
}

void
addNewMandatoryBlockAsRewireTarget(BasicBlock* exitBlock,
                                   const LoopInfo& loopInfo)
{
    assert (WFV::hasMetadata(exitBlock, WFV::WFV_METADATA_MANDATORY));
    assert (WFV::isExitOfDivergentLoop(*exitBlock, loopInfo));

    BasicBlock* predBB = exitBlock->getUniquePredecessor();
    Loop* loop = loopInfo.getLoopFor(predBB);

    // This does not work if some edges have already been removed!
    SmallVector<BasicBlock*, 2> exitBlocks;
    loop->getExitBlocks(exitBlocks);

    // Iterate over all MANDATORY exits and check if predBB is "below" it.
    // We say an exit E1 is "above" another exit E2, if the exiting block of E2
    // is reachable from the exiting block of E1.
    // TODO: This means we currently conservatively treat "neighboring" exits
    //       as "below" each other. To improve this, we would have to prove that
    //       both exits can only be reached on disjoint, fully uniform paths from
    //       the loop header. In this case, we could treat both exits as "above"
    //       each other.
    SmallPtrSet<BasicBlock*, 2> upperExits;
    for (auto &exitBB : exitBlocks)
    {
        if (exitBlock == exitBB) continue;
        if (!WFV::hasMetadata(exitBB, WFV::WFV_METADATA_MANDATORY)) continue;

        BasicBlock* exitingBB = exitBB->getUniquePredecessor();
        assert (exitingBB);

        // If we can't reach exitingBB from predBB, we can not be sure
        // that the current exit is above this one.
        if (WFV::isReachable(exitingBB, predBB, nullptr, loop, loop)) continue;
        upperExits.insert(exitBB);
    }

    for (auto &dcBB : loop->getBlocks())
    {
        if (!WFV::hasMetadata(dcBB->getTerminator(), WFV::WFV_METADATA_OP_VARYING)) continue;
        SmallVector<Value*, 2> rewireTargets;
        WFV::getRewireTargetsOfDCBlock(*dcBB, rewireTargets);
        for (auto RT : rewireTargets)
        {
            assert (RT != exitBlock);
            if (upperExits.count(cast<BasicBlock>(RT)))
            {
                //outs() << "  Added block '" << exitBlock->getName();
                //outs() << "' as rewire target to '";
                //outs() << dcBB->getName() << "'.\n";
                WFV::addRewireTargetForDCBlock(dcBB, exitBlock);
                break;
            }
        }
    }
}

} // unnamed namespace
#endif

bool
WFVInterface::vectorizeFunction()
{
    assert (mInfo);
    assert (mInfo->mScalarFunction);
    assert (mInfo->mSimdFunction);
    Function&    scalarFunction = *mInfo->mScalarFunction;
    const Function&    simdFunction   = *mInfo->mSimdFunction;
    const std::string& scalarName     = scalarFunction.getName();
    const std::string& simdName       = simdFunction.getName();

    if (scalarFunction.isVarArg())
    {
        errs() << "ERROR while vectorizing function in module '"
                << mInfo->mModule->getModuleIdentifier() << "': function '"
                << scalarName << "' has a variable argument list (not supported)!\n";
        return false;
    }

    if (scalarFunction.isDeclaration())
    {
        errs() << "ERROR while vectorizing function in module '"
                << mInfo->mModule->getModuleIdentifier() << "': scalar source function '"
                << scalarName << "' has no body!\n";
        return false;
    }

    if (!simdFunction.isDeclaration())
    {
        assert (!simdFunction.getBasicBlockList().empty() &&
                "Function is no declaration but does not have basic blocks?!");
        errs() << "ERROR while vectorizing function in module '"
                << mInfo->mModule->getModuleIdentifier() << "': extern target function '"
                << simdName << "' must not have a body!\n";
        return false;
    }

    WFV::writeFunctionToFile(scalarFunction, "scalar.ll");
    if(mInfo->mVerbose) {
        scalarFunction.print(outs());
        WFV::writeFunctionToFile(scalarFunction, "scalar.ll");
        WFV::writeModuleToFile(*mInfo->mModule, "scalar.mod.ll");
        verifyFunction(scalarFunction);
    }

    Function* tempF = nullptr;
    {
        // Don't touch original function... clone it.
        ValueToValueMapTy valueMap;
        tempF = CloneFunction(&scalarFunction, valueMap);

        assert (tempF);
        tempF->setCallingConv(scalarFunction.getCallingConv());
        tempF->setAttributes(scalarFunction.getAttributes());
        tempF->setAlignment(scalarFunction.getAlignment());
        tempF->setLinkage(GlobalValue::InternalLinkage);
        tempF->setName(scalarFunction.getName()+".wfv.tmp");

        // Map all user-defined uniform/consecutive/aligned values
        // from the original scalar source function to the function
        // that we will be working on (tempF).
        mInfo->mValueInfoMap.mapValueInformation(valueMap);
    }

    if(mInfo->mVerbose) {
        tempF->print(outs());
        simdFunction.print(outs());
    }

    if (!verifyFunctionSignaturesMatch(*tempF, simdFunction))
    {
        errs() << "ERROR: Function signatures do not match!\n";
        errs() << "       scalar    : " << *tempF->getType() << "\n";
        errs() << "       vectorized: " << *simdFunction.getType() << "\n";
        return false;
    }

    DEBUG_WFV_NO_VERBOSE( verifyFunction(*tempF); );


    bool failure = false;
    mInfo->mFailure = &failure;

    //
    // Perform vectorization analysis of current function.
    //
    const bool analyzed = analyzeFunction(tempF, mInfo->mSimdFunction);
    if (!analyzed) {
        tempF->eraseFromParent();
        return false;
    }

#ifndef WFV_ENABLE_OPTIONAL_BELOW_MANDATORY_LOOP_EXITS
    //
    // We currently do not optimize OPTIONAL exits "below" MANDATORY ones.
    // Since we still want the vectorization analysis to return the best
    // possible result, we only modify its results here, directly before
    // vectorization.
    //
    {
        DominatorTreeBase<BasicBlock>* DTB = new DominatorTreeBase<BasicBlock>(false);
        DTB->recalculate(*tempF);
        LoopInfo loopInfo;
        loopInfo.analyze(*DTB);
        for (auto &BB : *tempF)
        {
            if (WFV::getNumIncomingEdges(BB) == 0) continue;

            if (WFV::hasMetadata(&BB, WFV::WFV_METADATA_OPTIONAL) &&
                WFV::isExitOfDivergentLoop(BB, loopInfo) &&
                !isOptionalAboveAllMandatoryLoopExits(BB, loopInfo))
            {
                // FIXME: This is a really ugly hack, but easier and more local than
                //        a modification of the all-false-branch generation code.
                errs() << "WARNING: Relabeling block '" << BB.getName() << "'";
                errs() << " from OPTIONAL to MANDATORY for vectorization";
                errs() << " (optimization of OPTIONAL exits \"below\" MANDATORY ones";
                errs() << " not implemented).\n";
                WFV::setMetadata(&BB, WFV::WFV_METADATA_MANDATORY);

                // Take the dc blocks that caused the other exits to be rewire targets
                // for this one as well.
                addNewMandatoryBlockAsRewireTarget(&BB, loopInfo);
            }
        }
        delete DTB;
    }
#endif


    // TODO: Implement region-based WFV:
    //
    // General:
    // - Add some API to mark a region from outside, e.g. with "disable vect" or "pumped compaction"
    // - Implement heuristics to automatically add marks where appropriate.
    // - The marks should be metadata at begin/end(s) of the region
    // Here:
    // - Run vectorization analysis
    // - Extract top level regions of current function
    // - For each region R {
    //   - For each mark of R, create a clone V
    //   - For each V {
    //     - Create SIMD target using results of current vectorization analysis
    //     - Use addSIMDSemantics() etc. to add custom marks (if required)
    //     - Run WFV to create V_SIMD (-> recurse) (if required, not necessary e.g. for
    //       "disable vect")
    //     }
    //   - Perform region-specific transformations, e.g. duplicate call to V_SIMD W times for
    //     "disable-vect", or insert dynamic test, branch, and calls to V_SIMD in then/else part
    //   }

#if 0
    // TODO: This is a hack to have the region code below do something useful
    //       for a single varying branch for testing purposes.
    DominatorTreeBase<BasicBlock>* DTB = new DominatorTreeBase<BasicBlock>(false);
    DominatorTreeBase<BasicBlock>* PTB = new DominatorTreeBase<BasicBlock>(true);
    DTB->recalculate(*tempF);
    PTB->recalculate(*tempF);
    for (auto &BB : *tempF)
    {
        TerminatorInst* terminator = BB.getTerminator();
        if (!isa<BranchInst>(terminator)) continue;
        BranchInst* br = cast<BranchInst>(terminator);
        if (br->isUnconditional()) continue;
        if (WFV::hasMetadata(br, WFV::WFV_METADATA_OP_UNIFORM)) continue;

        // Determine the variant region.
        BasicBlock* parentBB = br->getParent();
        BasicBlock* curStartBB = parentBB;
        BasicBlock* endBB      = parentBB;

        bool seseFound = false;
        while (!seseFound)
        {
            curStartBB = endBB;
            DomTreeNode* pdbNode = PTB->getNode(curStartBB);
            endBB = pdbNode->getIDom()->getBlock();

            assert (PTB->properlyDominates(endBB, curStartBB));

            seseFound = DTB->properlyDominates(curStartBB, endBB);
        }

        assert (endBB != parentBB);

        outs() << "Variant region created: " << parentBB->getName()
            << " -> " << endBB->getName() << "\n";

        // Set branch as start of the variant region.
        WFV::setMetadata(br, WFV::WFV_METADATA_VARIANT_START);
        //WFV::setMetadata(br, WFV::WFV_METADATA_VARIANT_DISABLE_VECT);
        WFV::setMetadata(br, WFV::WFV_METADATA_VARIANT_SEQUENTIALIZE);

        // Set first non-phi of end block as end of the variant region.
        Instruction* endInst = endBB->getFirstNonPHI();
        WFV::setMetadata(endInst, WFV::WFV_METADATA_VARIANT_END);
        //WFV::setMetadata(endInst, WFV::WFV_METADATA_VARIANT_DISABLE_VECT);
        WFV::setMetadata(endInst, WFV::WFV_METADATA_VARIANT_SEQUENTIALIZE);

        outs() << "  start: " << *br << "\n";
        outs() << "  end  : " << *endInst << "\n";

        break;
    }

    delete DTB;
    delete PTB;
#endif

    //
    // Extract regions.
    //
    SmallVector<Function*, 2> regionFns;
    SmallVector<SetVector<Value*>*, 2> inputSets;
    SmallVector<SetVector<Value*>*, 2> outputSets;
    bool regionSuccess = extractTopLevelVariantRegions(tempF, regionFns, inputSets, outputSets);

    if(!regionSuccess)
        return false;

    if(mInfo->mVerbose) {
        outs() << "Variant regions found:\n";
        for (auto &F : regionFns) {
            outs() << " * " << F->getName() << "\n";
        }
    }

    DEBUG_WFV_NO_VERBOSE( verifyFunction(*tempF); );

    // To add a new variant, do the following:
    // - Add metadata for the variant to metadata.h/.cpp,
    //   - Adapt WFV::getVariantMetadata().
    // - Implement class that is derived from VariantRegion.
    //   - Make sure it has a unique ID!
    // - Register class in VariantRegionFactory constructor.
    // - Modify getVariantRegionType() (variantRegion.cpp).
    SmallVector<WFV::RegionBased::VariantRegion*, 2> variantRegions;

    for (unsigned i=0, e=regionFns.size(); i<e; ++i)
    {
        Function* R = regionFns[i];
        SetVector<Value*>* inputs = inputSets[i];
        SetVector<Value*>* outputs = outputSets[i];

        //
        // Determine properties of this region.
        //
        WFV::RegionBased::VariantRegion* VR =
            getVariantRegion(tempF, R, inputs, outputs, mInfo);

        variantRegions.push_back(VR);
    }

    //
    // Run variant transformations.
    //
    for (auto &VR : variantRegions)
    {
        const bool success = VR->run();
        if(!success) {
            errs() << "ERROR: Variant generation failed.";
            return false;
        }

        if(mInfo->mVerbose)
            if (VR->verify())
                outs() << "Variant verified!\n";
    }

    DEBUG_WFV_NO_VERBOSE( verifyFunction(*tempF); );

    // Now that all nested regions have been treated, vectorize the
    // current function.

    // Metadata may have been teared down by recursive vectorization, set it up again.
    WFV::setUpMetadata(mInfo->mModule);

    {
        // NOTE: Loop live value information is invalidated after select generation!
        // NOTE: loopInfo, PDT, and DT do not match tempF anymore after cfgLinearization!
        // NOTE: Passes must not be deleted manually,
        //       this is done by FPM (including calls to releaseMemory()).

        legacy::FunctionPassManager FPM(mInfo->mModule);

        FPM.add(mInfo);
        FPM.add(createMaskGeneratorPass());
        FPM.add(createSelectGeneratorPass());
        FPM.add(createCFGLinearizerPass());

        // Run third verification phase that verifies the linearized code.
        FPM.add(createLinearizationVerifierPass());

        FPM.add(createFunctionVectorizerPass());

        FPM.run(*tempF);
        //WFV::writeFunctionToFile(*tempF, "mask.ll");
    }
    mInfo = nullptr; // PassManager has deleted mWFVInfo already.

    // Clean up temporary function.
    tempF->eraseFromParent();

    //
    // Merge variants back into the main function.
    //
    for (auto &VR : variantRegions)
    {
        const bool success = VR->merge();
        if(!success) {
            errs() << "ERROR: Variant merging failed.";
            return false;
        }
    }

    // Clean up variants.
    for (auto &VR : variantRegions) delete VR;
    variantRegions.clear();

    return !failure;
}


bool
WFVInterface::verifyFunctionSignaturesMatch(const Function& f,
                                            const Function& f_SIMD)
{
    bool verified = true;

    if (f.arg_size() != f_SIMD.arg_size())
    {
        errs() << "ERROR: number of function arguments does not match!\n";
        verified = false;
    }

    // check argument and return types
    Type* scalarReturnType      = f.getReturnType();
    Type* foundVectorReturnType = f_SIMD.getReturnType();

    if (!verifyVectorizedType(scalarReturnType, foundVectorReturnType))
    {
        errs() << "ERROR: return type does not match!\n";
        errs() << "       scalar      : " << *scalarReturnType << "\n";
        errs() << "       vec found   : " << *foundVectorReturnType << "\n";
        verified = false;
    }

    for (Function::const_arg_iterator A=f.arg_begin(), extA=f_SIMD.arg_begin();
         A!=f.arg_end() && extA!=f_SIMD.arg_end(); ++A, ++extA)
    {
        Type* scalarType = A->getType();
        Type* foundVectorType = extA->getType();

        if (!verifyVectorizedType(scalarType, foundVectorType))
        {
            errs() << "ERROR: argument type does not match: " << *A << "\n";
            errs() << "       scalar      : " << *scalarType << "\n";
            errs() << "       vec found   : " << *foundVectorType << "\n";
            verified = false;
        }
    }

    return verified;
}

bool
WFVInterface::verifyVectorizedType(Type* scalarType, Type* vecType)
{
    // Check for uniform equivalence.
    if (scalarType == vecType) return true;
    if (WFV::typesMatch(scalarType, vecType)) return true;

    // Check for varying equivalence.
    Type* vectorizedType = WFV::vectorizeSIMDType(scalarType, mInfo->mVectorizationFactor);
    if (WFV::typesMatch(vecType, vectorizedType)) return true;

    return false;
}

// Analyze the code.
// This function does not alter the code except that it introduces
// dummy-calls at the start of every every basic block.
// The function afterwards holds metadata for every instruction and
// for every block (in the dummy call).
// The dummy calls are marked as "nounwind" and "readnone" such that
// they should be removed automatically by optimization phases.
bool
WFVInterface::analyze()
{
    assert (mInfo && "WFVInterface is not initialized correctly");
    assert (mInfo->mScalarFunction);
    assert (mInfo->mSimdFunction);
    const Function&    scalarFunction = *mInfo->mScalarFunction;
    const Function&    simdFunction   = *mInfo->mSimdFunction;
    const std::string& scalarName     = scalarFunction.getName();
    const std::string& simdName       = simdFunction.getName();

    if (scalarFunction.isVarArg())
    {
        errs() << "ERROR while analyzing function in module '"
                << mInfo->mModule->getModuleIdentifier() << "': function '"
                << scalarName << "' has a variable argument list (not supported)!\n";
        return false;
    }

    if (scalarFunction.isDeclaration())
    {
        errs() << "ERROR while analyzing function in module '"
                << mInfo->mModule->getModuleIdentifier() << "': scalar source function '"
                << scalarName << "' has no body!\n";
        return false;
    }

    if (!simdFunction.isDeclaration())
    {
        assert (!simdFunction.getBasicBlockList().empty() &&
                "Function is no declaration but does not have basic blocks?!");
        errs() << "ERROR while analyzing function in module '"
                << mInfo->mModule->getModuleIdentifier() << "': extern target function '"
                << simdName << "' must not have a body!\n";
        return false;
    }

    DEBUG_WFV_NO_VERBOSE( verifyFunction(scalarFunction); );

    if (!verifyFunctionSignaturesMatch(scalarFunction, simdFunction))
    {
        errs() << "ERROR: Function signatures do not match!\n";
        errs() << "       scalar    : " << *scalarFunction.getType() << "\n";
        errs() << "       vectorized: " << *simdFunction.getType() << "\n";
        return false;
    }

    return analyzeFunction(&const_cast<Function&>(scalarFunction), mInfo->mSimdFunction);
}

bool
WFVInterface::clearAnalysisMetadata(Function* f)
{
    assert (f);
    WFV::removeAllMetadata(f);
    return true;
}

} // namespace WFVInterface
