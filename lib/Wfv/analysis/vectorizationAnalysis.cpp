/**
 * @file   vectorizationAnalysis.cpp
 * @date   28.03.2012, 19.02.2015
 * @author Ralf Karrenberg, Simon Moll
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */

#include "wfv/analysis/vectorizationAnalysis.h"
#include "wfv/wfvConfig.h"

#include "wfv/utils/wfvTools.h"
#include "wfv/utils/metadata.h"
#include "wfv/utils/valueInfoMap.h"
#include "wfv/utils/functionInfoMap.h"

#include "llvm/IR/Instruction.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Constants.h" // ConstantInt
#include "llvm/Analysis/CFG.h" // GraphTraits
#include "llvm/IR/InstIterator.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/IR/Verifier.h" // verifyFunction()

#include "llvm/Support/raw_os_ostream.h"

#include "wfv/analysis/SketchGraph.h"
#include "wfv/analysis/PathFinder.h"
#include <stdexcept>

using namespace llvm;


char VectorizationAnalysis::ID = 0;
INITIALIZE_PASS_BEGIN(VectorizationAnalysis, "vectorizationAnalysis", "Vectorization Analysis", false, true)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
//INITIALIZE_PASS_DEPENDENCY(LoopSimplify)
INITIALIZE_PASS_END(VectorizationAnalysis, "vectorizationAnalysis", "Vectorization Analysis", false, true)

// Public interface to the VectorizationAnalysis pass
FunctionPass*
llvm::createVectorizationAnalysisPass(Function*                   scalarFn,
                                      const Function*             simdFn,
                                      const unsigned              vectorizationFactor,
                                      const int                   maskPosition,
                                      TargetTransformInfo         *TTI,
                                      const WFV::ValueInfoMap*    valueInfoMap,
                                      const WFV::FunctionInfoMap* functionInfoMap,
                                      const bool                  disableMemAccessAnalysis,
                                      const bool                  disableControlFlowDivAnalysis,
                                      const bool                  disableAllAnalyses,
                                      const bool                  verbose,
                                      bool*                       failure)
{
    return new VectorizationAnalysis(scalarFn,
                                     simdFn,
                                     vectorizationFactor,
                                     maskPosition,
                                     TTI,
                                     valueInfoMap,
                                     functionInfoMap,
                                     disableMemAccessAnalysis,
                                     disableControlFlowDivAnalysis,
                                     disableAllAnalyses,
                                     verbose,
                                     failure);
}

VectorizationAnalysis::VectorizationAnalysis()
: FunctionPass(ID),
        mScalarFunction(nullptr),
        mSimdFunction(nullptr),
        mMaskPosition(-1),
        mTTI(nullptr),
        mValueInfoMap(nullptr),
        mFunctionInfoMap(nullptr),
        mDisableMemAccessAnalysis(false),
        mDisableControlFlowDivAnalysis(false),
        mDisableAllAnalyses(false),
        mVerbose(false),
        mFailure(nullptr),
        mVectorizationFactor(0)
{
    assert (false &&
            "must never call default constructor - check getAnalysisUsage() functions!");
}

VectorizationAnalysis::VectorizationAnalysis(Function*                   scalarFn,
                                             const Function*             simdFn,
                                             const unsigned              vectorizationFactor,
                                             const int                   maskPosition,
                                             TargetTransformInfo         *TTI,
                                             const WFV::ValueInfoMap*    valueInfoMap,
                                             const WFV::FunctionInfoMap* functionInfoMap,
                                             const bool                  disableMemAccessAnalysis,
                                             const bool                  disableControlFlowDivAnalysis,
                                             const bool                  disableAllAnalyses,
                                             const bool                  verbose,
                                             bool*                       failure)
: FunctionPass(ID),
        mScalarFunction(scalarFn),
        mSimdFunction(simdFn),
        mMaskPosition(maskPosition),
        mTTI(TTI),
        mValueInfoMap(valueInfoMap),
        mFunctionInfoMap(functionInfoMap),
        mDisableMemAccessAnalysis(disableMemAccessAnalysis),
        mDisableControlFlowDivAnalysis(disableControlFlowDivAnalysis),
        mDisableAllAnalyses(disableAllAnalyses),
        mVerbose(verbose),
        mFailure(failure),
        mVectorizationFactor(vectorizationFactor)
{
    // WFVInterface should set disableMemAccessAnalysis and disableControlFlowDivAnalysis
    // to 'false' if disableAllAnalyses is set.
    assert ((!mDisableAllAnalyses ||
             (mDisableMemAccessAnalysis && mDisableControlFlowDivAnalysis)) &&
            "expecting all analyses to internally be disabled if 'disableAllAnalyses' is set!");
    assert (scalarFn &&
            "constructor has to be called with valid scalar function!");
    assert (simdFn && "constructor has to be called with valid simd function!");
    assert (vectorizationFactor % 4 == 0 && "vectorizationFactor must be multiple of 4!");
    assert (maskPosition == -1 ||
            (maskPosition >= 0 && maskPosition < (int)simdFn->getFunctionType()->getNumParams()));

    // We can not use "AnalysisPassOnce" here because the test infrastructure
    // calls this for every test case.
    initializeVectorizationAnalysisPass(*PassRegistry::getPassRegistry());
}

VectorizationAnalysis::~VectorizationAnalysis()
{
}

void
VectorizationAnalysis::releaseMemory()
{
}

void
VectorizationAnalysis::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<PostDominatorTreeWrapperPass>();
    AU.addRequired<DominatorTreeWrapperPass>();
    AU.addRequired<LoopInfoWrapperPass>();

    // Fore some reason, the analysis does not have to be required -
    // it works if it is just added to the passmanager...
    //AU.addRequired<VectorizationPrereqVerifier>();

    // 'Unable to schedule' because this is a LoopPass.
    // FIXME: I currently have no idea how we can do this.
    //AU.addRequiredID(LoopSimplifyID);

    AU.setPreservesAll();
}

bool
VectorizationAnalysis::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
VectorizationAnalysis::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
VectorizationAnalysis::runOnFunction(Function& F)
{
    assert (mScalarFunction && "scalar function must be set before runOnFunction() is called!");
    assert (mSimdFunction && "scalar function must be set before runOnFunction() is called!");

    // If an error occurred in a previous pass, abort.
    assert (mFailure);
    if (*mFailure) return false;

    // If there is already vectorization metadata in the function, output a warning.
    if (WFV::hasWFVMetadata(F))
    {
        errs() << "WARNING: Function '" << F.getName() << "' already has"
            << " vectorization metadata associated - if inconsistent, this can lead"
            << " to undefined behavior!\n";
    }

    mLoopInfo    = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    mPostDomTree = &getAnalysis<PostDominatorTreeWrapperPass>().getPostDomTree();
    mDomTree     = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();

    assert (&F == mScalarFunction);

    if(mVerbose) {
        outs() << "\n";
        mValueInfoMap->print(outs());
        outs() << "\n";
    }

    // Find out if the function has a uniform return value.
    const bool hasUniformReturn = WFV::hasUniformReturn(*mScalarFunction, *mSimdFunction);

    // Find out which arguments are uniform and which are varying.
    SmallVector<bool, 4> uniformArgs;
    WFV::findUniformArguments(*mScalarFunction,
                              *mSimdFunction,
                              uniformArgs,
                              mValueInfoMap);

    setUserDefinedMarks(&F, *mValueInfoMap, *mFunctionInfoMap);

    if (mDisableAllAnalyses)
    {
        analyzeNothing(&F, hasUniformReturn);
    }
    else
    {
        analyzeUniformInfo(&F, hasUniformReturn, uniformArgs);

        analyzeConsecutiveAlignedInfo(&F);
    }

    analyzeSplitInfo(&F);

    analyzeMaskInfo(&F);

    if(mVerbose) outs() << F << "\n";

    // Function was not changed.
    return false;
}

void
VectorizationAnalysis::print(raw_ostream& O, const Module* M) const
{
}

void
VectorizationAnalysis::setMarksFromOutside(Value*                   value,
                                           const WFV::ValueInfoMap& valueInfoMap)
{
    const WFV::ValueInfo& info = valueInfoMap.get(*value);

    //
    // OP_UNIFORM / OP_VARYING / OP_SEQUENTIAL / OP_SEQUENTIAL_GUARDED.
    //
    if (info.mIsOpUniform)
    {
        assert (info.mIsResultUniform && "OP_UNIFORM value has to be RESULT_SAME!");
        assert (info.mIsIndexSame && "OP_UNIFORM value has to be INDEX_SAME!");
        assert ((isa<Instruction>(value) || isa<GlobalVariable>(value)) &&
                                                   "Only instructions or global variables can be OP_UNIFORM!");
        markValueAs(value, WFV::WFV_METADATA_OP_UNIFORM);
    }
    else if (info.mIsOpVarying)
    {
        assert (!info.mIsResultUniform && "Only OP_UNIFORM values can be RESULT_SAME!");
        assert (!info.mIsIndexSame && "Only OP_UNIFORM values can be INDEX_SAME!");
        assert (isa<Instruction>(value) && "Only instructions can be OP_VARYING!");
        recursivelyMarkVarying(cast<Instruction>(value), nullptr);
        markValueAs(value, WFV::WFV_METADATA_OP_VARYING);
    }
    else if (info.mIsOpSequential)
    {
        assert (isa<Instruction>(value) && "Only instructions can be OP_SEQUENTIAL!");
        recursivelyMarkVarying(cast<Instruction>(value), nullptr);
        markValueAs(value, WFV::WFV_METADATA_OP_SEQUENTIAL);
    }
    else if (info.mIsOpSequentialGuarded)
    {
        assert (isa<Instruction>(value) && "Only instructions can be OP_SEQUENTIAL_GUARDED!");
        recursivelyMarkVarying(cast<Instruction>(value), nullptr);
        markValueAs(value, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED);
    }

    //
    // RES_UNIFORM / RES_VECTOR.
    //
    if (info.mIsResultUniform)
    {
        markValueAs(value, WFV::WFV_METADATA_RES_UNIFORM);
    }
    else if (info.mIsResultVector)
    {
        markValueAs(value, WFV::WFV_METADATA_RES_VECTOR);
    }
    else if (info.mIsResultScalars)
    {
        assert ((!isa<Instruction>(value) ||
                 (info.mIsOpSequential || info.mIsOpSequentialGuarded)) &&
                "RESULT_SCALARS instruction has to be OP_SEQUENTIAL or OP_SEQUENTIAL_GUARDED!");
        markValueAs(value, WFV::WFV_METADATA_RES_SCALARS);
    }

    // Since arguments may not have OP_* marks, we have to "manually"
    // make sure that all uses are marked as OP_VARYING if the argument
    // is not RES_UNIFORM.
    if (Argument* arg = dyn_cast<Argument>(value))
    {
        if (info.mIsResultVector || info.mIsResultScalars)
        {
            // Recurse into uses (DFS).
            for (Argument::user_iterator U = arg->user_begin(),
                UE = arg->user_end(); U != UE; ++U)
            {
                assert (isa<Instruction>(*U));
                Instruction* useInst = cast<Instruction>(*U);
                recursivelyMarkVarying(useInst, nullptr);
            }
        }
    }

    //
    // INDEX_SAME / INDEX_CONSECUTIVE / INDEX_RANDOM.
    //
    if (info.mIsIndexSame)
    {
        assert (!isa<Instruction>(value) ||
                WFV::hasMetadata(value, WFV::WFV_METADATA_OP_UNIFORM));
        assert (WFV::hasMetadata(value, WFV::WFV_METADATA_RES_UNIFORM));
        markValueAs(value, WFV::WFV_METADATA_INDEX_SAME);
    }
    else if (info.mIsIndexConsecutive)
    {
        assert (!isa<Instruction>(value) ||
                !WFV::hasMetadata(value, WFV::WFV_METADATA_OP_UNIFORM));
        assert (!WFV::hasMetadata(value, WFV::WFV_METADATA_RES_UNIFORM));
        markValueAs(value, WFV::WFV_METADATA_INDEX_CONSECUTIVE);
    }
    else
    {
        assert (!isa<Instruction>(value) ||
                !WFV::hasMetadata(value, WFV::WFV_METADATA_OP_UNIFORM));
        assert (!WFV::hasMetadata(value, WFV::WFV_METADATA_RES_UNIFORM));
        markValueAs(value, WFV::WFV_METADATA_INDEX_RANDOM);
    }

    //
    // ALIGNED_TRUE / ALIGNED_FALSE
    //
    markValueAs(value, info.mIsAligned ?
        WFV::WFV_METADATA_ALIGNED_TRUE :
        WFV::WFV_METADATA_ALIGNED_FALSE);

    return;
}


void
VectorizationAnalysis::setUserDefinedMarks(Function*                   scalarFn,
                                           const WFV::ValueInfoMap&    valueInfoMap,
                                           const WFV::FunctionInfoMap& functionInfoMap)
{
    Module *M = scalarFn->getParent();

    // Mark arguments that are defined as OP_VARYING by the user via
    // addSIMDSemantics() as well as their uses.
    for (Function::arg_iterator A = scalarFn->arg_begin(),
            AE = scalarFn->arg_end(); A != AE; ++A)
    {
        if (!valueInfoMap.hasMapping(*A)) continue;
        setMarksFromOutside(&*A, valueInfoMap);
    }

    //Mark global variables that are defined by the user
    for(auto &global : M->globals()) {
       if(!valueInfoMap.hasMapping(global)) continue;
        setMarksFromOutside(&global, valueInfoMap);
    }

    // Mark instructions that are defined as OP_VARYING by the user via
    // addSMDSemantics() or addFunctionMapping() as well as their uses.
    for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
    {
        Instruction* inst = &*I;

        // 1) Check if this instruction was marked from outside.
        // If it is marked varying update its users
        if (valueInfoMap.hasMapping(*inst))
        {
            setMarksFromOutside(inst, valueInfoMap);
            continue;
        }

        // 2) Check if this is a call to a function that has a mapping.
        // If the user specified a mapping for this call (or the called
        // function) to have no side effects, we may leave it scalar if
        // the arguments allow it (so we initialize it with OP_UNIFORM).
        if (!isa<CallInst>(inst)) continue;
        CallInst* call = cast<CallInst>(inst);

        Function* calledFn = call->getCalledFunction();
        if (!calledFn || !functionInfoMap.hasMapping(*calledFn)) continue;

        // If the function does not have side effects and it is not already
        // marked as OP_VARYING due to some data dependency, we mark it
        // OP_UNIFORM/RES_UNIFORM (for now, may change during fixpoint
        // iteration).
        if (!functionInfoMap.mayHaveSideEffects(*calledFn) &&
            !WFV::hasMetadata(call, WFV::WFV_METADATA_OP_VARYING))
        {
            WFV::setMetadata(call, WFV::WFV_METADATA_OP_UNIFORM);
            WFV::setMetadata(call, WFV::WFV_METADATA_RES_UNIFORM);
        }
        else
        {
            recursivelyMarkVarying(call, nullptr);
        }
    }
}

void
VectorizationAnalysis::analyzeNothing(Function* scalarFn, const bool uniformReturn)
{
    assert (scalarFn);
    // TODO: It should be enough to not mark anything,
    //       this should result in the most conservative
    //       result, which is to expect all instructions
    //       to be OP_VARYING, all blocks to be MANDATORY,
    //       and all loops to be LOOP_DIVERGENT (#20).
    //return;

    for (Function::arg_iterator A=scalarFn->arg_begin(),
         AE=scalarFn->arg_end(); A!=AE; ++A)
    {
        if (mValueInfoMap->hasMapping(*A))
        {
            continue;
        }

        if (A->getType()->isPointerTy())
        {
            WFV::setMetadata(&*A, WFV::WFV_METADATA_RES_SCALARS);
        }
        else
        {
            WFV::setMetadata(&*A, WFV::WFV_METADATA_RES_VECTOR);
        }

        WFV::setMetadata(&*A, WFV::WFV_METADATA_INDEX_RANDOM);
        WFV::setMetadata(&*A, WFV::WFV_METADATA_ALIGNED_FALSE);
    }

    for (auto &BB : *scalarFn)
    {
        // Mark all instructions as OP_VARYING/RES_VECTOR
        // except for unconditional branches and void-returns.
        for (auto &I : BB)
        {
            // Ignore our own metadata calls.
            if (WFV::isMetadataCall(&I)) continue;

            if (BranchInst* br = dyn_cast<BranchInst>(&I))
            {
                if (br->isUnconditional())
                {
                    markValueAs(&I, WFV::WFV_METADATA_OP_UNIFORM);
                    continue;
                }
            }
            else if (ReturnInst* ret = dyn_cast<ReturnInst>(&I))
            {
                if (!ret->getReturnValue() || uniformReturn)
                {
                    markValueAs(&I, WFV::WFV_METADATA_OP_UNIFORM); // TODO: Why not also RES_UNIFORM?
                    continue;
                }
            }

            if (mValueInfoMap->hasMapping(I))
            {
                continue;
            }

            markValueAs(&I, WFV::WFV_METADATA_OP_VARYING);
            markValueAs(&I, WFV::WFV_METADATA_RES_VECTOR);
            markValueAs(&I, WFV::WFV_METADATA_INDEX_RANDOM);
            markValueAs(&I, WFV::WFV_METADATA_ALIGNED_FALSE);
        }

        // Mark all loops as LOOP_DIVERGENT.
        if (mLoopInfo->isLoopHeader(&BB))
        {
            markValueAs(&BB, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE);
        }
    }

    // Mark DIVERGENT blocks.
    // We can't just mark each non-header block with more than one incoming edge
    // as DIVERGENT since we also need the divergence-causing blocks.
#ifdef WFV_ENABLE_LEGACY_API
    findReachableEdges(scalarFn);
#endif
    markDivergentBlocks(scalarFn);

    // Mark MANDATORY blocks.
    // Similiarly, we can't just mark blocks MANDATORY since we also need the
    // rewire information.
    markMandatoryBlocks(scalarFn);

    // Mark special DIVERGENT loops.
    markNestedDivergentTopLevelLoops();
}

// Mark instructions as OP_UNIFORM or OP_VARYING.
// Mark results of instructions as RES_UNIFORM or RES_VECTOR.
// Mark blocks as MANDATORY or OPTIONAL.
// Mark loops as LOOP_DIVERGENT or LOOP_NON_DIVERGENT.
// Mark of phis depends on mark of parent block (fixpoint iteration required).
// Mark of result of instruction depends on mark of parent loop if
//   it is live across loop boundaries (LALB) (fixpoint iteration required).
void
VectorizationAnalysis::analyzeUniformInfo(Function*                   scalarFn,
                                          const bool                  uniformReturn,
                                          const SmallVector<bool, 4>& uniformArgs)
{
    assert (scalarFn);

    // Start at arguments and user-defined functions,
    // mark all non-UNIFORM instructions as OP_VARYING.
    // Which ones actually are SEQUENTIAL/SEQUENTIAL_GUARDED
    // is analyzed & refined later.

    // Mark instructions that depend on varying arguments as well as their uses.
    // (Only required once, thus not in fixpoint iteration).
    unsigned i=0;
    for (Function::arg_iterator A=scalarFn->arg_begin(),
            AE=scalarFn->arg_end(); A!=AE; ++A, ++i)
    {
        if (mValueInfoMap->hasMapping(*A))
        {
            continue;
        }

        if (uniformArgs[i])
        {
            WFV::setMetadata(&*A, WFV::WFV_METADATA_RES_UNIFORM);
            assert (WFV::hasMetadata(&*A, WFV::WFV_METADATA_RES_UNIFORM));
            continue;
        }

        WFV::setMetadata(&*A, WFV::WFV_METADATA_RES_VECTOR);
        assert (WFV::hasMetadata(&*A, WFV::WFV_METADATA_RES_VECTOR));

        if(mVerbose) outs() << "\nmarking uses of argument: " << *A << " as OP_VARYING...\n";

        // Recurse into uses (DFS).
        for (Instruction::user_iterator U = A->user_begin(),
                UE = A->user_end(); U != UE; ++U)
        {
            assert (isa<Instruction>(*U));
            Instruction* userInst = cast<Instruction>(*U);
            recursivelyMarkVarying(userInst, nullptr);
        }
    }

    // TODO: What about non-UNIFORM pointer parameters of
    //       user-defined functions? Is that an issue?
    //       Recurse into operands (as done in wfv1)?

    // Mark unconditional branches and void-returns as UNIFORM.
    for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
    {
        Instruction* inst = &*I;
        if (!isa<BranchInst>(inst) && !isa<ReturnInst>(inst)) continue;

        if (BranchInst* br = dyn_cast<BranchInst>(inst))
        {
            if (br->isUnconditional()) markValueAs(br, WFV::WFV_METADATA_OP_UNIFORM);
        }
        else
        {
            ReturnInst* ret = cast<ReturnInst>(inst);
            if (!ret->getReturnValue())
            {
                markValueAs(ret, WFV::WFV_METADATA_OP_UNIFORM);
            }
#if 1
            else if (!uniformReturn)
            {
                // Make sure that return instructions are properly marked as
                // OP_VARYING / RES_VECTOR if the target function requires it.
                // This is necessary for kernels that have a declaration that requires
                // a vector but the actual returned value is known to be uniform.
                markValueAs(ret, WFV::WFV_METADATA_OP_VARYING); // TODO: Why not also RES_VECTOR?
            }
#else
            else
            {
                // TODO: Isn't this what we want? Why does this fire an assertion for
                //       TestFactorial?
                markValueAs(ret,
                            uniformReturn ?
                                WFV::WFV_METADATA_OP_UNIFORM :
                                WFV::WFV_METADATA_OP_VARYING);
            }
#endif
        }
    }

    // Everything else is OP_UNIFORM for now (possibly updated during
    // following fixpoint iteration).
    for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
    {
        Instruction* inst = &*I;

        // Ignore our own metadata calls.
        if (WFV::isMetadataCall(inst))
        {
            continue;
        }

        if (WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_VARYING) ||
            WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
            WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
        {
            continue;
        }

        markValueAs(inst, WFV::WFV_METADATA_OP_UNIFORM);
    }

    if (mDisableControlFlowDivAnalysis)
    {
        // Mark all blocks as mandatory.
        for (auto &BB : *scalarFn)
        {
            // Mark only branches and returns.
            for (auto &I : BB)
            {
                if (!isa<BranchInst>(&I) && !isa<ReturnInst>(&I)) continue;

                if (BranchInst* br = dyn_cast<BranchInst>(&I))
                {
                    if (br->isUnconditional()) continue;
                }
                else if (ReturnInst* ret = dyn_cast<ReturnInst>(&I))
                {
                    if (!ret->getReturnValue()) continue;
                }

                markValueAs(&I, WFV::WFV_METADATA_OP_VARYING);
                markValueAs(&I, WFV::WFV_METADATA_RES_VECTOR);
            }

            // Mark all loops as LOOP_DIVERGENT.
            if (mLoopInfo->isLoopHeader(&BB))
            {
                markValueAs(&BB, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE);
            }
        }
    }


    // TODO: Introduce shortcuts to stop iteration, e.g. before updating
    //       of phis if nothing changed anyways.
    bool changed = true;
    while (changed)
    {
        changed = false;

        if (!mDisableControlFlowDivAnalysis)
        {
            // Mark loops as LOOP_DIVERGENT/LOOP_NON_DIVERGENT.
            if(mVerbose) outs() << "\nMarking divergent loops...\n\n";
            changed |= markDivergentLoops(*mLoopInfo);

            // Mark blocks as ALWAYS_BY_ALL
            if(mVerbose) outs() << "\nMarking blocks that are always executed by "
                                << "all threads...\n\n";
            changed |= markAlwaysByAllBlocks(scalarFn);

            // Mark blocks as ALWAYS_BY_ALL_OR_NONE or ALWAYS_BY_ALL_FALSE.
            // NOTE: This is only inside the fixpoint iteration because the derived
            //       information is required for the alloca update.
            changed |= markABAONBlocks(scalarFn);
        }
#ifdef WFV_ENABLE_LEGACY_API
        findReachableEdges(scalarFn);
#endif
        // Mark blocks as DIVERGENT/NON_DIVERGENT.
        // We *have* to mark them, even if control flow analyses are disabled.
        // TODO: We have to update the divergence-causing blocks even if
        //       the mark did not change!
        if(mVerbose) outs() << "\nMarking divergent blocks...\n\n";
        changed |= markDivergentBlocks(scalarFn);

        // Update UNIFORM/VARYING information of phis.
        if(mVerbose) outs() << "\nUpdating phis with divergence information...\n\n";
        changed |= updateUniformPhisWithDivergenceInfo(scalarFn);

        // Update UNIFORM/VARYING information of operations with side effects.
        if(mVerbose) outs() << "\nUpdating operations with possible side effects...\n\n";
        changed |= updateUniformSideEffectOperations(scalarFn);

        // Update UNIFORM/VARYING information of allocas.
        if(mVerbose) outs() << "\nUpdating alloca uniform/varying information...\n\n";
        changed |= updateUniformAllocas(scalarFn);

        // Update UNIFORM/VARYING information of LALB-values.
        if(mVerbose) outs() << "\nUpdating values that are live across loop boundaries...\n\n";
        changed |= updateUniformLALBValues(scalarFn);
    }

    // Mark blocks as MANDATORY/OPTIONAL.
    if(mVerbose) outs() << "\nMarking mandatory/optional blocks...\n\n";
    markMandatoryBlocks(scalarFn);

    // Mark all instructions without mark as UNIFORM.
    for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
    {
        Instruction* inst = &*I;
        if (WFV::hasMetadata(inst)) continue;
        markValueAs(inst, WFV::WFV_METADATA_OP_UNIFORM);
    }

    // Mark all OP_UNIFORM without RES_* mark as RES_UNIFORM.
    for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
    {
        Instruction* inst = &*I;
        if (!WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_UNIFORM)) continue;
        if (WFV::hasMetadata(inst, WFV::WFV_METADATA_RES_VECTOR) ||
            WFV::hasMetadata(inst, WFV::WFV_METADATA_RES_SCALARS)) continue;
        markValueAs(inst, WFV::WFV_METADATA_RES_UNIFORM);
    }

    // Mark loops as TOP_LEVEL_DIVERGENT and/or DIVERGENT_INNERMOST.
    markNestedDivergentTopLevelLoops();
}

// Recursively marks the instruction and all its uses as OP_VARYING / RES_VECTOR
// except for phis and selects which require some special handling.
// Returns true if some mark was set, false otherwise.
bool
VectorizationAnalysis::recursivelyMarkVarying(Instruction* inst,
                                              BasicBlock*  block)
{

    assert (inst);

    if (WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_VARYING))
    {
        if(mVerbose) outs() << "    previously marked as OP_VARYING - ignored!\n";
        return false;
    }

    if (WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
        WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
    {
        if(mVerbose) outs() << "    previously marked as OP_SEQUENTIAL(_GUARDED) - ignored!\n";
        return false;
    }

    bool changed = false;

    // We have to treat some instructions differently:
    // - a phi can only be marked OP_VARYING by the divergence analysis.
    // - a select is only OP_VARYING if its condition is non-uniform.
    switch (inst->getOpcode())
    {
        case Instruction::PHI:
        {
            // If block is set, this function was called from
            // the divergence analysis and requests the phi to be marked
            // OP_VARYING.
            // Otherwise, mark phi as RES_VECTOR only.
            if (block) { // FIXME hack: this is only valid, if there is only a single value incoming from divergent edges
#if 0
				auto * phi = dyn_cast<PHINode>(inst);

			// Check whether is only one value incoming from divergent edges
				// Remark: this must be a constant because of temporal divergence in loops
				// In the acyclic case, it suffices to check for the same value
				bool divergentPHI = false;
				Constant * divergentEdgeConst = nullptr;
				for (unsigned i = 0; i < phi->getNumIncomingValues(); ++i) {
					BasicBlock * inBlock = phi->getIncomingBlock(i);

					if (IsDivergentEdge(inBlock, block)) {
						if (Constant * thisEdgeConst = dyn_cast<ConstantExpr>(phi->getIncomingValue(i))) {
							if (!divergentEdgeConst) {
								divergentEdgeConst = thisEdgeConst;
							} else if (divergentEdgeConst != thisEdgeConst) {
								divergentPHI = true;
								break;
							}
						}
					}
				}

				// FIXME safe, to not promote vectors here?
				if (! divergentPHI) {
					break;
				}
#endif
			// Default case. One divergent edge-> the entire PHI is divergent
				assert (WFV::isExitOfDivergentLoop(*block, *mLoopInfo) ||
						!WFV::hasMetadata(block, WFV::WFV_METADATA_DIVERGENT_FALSE));

				changed |= markValueAs(inst, WFV::WFV_METADATA_OP_VARYING);
            }
            changed |= markValueAs(inst, WFV::WFV_METADATA_RES_VECTOR);
            break;
        }

        case Instruction::Select:
        {
            // If the condition is RES_UNIFORM, the select can remain uniform and
            // just choose one of the vectors.
            SelectInst* select = cast<SelectInst>(inst);
            Value* condition = select->getCondition();
            if ((isa<Argument>(condition) || isa<Instruction>(condition)) &&
                !WFV::hasMetadata(select->getCondition(), WFV::WFV_METADATA_RES_UNIFORM))
            {
                changed |= markValueAs(select, WFV::WFV_METADATA_OP_VARYING);
            }
            changed |= markValueAs(select, WFV::WFV_METADATA_RES_VECTOR);
            break;
        }

        // Mark instructions that do not have a result as OP_VARYING only.
        case Instruction::Store:
        case Instruction::Ret:
        case Instruction::Br:
        case Instruction::Switch:
        {
            changed |= markValueAs(inst, WFV::WFV_METADATA_OP_VARYING);
            break;
        }
        case Instruction::Call:
        {
            CallInst* call = cast<CallInst>(inst);
            if (!call->getType()->isVoidTy())
            {
                changed |= markValueAs(call, WFV::WFV_METADATA_RES_VECTOR);
            }
            changed |= markValueAs(call, WFV::WFV_METADATA_OP_VARYING);
            break;
        }
        default:
        {
            assert (!inst->getType()->isVoidTy());
            // Mark as OP_VARYING / RES_VECTOR.
            changed |= markValueAs(inst, WFV::WFV_METADATA_OP_VARYING);
            changed |= markValueAs(inst, WFV::WFV_METADATA_RES_VECTOR);
            break;
        }
    }

    if (!changed) return false;

    // Recurse into uses (DFS).
    for (Instruction::user_iterator U = inst->user_begin(),
            UE = inst->user_end(); U != UE; ++U)
    {
        assert (isa<Instruction>(*U));
        Instruction* userInst = cast<Instruction>(*U);
        recursivelyMarkVarying(userInst, nullptr);
    }

    return true;
}


bool
VectorizationAnalysis::markAlwaysByAllBlocks(Function* scalarFn)
{
    assert (scalarFn);

    if (mMaskPosition != -1)
    {
        if(mVerbose) outs() << "  Function has mask argument, no blocks can be " << "ALWAYS_BY_ALL!\n";
        return false; // Nothing changed.
    }

    BasicBlock* potentialABA = &scalarFn->getEntryBlock();

    // Mark the start block.
    bool changed = markValueAs(potentialABA, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE);

    while (potentialABA)
    {
        DomTreeNode* dtn = mPostDomTree->getNode(potentialABA);

        // If there is no post dominator, stop.
        if (!dtn || !dtn->getIDom()) break;

        // Set the post dominator as the current block.
        potentialABA = dtn->getIDom()->getBlock();

        if (!potentialABA) break;

        // If the block is not part of a divergent loop, mark it ABA.
        // NOTE: We have to check *all* loops to which this block belongs.
        Loop* loop              = mLoopInfo->getLoopFor(potentialABA);
        bool  isInDivergentLoop = false;

        while (loop)
        {
            if (!loop->contains(potentialABA))
            {
                break;
            }
            if (WFV::hasMetadata(loop, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE))
            {
                isInDivergentLoop = true;
                break;
            }
            loop = loop->getParentLoop();
        }

        changed |= markValueAs(potentialABA,
                               isInDivergentLoop ?
                                   WFV::WFV_METADATA_ALWAYS_BY_ALL_FALSE :
                                   WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE);
    }

    return changed;
}


#ifdef WFV_ENABLE_LEGACY_API
// Collect paths from each varying branch to those blocks of the function
// that can be reached from it.
void
VectorizationAnalysis::findReachableEdges(Function* scalarFn)
{
	if(mVerbose) outs() << "\nENTERED findReachableEdges:\n" << mReachableEdgeMap;

    assert (scalarFn);

    for (auto it : mReachableEdgeMap)
    {
        delete it.second;
    }
    mReachableEdgeMap.clear();

    for (auto it : mVisitedEdgeMap)
    {
        delete it.second;
    }
    mVisitedEdgeMap.clear();

    // Initialize empty info for every block.
    for (const auto &BB : *scalarFn)
    {
        mReachableEdgeMap[&BB] = new DivergenceInfo();
    }

    // Start recursive traversal.
    visitEdge(&scalarFn->getEntryBlock(), nullptr);

    if(mVerbose) outs() << "\nReachable edges:\n" << mReachableEdgeMap;
}
#endif

bool
VectorizationAnalysis::markDivergentBlocks(Function* scalarFn)
{
    assert (scalarFn);
    bool changed = false;

    for (Function::iterator BB=scalarFn->begin(), BBE=scalarFn->end(); BB!=BBE; ++BB)
    {
        SmallVector<Value*, 2> divergenceCausingBlocks;
        const bool divergent = isDivergent(&*BB, divergenceCausingBlocks);
        changed |= markValueAs(&*BB,
                               divergent ?
                                   WFV::WFV_METADATA_DIVERGENT_TRUE :
                                   WFV::WFV_METADATA_DIVERGENT_FALSE);
        if (divergent)
        {
            assert (!divergenceCausingBlocks.empty());
            WFV::setMetadataForDivergentBlock(&*BB, divergenceCausingBlocks);
        }
    }

    return changed;
}

Loop *
VectorizationAnalysis::getCommonLoop(const BasicBlock * A, const BasicBlock * B) const {
	const LoopInfo & LI = *mLoopInfo;
	Loop * aLoop = LI.getLoopFor(A);
	if (!aLoop) return nullptr;
	Loop * bLoop = LI.getLoopFor(B);
	if (!bLoop) return nullptr;

	while (aLoop != bLoop) {
		uint aDepth = !aLoop ? 0 : aLoop->getLoopDepth();
		uint bDepth = !bLoop ? 0 : bLoop->getLoopDepth();
		if (aDepth >= bDepth) {
			aLoop = aLoop->getParentLoop();
		} else if (aDepth < bDepth) {
			bLoop = bLoop->getParentLoop();
		}
	}

	return aLoop;
}

static inline
const DomTreeNode*
GetIDom(const DominatorTree & domTree, const BasicBlock & block) {
	const DomTreeNode * domNode = domTree.getNode(const_cast<BasicBlock*>(&block));
	return domNode->getIDom();
}

#if 1
#define VA_DBG(X) X
#else
#define VA_DBG(X)
#endif

bool
VectorizationAnalysis::checkForDisjointLoopPaths(const BasicBlock*             exitBlock,
				const Loop * loop,
				BlockSet * divCauseBlocks) const {

	NodeSet exitingNodes;
	SketchGraph * graph = nullptr;

// Restrict the graph search to @loop and exiting edges from @loop to @exitBlock
	BasicBlock * loopHeader = loop->getHeader();

	graph = SketchGraph::createFromDominatorNode(*mDomTree, *loopHeader, exitingNodes);

// get the node handle for the exiting node
	const BasicBlock * exitingBlock = exitBlock->getUniquePredecessor();
	assert(exitingBlock);
	auto exitingNode = graph->getNode(*exitingBlock);

// add the exiting node explicitly
	SketchNode * exitNode = graph->push(exitBlock);
	exitingNode->insert(exitNode);

// only consider reachable edges
	const BasicBlock * latchBlock = loop->getLoopLatch();
#if 0
	errs() << "Loop scope "; loop->print(errs());
	llvm::errs() << "Graph "; graph->dump();
	llvm::errs() << "Exit block: "<< exitBlock->getName() << "\n";
	llvm::errs() << "Latch block: "<< latchBlock->getName() << "\n";
#endif
	SketchNode * latchNode = graph->getNode(*latchBlock);
	if (!latchNode) {
		return false; // FIXME
	}

	assert(latchNode && "latchNode killed during clean-up");
	// except for the actual path enumeration, this is copied over from isMandatoryExit(..)
	NodeSet accepting;
	accepting.insert(exitNode);
	accepting.insert(latchNode);
	PathFinder finder(*graph, accepting);

	const auto & loopBlocks = loop->getBlocks();

	bool isMandatoryExit = false;
    for (const auto * vBlock : loopBlocks) {
		if (! WFV::HasVaryingBranch(*vBlock))
			continue;

	   // this only applies to nodes, that vBlock does not post-dominate
	   if ( mPostDomTree->dominates(exitBlock, vBlock) )
		   continue;

	   if ( mPostDomTree->dominates(latchBlock, vBlock) )
		   continue;

    // new path search algorithm starting from here
		SketchNode * parentNode = graph->getNode(*vBlock);
		assert(parentNode && "branch not modelled");

		if (finder.findPath(parentNode)) { // validity is not checked for first node
#ifndef VA_VERIFY
            if(mVerbose) {
                outs() << "  Block '" << exitBlock->getName() << "' is MANDATORY (4)!\n";
                outs() << "    due to exit '" << vBlock->getName() << "'.\n";
            }
			// Add both the current block as well as the latch as rewire targets for the branch parent.
			WFV::addRewireTargetForDCBlock(const_cast<BasicBlock*>(vBlock), const_cast<BasicBlock*>(exitBlock));
			WFV::addRewireTargetForDCBlock(const_cast<BasicBlock*>(vBlock), const_cast<BasicBlock*>(latchBlock));
#endif
			if (divCauseBlocks) {
				divCauseBlocks->insert(const_cast<BasicBlock*>(exitBlock));
			}
			isMandatoryExit =  true;
			break;
		}
	}

    delete graph;
	return isMandatoryExit;
}


bool
VectorizationAnalysis::checkForDisjointPaths(const BasicBlock*             block,
					bool doNotBranchToOuterLoops,
					ConstBlockSet & divergenceCausingBlocks) const {
	NodeSet exitingNodes;
	BasicBlock * entryBlock = &mScalarFunction->getEntryBlock();
	SketchGraph * graph = nullptr;

	const DomTreeNode * idom = GetIDom(*mDomTree, *block);
	BasicBlock * idomBlock = idom ? idom->getBlock() : nullptr;

	// Restrict the search to nodes in the idom region that reach @block
	if (doNotBranchToOuterLoops) {
		const Loop * loop = mLoopInfo->getLoopFor(block);
		BasicBlock * topBlock = loop ? loop->getHeader() : entryBlock;
		graph = SketchGraph::createFromDominatorNode(*mDomTree, *topBlock, exitingNodes);

	} else{
		graph = SketchGraph::createFromDominatorNode(*mDomTree, *entryBlock, exitingNodes);
		assert(exitingNodes.empty() && "invalid dominance graph for entry block");
	}

#if 0
	errs() << "Original graph:\n";
	graph->dump();
	mLoopInfo->print(errs());
#endif

// Remove all back-edges to outer loops
	// FIXME
	Loop * targetLoop = mLoopInfo->getLoopFor(block);
	if (doNotBranchToOuterLoops) {
		for (Loop * outerLoop = targetLoop;
				outerLoop;
				outerLoop = outerLoop->getParentLoop())
		{
			SketchNode * outerHeaderNode = graph->getNode(*outerLoop->getHeader());
			NodeSet outerLoopPredSet = graph->getPredecessors(outerHeaderNode);
			// graph->dumpSet(outerLoopPredSet);

			for (SketchNode * outerHeaderPred : outerLoopPredSet) {
				outerHeaderPred->erase(outerHeaderNode);
			}
		}
		// reference
		// original condition restated here
		// if (loop == targetLoop || !targetLoop->contains(loop)) continue;
	}
// Eliminate dead-ends to accelerate path search
	{
		NodeSet keepSet;
		keepSet.insert(graph->getNode(*block));
		graph->eliminateDeadEnds(keepSet);
	}

	SketchNode * blockNode = graph->getNode(*block);

// Run confluence contraction to merge any two nodes that will eventually pass through the same post-dominating node
	SketchGraph * compactGraph = nullptr;

	if (graph->getNumNodes() < 5) {
		compactGraph = graph;
		if(mVerbose) errs() << "[VA] Graph with "<<  graph->getNumNodes() << " nodes to small for compaction\n";
	} else {
		NodeSet coreNodes = graph->getPredecessors(blockNode);
		coreNodes.insert(blockNode);
		compactGraph = graph->generateConfluenceGraph(coreNodes, nullptr);
		// FIXME also make verbose
		if(mVerbose) errs() << "[VA] Compacted from " << graph->getNumNodes() << " to "
                            << compactGraph->getNumNodes() << "\n";
	}

#ifdef VERBOSE
	errs () << "Mapping "; confluenceMap.dump(*compactGraph, *graph);
	errs() << "Compact "; compactGraph->dump();
	errs() << "Source "; graph->dump();
#endif

// Check for existing path
	SketchNode * compactBlockNode = compactGraph->getNode(*block);
	assert(compactBlockNode);

	NodeSet compactAcceptingNodes = compactGraph->getPredecessors(compactBlockNode);
	PathFinder finder(*compactGraph, compactAcceptingNodes);

	for (auto & pair  : *compactGraph) {
		SketchNode * vCompactNode = pair.first;
		const llvm::BasicBlock * vBlock = pair.second;

		// optimization only look below the idom (if any)
		if (! idomBlock || (vBlock == idomBlock) || mDomTree->dominates(idomBlock, vBlock)) { //  TODO Only nodes below the idom are interesting
			if (WFV::HasVaryingBranch(*vBlock)) {
				/* const BasicBlock * A;
				const BasicBlock * B;
				GetTwoSuccessors(vBlock, A, B);

				// FIXME do look-ups in the contraction map
				SketchNode * aStart = graph->getNode(*A);
				SketchNode * bStart = graph->getNode(*B);
				SketchNode * aCompactStart = confluenceMap.getRootForNode(aStart);
				SketchNode * bCompactStart = confluenceMap.getRootForNode(bStart);
				assert(aCompactStart && bCompactStart);

				if (aCompactStart && aCompactStart == bCompactStart) { // a and b have the same post-dominating node
					continue;
				} */

				// the node was not represented in this graph (FIXME this could be the reason why IDom filtering does not work. Consider, that one path might stay at vBlock==block)
				// if (vCompactNode->size() <= 2) { continue; }

				if ( finder.findPath(vCompactNode)) { // validity is not checked for first node
					divergenceCausingBlocks.insert(const_cast<BasicBlock*>(vBlock)); // FIXME const_cast
				}
			}
		}
	}

	delete graph;
	if (compactGraph != graph) delete compactGraph;
	return ! divergenceCausingBlocks.empty();
}

// A block is divergent if there exist two *disjoint* paths from a block v
// that ends with a varying branch to the current block b.
bool
VectorizationAnalysis::isDivergent(BasicBlock*             block,
                                   SmallVector<Value*, 2>& divergenceCausingBlocks) const
{
    assert (block);
    if(mVerbose) outs() << "\nisDivergent(" << block->getName() << ")\n";

    if (WFV::hasMetadata(block, WFV::WFV_METADATA_DIVERGENT_TRUE))
    {
        if(mVerbose) outs() << "  Block '" << block->getName() << "' is DIVERGENT (0)!\n";
        // TODO: HERE! We have to update the 'divergence-causing block'
        //             information during fixpoint iteration!
        WFV::getDivergenceCausingBlocks(*block, divergenceCausingBlocks);
        return true;
    }

    // A block with less than two incoming edges can not be divergent.
    if (block->getUniquePredecessor() ||
        &block->getParent()->getEntryBlock() == block ||
        pred_begin(block) == pred_end(block))
    {
        if(mVerbose) outs() << "  has less than two incoming edges - NON_DIVERGENT!\n";
        return false;
    }

    ConstBlockSet divCauseBlocks;
    checkForDisjointPaths(block, true, divCauseBlocks); // FIXME const_cast
    for (auto it : divCauseBlocks) divergenceCausingBlocks.push_back(const_cast<BasicBlock*>(it));

    // Legacy algorithm for verification purposes
#ifdef VA_VERIFY
    SmallVector<Value*, 2> altDivCausingBlocks;

    // Collect reachable edges of all predecessors.
    // Look at edges from varying branches that are reachable via predecessors.
    // If there is one of which one side is reachable from one predecessor and
    // the other side is reachable from another predecessor while none can be
    // reached from both, the criterion is met.
    // This criterion is only relevant for blocks with more than one predecessor.
    SmallVector<const EdgeMapType*, 2> predInfos;
    SmallVector<const BasicBlock*, 2>  predBlocks;
    const BasicBlock*                  reference  = nullptr;
    bool                               allTheSame = true;

    typedef GraphTraits<Inverse<const BasicBlock*> > InvBlockTraits;
    for (InvBlockTraits::ChildIteratorType P = InvBlockTraits::child_begin(block),
            PE = InvBlockTraits::child_end(block); P != PE; ++P)
    {
        const BasicBlock* v = *P;
        assert (mReachableEdgeMap.find(v) != mReachableEdgeMap.end());
        assert (mReachableEdgeMap.find(v)->second);

        const DivergenceInfo& dInfo = *mReachableEdgeMap.find(v)->second;

        // If any incoming edge has no reference, the edges can
        // not have a common reference.
        if (!dInfo.mReference) allTheSame = false;

        // Otherwise, set the reference (first edge), or compare
        // the reference to the current reference.
        if (allTheSame)
        {
            if (!reference) reference = dInfo.mReference;
            allTheSame &= reference == dInfo.mReference;
        }

        // If the predecessor is unreachable, there is no edge map.
        if (!dInfo.mReachableEdges) continue;

        // In order to cope with critical edges, we have to add the
        // incoming edges to the set as well.
        // This forces us to create a copy of the map so we do not alter
        // any information.
        const EdgeMapType& edgeMapOrig = *dInfo.mReachableEdges;
        EdgeMapType*       edgeMap     = new EdgeMapType(edgeMapOrig);
        const BranchInst*  br          = dyn_cast<BranchInst>(v->getTerminator());
        if (br && WFV::hasMetadata(br, WFV::WFV_METADATA_OP_VARYING))
        {
            // TODO: use insertReachableEdge() ?
            if (edgeMap->count(br))
            {
                if (br->getSuccessor(0) == block) edgeMap->find(br)->second.first = true;
                else edgeMap->find(br)->second.second = true;
            }
            else
            {
                const bool trueEdge = br->getSuccessor(0) == block;
                (*edgeMap)[br] = std::make_pair(trueEdge, !trueEdge);
            }
        }

        predInfos.push_back(edgeMap);
        predBlocks.push_back(v);

        if(mVerbose) outs() << "predInfos[" << v->getName()
                << "]:\n" << *predInfos[predInfos.size()-1] << "\n";
    }

    if (allTheSame && reference)
    {
        if(mVerbose) outs() << "  all edges refer to the set of the same block '"
                << reference->getName() << "' - NON_DIVERGENT!\n";
        for (auto em : predInfos) delete em;
        return false;
    }

    // Otherwise, a loop back edge has to be involved.

    // Check each pair of incoming edges.
    for (unsigned i=0, e=predInfos.size(); i<e; ++i)
    {
        const EdgeMapType& edgeMap0 = *predInfos[i];
        const BasicBlock&  pred0    = *predBlocks[i];

        // Copy the edge map to vectors to enforce deterministic behavior.
        SmallVector<const BranchInst*, 16> branchVec;
        SmallVector<bool, 16> trueVec;
        SmallVector<bool, 16> falseVec;
        for (const auto &BB : *pred0.getParent())
        {
            if (isa<ReturnInst>(BB.getTerminator())) continue;
            assert (isa<BranchInst>(BB.getTerminator()));
            const BranchInst* branch = cast<BranchInst>(BB.getTerminator());
            EdgeMapType::const_iterator it0 = edgeMap0.find(branch);
            if (it0 == edgeMap0.end()) continue;
            branchVec.push_back(branch);
            trueVec.push_back(it0->second.first);
            falseVec.push_back(it0->second.second);
        }

        for (unsigned j=i+1; j<e; ++j)
        {
            const EdgeMapType& edgeMap1 = *predInfos[j];
            const BasicBlock&  pred1    = *predBlocks[j];

            for (unsigned brIdx=0, brE=branchVec.size(); brIdx<brE; ++brIdx)
            {
                const BranchInst* branch = branchVec[brIdx];
                if(mVerbose) outs() << "  varying branch in block: "
                        << branch->getParent()->getName() << "\n";

                EdgeMapType::const_iterator it1 = edgeMap1.find(branch);
                if (it1 == edgeMap1.end()) continue;

                // Determine whether there are disjoint paths to the true- and
                // false-edge of that branch. If the paths were not disjoint,
                // the two sets would be equal (pointer equivalence test above).
                const bool trueFrom0  = trueVec[brIdx];
                const bool trueFrom1  = it1->second.first;
                const bool falseFrom0 = falseVec[brIdx];
                const bool falseFrom1 = it1->second.second;
                if(mVerbose) {
                    outs() << (trueFrom0  ? "   - trueFromPred0\n" : "");
                    outs() << (trueFrom1  ? "   - trueFromPred1\n" : "");
                    outs() << (falseFrom0 ? "   - falseFromPred0\n" : "");
                    outs() << (falseFrom1 ? "   - falseFromPred1\n" : "");
                }

                // We only care about disjoint paths if both edges reach
                // the current block over different incoming edges.
                const bool case1 = trueFrom0 && falseFrom1;
                const bool case2 = falseFrom0 && trueFrom1;
                if (!case1 && !case2) continue;

#if 1
                // If only one of the two cases exists, we can not conclude that the
                // paths can not overlap. This is because a case can exist because of
                // outer loops, which we must not take into account for this criterion.

                // Now we have to make sure that the potentially disjoint paths are
                // actually disjoint.
                WFV::PathVecType pathsTo0;
                WFV::PathVecType pathsTo1;

                if(mVerbose) {
                    outs() << "    collecting paths: " << branch->getParent()->getName();
                    outs() << " -> " << pred0.getName() << "\n";
                }

                SmallPtrSet<const BasicBlock*, 16> visitedBlocks;
                SmallPtrSet<const BasicBlock*, 16> unreachableBlocks;
                WFV::PathType currentPath;
                WFV::collectPaths(&pred0,
                                  branch->getParent(),
                                  currentPath,
                                  pathsTo0,
                                  true, /* ignoreOuterLoops */
                                  *mLoopInfo,
                                  visitedBlocks,
                                  unreachableBlocks);

                if(mVerbose) {
                    outs() << "\n  Paths to predecessor '" << pred0.getName() << "':\n";
                    printPaths(pathsTo0, outs()); outs() << "\n";
                }

                // If no path to the exit was found, this means they all go through the
                // latch and thus overlap with any path to the latch. Thus, the block
                // is not MANDATORY due to this varying branch.
                if (pathsTo0.empty()) continue;

                if(mVerbose) {
                    outs() << "    collecting paths: " << branch->getParent()->getName();
                    outs() << " -> " << pred1.getName() << "\n";
                }

                // Otherwise, also collect paths to the latch and check if there is any
                // pair of paths p1 \in pathsTo0, p2 \in pathsTo1 that are disjoint.
                visitedBlocks.clear();
                unreachableBlocks.clear();
                currentPath.clear();
                WFV::collectPaths(&pred1,
                                  branch->getParent(),
                                  currentPath,
                                  pathsTo1,
                                  true, /* ignoreOuterLoops */
                                  *mLoopInfo,
                                  visitedBlocks,
                                  unreachableBlocks);

                if(mVerbose) {
                    outs() << "\n  Paths to predecessor '" << pred1.getName() << "':\n";
                    printPaths(pathsTo1, outs()); outs() << "\n";
                }

                if (hasDisjointPaths(pathsTo0, pathsTo1, true /* ignoreStartBlock */))
                {
                	bool alreadyAdded = false;
                    for (const auto &dcBB : altDivCausingBlocks)
                    {
                        if (dcBB != branch->getParent()) continue;
                        alreadyAdded = true;
                        break;
                    }
                    if (!alreadyAdded)
                    {
                    	altDivCausingBlocks.push_back(const_cast<BasicBlock*>(branch->getParent()));
                    }
                    continue;
                }
#endif
#if 0 // legacy code
                const BasicBlock& truePathStart  = *branch->getSuccessor(0);
                const BasicBlock& falsePathStart = *branch->getSuccessor(1);

                if (case1)
                {
                    const BasicBlock& truePathEnd  = pred0;
                    const BasicBlock& falsePathEnd = pred1;

                    if (isDivergentDueToDisjointPaths(*block,
                                                      truePathStart,
                                                      falsePathStart,
                                                      truePathEnd,
                                                      falsePathEnd,
                                                      *branch->getParent()))
                    {
                        bool alreadyAdded = false;
                        for (const auto &dcBB : altDivCausingBlocks)
                        {
                            if (dcBB != branch->getParent()) continue;
                            alreadyAdded = true;
                            break;
                        }
                        if (!alreadyAdded)
                        {
                            // Metadata requires to supply ArrayRef<Value*> with non-const Value* ...
                        	altDivCausingBlocks.push_back(const_cast<BasicBlock*>(branch->getParent()));
                        }
                        continue;
                    }
                }

                if (case2)
                {
                    const BasicBlock& truePathEnd  = pred1;
                    const BasicBlock& falsePathEnd = pred0;

                    if (isDivergentDueToDisjointPaths(*block,
                                                      truePathStart,
                                                      falsePathStart,
                                                      truePathEnd,
                                                      falsePathEnd,
                                                      *branch->getParent()))
                    {
                        bool alreadyAdded = false;
                        for (const auto &dcBB : altDivCausingBlocks)
                        {
                            if (dcBB != branch->getParent()) continue;
                            alreadyAdded = true;
                            break;
                        }
                        if (!alreadyAdded)
                        {
                        	altDivCausingBlocks.push_back(const_cast<BasicBlock*>(branch->getParent()));
                        }
                        continue;
                    }
                }
#endif

            } // for all edges
        }
    }
    for (auto em : predInfos) delete em;
#endif // VA_VERIFY

    if (divergenceCausingBlocks.empty())
    {
        if(mVerbose) outs() << "  no pair of incoming edges with disjoint paths - NON_DIVERGENT!\n";
    }

#ifdef VA_VERIFY
    //  --
    // Verification code for the new divergence analysis
    // TODO verify @otherDivCausingBlocks
    bool newDivergent = ! divergenceCausingBlocks.empty();
    bool oldDivergent = ! altDivCausingBlocks.empty();

    bool unequalSets = false;

	if (newDivergent != oldDivergent) {
		errs() << "Evaluating sets of divergence causing blocks\n";
		errs() << "Missing blocks {";
		for (auto altBlock : altDivCausingBlocks) {
			if (! divCauseBlocks.count(static_cast<BasicBlock*>(altBlock))) {
				errs() << altBlock->getName() << ", ";
				unequalSets = true;
			}
		}
		errs() << "}\n";

		errs() << "Invalid blocks {";
		for (auto newBlock : divCauseBlocks) {
			bool foundBlock = false;
			for (auto altBlock : altDivCausingBlocks) {
				if (newBlock == altBlock) { foundBlock = true; break; }
			}

			if (! foundBlock) {
				errs() << newBlock->getName() << ", ";
				unequalSets = true;
			}
		}
		errs() << "}\n";

		assert(! unequalSets && "sets of divergence causing blocks do not agree!");
	}
    assert( (!newDivergent || oldDivergent) && "newDiv => oldDiv");
    assert( (!oldDivergent || newDivergent) && "oldDiv => newDiv");
    // -- DEBUG
#endif
    return ! divergenceCausingBlocks.empty();
}

#ifdef WFV_ENABLE_LEGACY_API
// The idea here is that if both paths have the the same set of edges incoming,
// this can only happen a) if there are disjoint paths ("crossing" without
// overlapping), b) if they both reference the same parent (already
// tested before calling this function), or c) if there is a loop back edge
// involved. In case c), the question whether the paths are disjoint breaks
// down to whether both paths require a common back edge.
bool
VectorizationAnalysis::isDivergentDueToDisjointPaths(const BasicBlock& block,
                                                     const BasicBlock& truePathStart,
                                                     const BasicBlock& falsePathStart,
                                                     const BasicBlock& truePathEnd,
                                                     const BasicBlock& falsePathEnd,
                                                     const BasicBlock& branchParent) const
{
    // If both paths do not require a back edge, the paths
    // are disjoint and thus the block is divergent.
    // TODO: We have to allow taking back edges of inner loops!
    //       I don't think so: If we come from outside the loop, we can not
    //       reach additional blocks with the back edge that we can't reach
    //       otherwise (for reducible loops!).

    if(mVerbose) {
        outs() << "  true path : " << truePathStart.getName() << " -> " << truePathEnd.getName() << "\n";
        outs() << "  false path: " << falsePathStart.getName() << " -> " << falsePathEnd.getName() << "\n";
    }

    // The block in question is divergent if
    // 1) no path requires a back edge, or
    // 2) only one path requires a back edge, or
    // 3) both paths require a different back edge and
    //    the block does not belong to the inner loop.

    // If the branch is not in a loop, the paths are divergent.
    if (!mLoopInfo->getLoopFor(&branchParent))
    {
        if(mVerbose) outs() << "  disjoint paths found (no back edge) - DIVERGENT (1a)!\n";
        return true;
    }

    // Since we iterate from the successor of the parent block
    // to the predecessor of the end block, we could accidentally
    // invert the path if the blocks directly succeed each other.
    // Thus, we need to treat this either in the iteration scheme
    // of hasPathWithBackedge() (call with terminator+index instead
    // of block) or via a special case.
    // We choose the latter to keep hasPathWithBackedge() as simple
    // as possible.
    const bool truePathStartIsEnd  = &branchParent == &truePathEnd;
    const bool falsePathStartIsEnd = &branchParent == &falsePathEnd;

    // Work our way from innermost to outermost loop and check
    // every time if there are paths for one or both edges.
    // We start off without a loop since there might be a direct
    // path without back edge even if we are inside a loop.
    DenseSet<const BasicBlock*> visitedBlocks;

    visitedBlocks.clear();
    const bool truePathFound = truePathStartIsEnd ||
    hasPathWithBackedge(truePathStart,
                        truePathEnd,
                        nullptr,
                        false, /* allowParentLoopBackedges */
                        visitedBlocks);

    visitedBlocks.clear();
    const bool falsePathFound = falsePathStartIsEnd ||
    hasPathWithBackedge(falsePathStart,
                        falsePathEnd,
                        nullptr,
                        false, /* allowParentLoopBackedges */
                        visitedBlocks);

    if(mVerbose) {
        if (truePathFound) outs() << "  true path found!\n";
        if (falsePathFound) outs() << "  false path found!\n";
    }

    // If we find paths for both edges, they are disjoint since they
    // are not in a loop.
    if (truePathFound && falsePathFound)
    {
        if(mVerbose) outs() << "  disjoint paths found (no back edge)" << " - DIVERGENT (1b)!\n";
        return true;
    }

    // If we found no path, they are not disjoint since they both
    // require the same back edge.
    if (!truePathFound && !falsePathFound)
    {
        return false;
    }

    // Otherwise, we found one path only.

    // Divergence of the block now depends upon whether the
    // block is inside the loop(s) whose back edge is(are)
    // required for the other path (the one not found).
    // If the block is inside, it is not divergent because
    // we compare information that relates to different
    // iterations of the same loop(s).
    // If the block is outside, it is divergent because the
    // loop(s) has(have) finished iterating when this block
    // is reached.

    // The block is divergent if it is outside the loop(s) that
    // is(are) required for the other path. The innermost loop
    // is not necessarily the next parent loop, so we have to
    // look for the loop(s) that is(are) required.
    Loop* tmpLoop = mLoopInfo->getLoopFor(&branchParent);

    // Since both edges are reachable from both paths,
    // but only one is reachable without back edges,
    // there *has* to be a parent loop.
    assert (tmpLoop);

    const BasicBlock& pathStart = truePathFound ? falsePathStart : truePathStart;
    const BasicBlock& pathEnd   = truePathFound ? falsePathEnd   : truePathEnd;

    const bool pathStartIsEnd = &branchParent == &pathEnd;

    if(mVerbose) {
        outs() << "  looking for required back edge on path: " << pathStart.getName() << " ->"
        << pathEnd.getName() << " (blocks can be inaccurate)\n";
    }

    while (tmpLoop)
    {
        visitedBlocks.clear();
        const bool pathFound = pathStartIsEnd ||
            hasPathWithBackedge(pathStart,
                                pathEnd,
                                tmpLoop,
                                true, /* allowParentLoopBackedges */
                                visitedBlocks);

        if (pathFound) break;

        tmpLoop = tmpLoop->getParentLoop();
    }

    // If no path was found, this means that there is only a
    // path that requires taking at least two back edges. In
    // this case, the inner of the back edges must be used by
    // both paths, so they are not disjunct.
    // TODO: I'm not entirely sure that there are no cases where
    //       we should allow taking inner back edges as well.
    if (!tmpLoop) return false;

    if (!tmpLoop->contains(&block))
    {
        if(mVerbose) outs() << "  disjoint paths found (block reachable"
            << " by different back edges) - DIVERGENT (2)!\n";
        return true;
    }

    return false;
}

bool
VectorizationAnalysis::hasPathWithBackedge(const BasicBlock&            block,
                                           const BasicBlock&            target,
                                           const Loop*                  allowedBackedge,
                                           const bool                   allowParentLoopBackedges,
                                           DenseSet<const BasicBlock*>& visitedBlocks) const
{
    if (&block == &target) return true;

    if (visitedBlocks.count(&block)) return false;
    visitedBlocks.insert(&block);

    const TerminatorInst& terminator = *block.getTerminator();
    const Loop*           loop       = mLoopInfo->getLoopFor(&block);

    for (unsigned i=0, e=terminator.getNumSuccessors(); i<e; ++i)
    {
        const BasicBlock& succBB = *terminator.getSuccessor(i);

        const bool isBackedge = loop &&
            loop->getLoopLatch() == &block &&
            loop->getHeader()    == &succBB;

        // Stop if this is a different back edge than the allowed one(s).
        if (isBackedge)
        {
            bool isAllowed = false;
            const Loop* nextAllowedBackedge = allowedBackedge;
            while (!isAllowed && nextAllowedBackedge)
            {
                if (loop == nextAllowedBackedge)
                {
                    isAllowed = true;
                    break;
                }
                nextAllowedBackedge = nextAllowedBackedge->getParentLoop();
            }
            if (!isAllowed) continue;
        }

        const bool foundPath = hasPathWithBackedge(succBB,
                                                   target,
                                                   allowedBackedge,
                                                   allowParentLoopBackedges,
                                                   visitedBlocks);
        if (foundPath) return true;
    }

    return false;
}
#endif

bool
VectorizationAnalysis::markMandatoryBlocks(Function* scalarFn)
{
    assert (scalarFn);

    bool changed = false;
    for (Function::iterator BB=scalarFn->begin(), BBE=scalarFn->end(); BB!=BBE; ++BB)
    {
        changed |= markValueAs(&*BB,
                               isMandatory(&*BB, *mLoopInfo) ?
                                   WFV::WFV_METADATA_MANDATORY :
                                   WFV::WFV_METADATA_OPTIONAL);
    }

    return changed;
}

// If the block only depends on non-divergent control-flow,
// it is "always by all or none".
// Those are blocks that have only ABA or ABAON predecessors
// with uniform branches.
bool
VectorizationAnalysis::markABAONBlocks(Function* scalarFn)
{
    assert (scalarFn);

    if (mMaskPosition != -1)
    {
        if(mVerbose) outs() << "  Function has mask argument, no blocks can be "
            << "ALWAYS_BY_ALL_OR_NONE!\n";
        return false; // Nothing changed.
    }

    bool changed = false;

    // There might be dependencies between ABAON blocks, so we have to make
    // sure we do not mark blocks as ABA_FALSE too early.
    // Blocks that post dominate ABAON blocks have to be ABAON as well,
    // unless they are in divergent loops (just like ABA analysis).
    // Do a top-down DFS and only mark blocks of which we have seen all predecessors.
    SmallVector<BasicBlock*, 8> stack;
    SmallPtrSet<BasicBlock*, 16> visitedBlocks;
    stack.push_back(&scalarFn->getEntryBlock());
    bool isFollowedPostDom = false;

    while (!stack.empty())
    {
        BasicBlock* block = stack.pop_back_val();

        // If we have already seen this block, stop recursion.
        if (visitedBlocks.count(block)) continue;

        // If we have not yet seen all predecessors, stop recursion for this path,
        // unless we are in a loop header and the predecessor is the latch, or we
        // are following a post dominator.
        // In case of a loop header, mark it depending on the loop's DIVERGENT mark,
        // and continue as if we had seen all predecessors.
        // In case of a post dominator, mark it ABAON and continue as if we had seen
        // all predecessors.
        Loop* loop = mLoopInfo->getLoopFor(block);
        const bool isLoopHeader = loop && loop->getHeader() == block;
        bool allSeen = true;
        for (pred_iterator P=pred_begin(block), PE=pred_end(block); P!=PE; ++P)
        {
            BasicBlock* predBB = *P;
            if (visitedBlocks.count(predBB)) continue;
            allSeen = false;
            break;
        }
        if (!allSeen && !isLoopHeader && !isFollowedPostDom) continue;

        // Otherwise, mark current block as seen, and reset postdom flag.
        visitedBlocks.insert(block);
        isFollowedPostDom = false;

        // Push all successors onto the stack for DFS traversal.
        TerminatorInst* terminator = block->getTerminator();
        for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
        {
            BasicBlock* succBB = terminator->getSuccessor(i);
            stack.push_back(succBB);
        }

        // If the block is marked already, skip it.
        if (WFV::hasMetadata(block, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) ||
            WFV::hasMetadata(block, WFV::WFV_METADATA_ALWAYS_BY_ALL_FALSE))
        {
            continue;
        }

        // Otherwise, mark the block.

        // If the block has a predecessor with OP_VARYING branch, it is ABA_FALSE.
        bool marked = false;
        bool hasABAFalsePred = false;
        for (pred_iterator P=pred_begin(block), PE=pred_end(block); P!=PE; ++P)
        {
            BasicBlock* predBB = *P;
            if (!WFV::hasMetadata(predBB->getTerminator(), WFV::WFV_METADATA_OP_UNIFORM))
            {
                changed |= markValueAs(block, WFV::WFV_METADATA_ALWAYS_BY_ALL_FALSE);
                marked = true;
                break;
            }

            if (!WFV::hasMetadata(predBB, WFV::WFV_METADATA_ALWAYS_BY_ALL_FALSE)) continue;
            hasABAFalsePred = true;
        }

        if (marked) continue;

        // If we are in a divergent loop, the block is ABA_FALSE.
        if (loop && !WFV::hasMetadata(loop, WFV::WFV_METADATA_LOOP_DIVERGENT_FALSE))
        {
            changed |= markValueAs(block, WFV::WFV_METADATA_ALWAYS_BY_ALL_FALSE);
            continue;
        }

        // Otherwise, it is ABAON
        // - if it post dominates an ABAON block and is dominated by it, or
        // - if it has no ABA_FALSE predecessor,
        // and ABA_FALSE otherwise.
        if(mVerbose) if (hasABAFalsePred) outs() << "  block '" << block->getName() << "' has ABA_FALSE predecessor.\n";
        if (!hasABAFalsePred)
        {
            changed |= markValueAs(block, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE);
        }
        else
        {
            bool isABAONSESEExit = false;
            DomTreeNode* dtn = mDomTree->getNode(block);
            if (dtn && dtn->getIDom() &&
                WFV::hasMetadata(dtn->getIDom()->getBlock(), WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE))
            {
                BasicBlock* postDominatedBB = dtn->getIDom()->getBlock();
                dtn = mPostDomTree->getNode(postDominatedBB);
                if (dtn && dtn->getIDom())
                {
                    BasicBlock* dominatedBB = dtn->getIDom()->getBlock();
                    isABAONSESEExit = dominatedBB == block;
                    if(mVerbose) if (isABAONSESEExit) outs() << "  block '" << block->getName() << "' is exit of ABAON SESE region.\n";
                }
            }
            changed |= markValueAs(block,
                                   (isABAONSESEExit || !hasABAFalsePred) ?
                                       WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE :
                                       WFV::WFV_METADATA_ALWAYS_BY_ALL_FALSE);
        }

        if (hasABAFalsePred) continue;

        // We just marked the block ABAON, so if there is a block that post-dominates the current
        // block and is dominated by the current block, we now mark it ABAON as well unless it is
        // in a divergent loop.
        DomTreeNode* dtn = mPostDomTree->getNode(block);
        if (!dtn || !dtn->getIDom()) continue;

        BasicBlock* postDominatedBB = dtn->getIDom()->getBlock();

        Loop* pdLoop = mLoopInfo->getLoopFor(postDominatedBB);
        if (pdLoop && !WFV::hasMetadata(pdLoop, WFV::WFV_METADATA_LOOP_DIVERGENT_FALSE)) continue;

        if (WFV::hasMetadata(postDominatedBB, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) ||
            WFV::hasMetadata(postDominatedBB, WFV::WFV_METADATA_ALWAYS_BY_ALL_FALSE))
        {
            continue;
        }

        dtn = mDomTree->getNode(postDominatedBB);
        if (!dtn || !dtn->getIDom()) continue;

        if (dtn->getIDom()->getBlock() != block) continue;

        if(mVerbose) outs() << "  marking post dominator '" << postDominatedBB->getName() << "' as ABAON...\n";
        changed |= markValueAs(postDominatedBB, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE);
        stack.push_back(postDominatedBB);
        isFollowedPostDom = true;
    }

    return changed;
}
#ifdef WFV_ENABLE_LEGACY_API
// If parentBranch is non-nullptr, this means it is non-uniform.
// TODO: If we encounter a loop latch, we should first take the
//       backedge and finish the fixpoint iteration before recursing
//       into the exit block (#28).
void
VectorizationAnalysis::visitEdge(const BasicBlock* block,
                                 const BasicBlock* parent)
{
    assert (block);

    if(mVerbose) outs() << "\nvisitEdge(" << (parent ? parent->getName() : "ENTRY") << " -> "
            << block->getName() << ")\n";

    // This is a special case for the entry block to
    // make the rest of the code more readable.
    if (!parent)
    {
        mReachableEdgeMap[block]->mReachableEdges = new EdgeMapType();
        typedef GraphTraits<const BasicBlock* > BlockTraits;
        for (BlockTraits::ChildIteratorType S = BlockTraits::child_begin(block),
             SE = BlockTraits::child_end(block); S != SE; ++S)
        {
            const BasicBlock* succBB = *S;
            visitEdge(succBB, block);
        }
        return;
    }

    // Mark 'parent'->'block' edge as visited.
    if (mVisitedEdgeMap.count(parent))
    {
        mVisitedEdgeMap[parent]->insert(block);
    }
    else
    {
        SmallPtrSet<const BasicBlock*, 2>* newSet = new SmallPtrSet<const BasicBlock*, 2>();
        newSet->insert(block);
        mVisitedEdgeMap[parent] = newSet;
    }

    assert (isa<BranchInst>(parent->getTerminator()) &&
            "Support for switch statements not implemented yet (#33)!");
    const BranchInst* parentBranch = cast<BranchInst>(parent->getTerminator());

    DivergenceInfo&       info       = *mReachableEdgeMap[block];
    const DivergenceInfo& parentInfo = *mReachableEdgeMap[parent];
    const bool            firstVisit = info.mReachableEdges == nullptr;

    if (firstVisit)
    {
        if(mVerbose) outs() << "  block not seen before - reference set to parent.\n";
        assert (!info.mReference &&
                "a block that has never been visited must not have a reference!");
        assert (!info.mReachableEdges &&
                "a block that has never been visited must not have an edge map!");

        info.mReachableEdges = new EdgeMapType();
        info.mReference = parent;
    }

    EdgeMapType&       edgeMap       = *info.mReachableEdges;
    const EdgeMapType& parentEdgeMap = *parentInfo.mReachableEdges;

    const bool edgeIsVarying            =
        WFV::hasMetadata(parentBranch, WFV::WFV_METADATA_OP_VARYING) ||
        WFV::hasMetadata(parentBranch, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
        WFV::hasMetadata(parentBranch, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED);
    const bool hasUnseenNonBackEdgePred = !allPredsVisitedOrBackEdge(block);
    const bool hasUnseenSucc            = !allSuccessorsVisited(block);

    if(mVerbose) {
        outs() << "  info(" << block->getName()
            << "):\n" << info;
        outs() << "  parentInfo(" << parent->getName()
            << "):\n" << parentInfo;

        if (hasUnseenNonBackEdgePred) outs() << "  has unseen non-backedge predecessor(s)!\n";
        if (hasUnseenSucc) outs() << "  has unseen successor(s)!\n";
    }

    // 'changed' checks whether anything changed across all stages of
    // modification (update, join, removal).
    // However, if some of the phases cancel out, we treat this as if
    // there was no change to ensure termination.
    bool changed = false;

    EdgeMapType origEdgeMap(edgeMap);

    if (edgeIsVarying)
    {
        // Update the map with the additional incoming edge.
        // This is allowed not to do anything if the block has been visited before.
        const bool changedByUpdate = insertReachableEdge(edgeMap, parentBranch, block);
        if(mVerbose) if (changedByUpdate) outs() << "  updated map with varying incoming edge "
                << "from block '" << parent->getName() << "'\n";
        changed |= changedByUpdate;

        // If this map was referencing the map of a predecessor, it does not do so anymore.
        if(mVerbose) if (info.mReference) outs() << "    -> reference to block '"
                << info.mReference->getName() << "' removed!\n";
        info.mReference = nullptr;
    }

    const bool changedByJoin = joinEdgeMaps(edgeMap, parentEdgeMap);
    if(mVerbose) if (changedByJoin) outs() << "  joined map with incoming map.\n";
    changed |= changedByJoin;

    if(mVerbose) outs() << "  info(" << block->getName()
            << ") after update & join:\n" << info;

    // We throw away information about varying branches whose parent V is
    // post-dominated by the current block B (i.e. a third block C that
    // depends upon B can not be DIVERGENT due to the branch in V because
    // all paths to it must pass B and therefore can not be disjoint).
    const bool changedByRemoval = removePostDominatedEdges(block, info, *mPostDomTree);
    changed |= changedByRemoval;

    // If the current block references another one, all its predecessors have to
    // (transitively) reference the same block. Otherwise, remove the reference.
    // We have to do this even if the sets are equal (see unit test cases 8 and 9).
    if (info.mReference)
    {
        const BasicBlock* refBB       = getTransitiveReference(block);
        const BasicBlock* refBBParent = getTransitiveReference(parent);

        if (refBB != refBBParent)
        {
            if(mVerbose) outs() << "  block has different transitive reference ("
                    << refBB->getName() << ") than parent ("
                    << (refBBParent ? refBBParent->getName() : "nullptr")
                    << ") -> reference removed!\n";
            info.mReference = nullptr;
            changed = true;
        }
        else if (edgeMap != parentEdgeMap)
        {
            if(mVerbose) outs() << "  map does not match parent map anymore - "
                    << "reference to block '"
                    << info.mReference->getName() << "' removed!\n";
            info.mReference = nullptr;
            changed = true;
        }
        else
        {
            if(mVerbose) outs() << "  reference to block '"
                    << info.mReference->getName() << "' is still valid!\n";
        }
    }

    assert ((changed || (origEdgeMap == edgeMap)) && "maps must be equal if nothing changed!");

    if (changed && origEdgeMap == edgeMap)
    {
        if(mVerbose) {
            outs() << "  origEdgeMap:\n" << origEdgeMap;
            outs() << "  edgeMap:\n" << edgeMap;
            outs() << "  effects of update/join/removal cancelled out - nothing changed!\n";
        }

        changed = false;
    }

    // If nothing changed, there is only one case where we want to go on
    // iterating: If we have previously stopped iterating because we had
    // not seen all incoming edges to the current block.
    if (!changed && !hasUnseenSucc)
    {
        if(mVerbose) outs() << "  no change observed, all successors seen - recursion stopped.\n";
        return;
    }

    // If this block has multiple (non-backedge) entry-edges, don't recurse
    // before all edges have been visited.
    if (hasUnseenNonBackEdgePred)
    {
        // There is at least one predecessor that is not marked, so we have
        // to wait until we reach the current block again from that predecessor.
        if(mVerbose) outs() << "  block still has unseen incoming edges - recursion stopped.\n";
        return;
    }

    if(mVerbose) outs() << "  info(" << block->getName() << ") final:\n" << info;

    // Ensure correct fixpoint iteration: If this is a back edge,
    // remove the "visited" mark from all blocks of the loop.
    // By this, we enforce that every edge of the loop is visited
    // again.
    if (changed)
    {
        const Loop* loop = mLoopInfo->getLoopFor(block);
        if (loop && loop->getLoopLatch() == parent)
        {
            if(mVerbose) outs() << "  fixpoint at back edge not reached - loop has to be reiterated...\n";
            std::vector<BasicBlock*> loopBlocks = loop->getBlocks();
            for (auto &it : loopBlocks)
            {
                const BasicBlock* loopBlock = it;
                if (!mVisitedEdgeMap.count(loopBlock)) continue;
                delete mVisitedEdgeMap[loopBlock];
                mVisitedEdgeMap.erase(loopBlock);
            }
        }
    }

    //-----------------------------------------------------------------------//
    // Recurse into all successors (if any).
    //-----------------------------------------------------------------------//
    typedef GraphTraits<const BasicBlock* > BlockTraits;
    for (BlockTraits::ChildIteratorType S = BlockTraits::child_begin(block),
            SE = BlockTraits::child_end(block); S != SE; ++S)
    {
        const BasicBlock* succBB = *S;

        // Break critical loop back edges.
        if (succBB == block && !changed) continue;

        // If there was no change, only recurse into unseen edges.
        if (!changed &&
            mVisitedEdgeMap.count(block) &&
            mVisitedEdgeMap[block]->count(succBB))
        {
            continue;
        }

        visitEdge(succBB, block);
    }
}

// Join the path information of all incoming edges.
// Return true if edgeMap1 was changed.
bool
VectorizationAnalysis::joinEdgeMaps(EdgeMapType&       edgeMap1,
                                    const EdgeMapType& edgeMap2)
{
    bool changed = false;

    for (const auto &it2 : edgeMap2)
    {
        const BranchInst* branch = it2.first;
        const std::pair<bool, bool>& edges = it2.second;

        if (!edgeMap1.count(branch))
        {
            edgeMap1[branch] = edges;
            changed = true;
        }
        else
        {
            if (edges.first &&!edgeMap1[branch].first)
            {
                changed = true;
                edgeMap1[branch].first = true;
            }
            if (edges.second && !edgeMap1[branch].second)
            {
                changed = true;
                edgeMap1[branch].second = true;
            }
            // This is easier but does not allow to set the 'changed' flag:
            //edgeMap1[branch].first  |= edges.first;
            //edgeMap1[branch].second |= edges.second;
        }
    }

    return changed;
}

bool
VectorizationAnalysis::insertReachableEdge(EdgeMapType&      edgeMap,
                                           const BranchInst* branch,
                                           const BasicBlock* block)
{
    bool changed = false;

    // If the path already has information about this branch,
    // we must not remove any info but only update it.
    const bool branchExists = edgeMap.count(branch);

    // Which direction of predBB is block?
    if (branch->getSuccessor(0) == block)
    {
        if (branchExists)
        {
            changed = !edgeMap[branch].first;
            edgeMap[branch].first = true;
        }
        else
        {
            changed = true;
            edgeMap[branch] = std::make_pair(true, false);
        }
    }
    else
    {
        assert (branch->getSuccessor(1) == block);
        if (branchExists)
        {
            changed = !edgeMap[branch].second;
            edgeMap[branch].second = true;
        }
        else
        {
            changed = true;
            edgeMap[branch] = std::make_pair(false, true);
        }
    }

    return changed;
}

// We can throw away information about varying branches whose parent is
// post-dominated by the current block (i.e. any block after the current
// one can not be MANDATORY due to this branch because all paths must
// pass the current block and therefore can not be disjoint).
// If we remove an edge from this block, we have to make sure it does not
// reference another block anymore.
bool
VectorizationAnalysis::removePostDominatedEdges(const BasicBlock*        block,
                                                DivergenceInfo&          info,
                                                const PostDominatorTree& postDomTree)
{
    assert (block);

    EdgeMapType& edgeMap = *info.mReachableEdges;

    SmallVector<const BranchInst*, 4> eraseVec;

    for (const auto &it : edgeMap)
    {
        const BranchInst* branch = it.first;
        const BasicBlock* parent = branch->getParent();

        // Check if 'parent' is strictly post dominated by 'block'.
        // "Strictly" because we don't want to remove edges that came from the
        // block itself (via a loop back edge). This can lead to infinite loops
        // because there is never a fixpoint (e.g. TestSuite Loop9).
        const bool postDomFound = postDomTree.dominates(block, parent);// && block != parent;

        if (!postDomFound) continue;

        // Store edge for removal.
        eraseVec.push_back(branch);

        if(mVerbose) outs() << "  removed edges from post-dominated block: "
                << parent->getName() << "\n";
    }

    // Make sure we do not reference a block anymore.
    if (eraseVec.size() > 0)
    {
        info.mReference = nullptr;
    }

    for (auto &it : eraseVec)
    {
        edgeMap.erase(it);
    }

    return eraseVec.size() > 0;
}

bool
VectorizationAnalysis::allPredsVisitedOrBackEdge(const BasicBlock* block) const
{
    typedef GraphTraits<Inverse<const BasicBlock*> > InvBlockTraits;
    for (InvBlockTraits::ChildIteratorType P = InvBlockTraits::child_begin(block),
            PE = InvBlockTraits::child_end(block); P != PE; ++P)
    {
        const BasicBlock* predBB = *P;

        // If the predecessor is already marked, this block must have already
        // been updated with its reachable edges.
        ReachableEdgesMapType::const_iterator it = mReachableEdgeMap.find(predBB);
        if (it != mReachableEdgeMap.end() && it->second->mReachableEdges != nullptr) continue;

        // Check if the predecessor is the loop latch of the same loop that
        // this block is the header of.
        if (mLoopInfo->isLoopHeader(const_cast<BasicBlock*>(block)))
        {
            Loop* loop = mLoopInfo->getLoopFor(block);
            if (loop->getLoopLatch() == predBB) continue;
        }

        // The predecessor is neither marked nor a loop latch.
        return false;
    }

    // All predecessors are either marked or loop latches.
    return true;
}

bool
VectorizationAnalysis::allSuccessorsVisited(const BasicBlock* block) const
{
    VisitedEdgeMapType::const_iterator it = mVisitedEdgeMap.find(block);
    if (it == mVisitedEdgeMap.end()) return false;

    const SmallPtrSet<const BasicBlock*, 2>& visitedEdges = *it->second;

    typedef GraphTraits<const BasicBlock*> BlockTraits;
    for (BlockTraits::ChildIteratorType S = BlockTraits::child_begin(block),
            SE = BlockTraits::child_end(block); S != SE; ++S)
    {
        const BasicBlock* succBB = *S;

        if (!visitedEdges.count(succBB)) return false;
    }

    return true;
}

const BasicBlock*
VectorizationAnalysis::getTransitiveReference(const BasicBlock* block) const
{
    assert (block);
    assert (mReachableEdgeMap.find(block) != mReachableEdgeMap.end());

    DivergenceInfo* info = mReachableEdgeMap.find(block)->second;
    if (!info->mReference) return block;

    while (true)
    {
        assert (mReachableEdgeMap.find(info->mReference) != mReachableEdgeMap.end());
        DivergenceInfo* tmpInfo = mReachableEdgeMap.find(info->mReference)->second;
        if (!tmpInfo->mReference) break;
        info = tmpInfo;
    }

    return info->mReference;
}

#endif

// A block b is marked as MANDATORY if
// 1. b is a direct successor of another block v that ends with a varying branch, or
// 2. b is DIVERGENT, or
// 3. b is a latch of a DIVERGENT loop, or
// 4. b is an exit block of a loop l with loop latch e. v is another block of
//    the same loop that ends with a varying branch. There exist two *disjoint*
//    paths p1 and p2 starting at the true and false edge of v, respectively,
//    that do not include the back edge of l. p1 goes from v to b while p2 goes
//    from v to e.
// NOTE: Case 4 is not fully exploited in CFG linearization.
bool
VectorizationAnalysis::isMandatory(const BasicBlock* block,
                                   const LoopInfo&   loopInfo) const
{
    assert (block);
    if(mVerbose) outs() << "\nisMandatory(" << block->getName() << ")\n";

    bool mandatory = false;

    //-----------------------------------------------------------------------//
    // 1. b is a direct successor of v.
    //-----------------------------------------------------------------------//
    for (const_pred_iterator P=pred_begin(block), PE=pred_end(block); P!=PE; ++P)
    {
        const BasicBlock* v = *P;

        // The branch is varying if the terminator is not OP_UNIFORM.
        // This is a little more safe than just checking for OP_VARYING
        // although at this point there should be no OP_SEQUENTIAL and
        // no unmarked terminators.
        if (!WFV::hasMetadata(v->getTerminator(),
                              WFV::WFV_METADATA_OP_UNIFORM))
        {
            if(mVerbose) {
                outs() << "  Block '" << block->getName() << "' is MANDATORY (1)!\n";
                outs() << "    due to predecessor '" << v->getName() << "'.\n";
            }

            WFV::addRewireTargetForDCBlock(const_cast<BasicBlock*>(v), const_cast<BasicBlock*>(block));
            mandatory = true;
        }
    }

    //-----------------------------------------------------------------------//
    // 2. b is a divergent block.
    //-----------------------------------------------------------------------//
    if (WFV::hasMetadata(block, WFV::WFV_METADATA_DIVERGENT_TRUE))
    {
        if(mVerbose) {
            outs() << "  Block '" << block->getName() << "' is MANDATORY (2)!\n";
            outs() << "    due to divergent blocks:";
        }

        SmallVector<Value*, 2> divergenceCausingBlocks;
        WFV::getDivergenceCausingBlocks(*block, divergenceCausingBlocks);
        for (auto &dcBB : divergenceCausingBlocks)
        {
            assert (isa<BasicBlock>(dcBB));
            if(mVerbose) {
                outs() << " '" << dcBB->getName() << "'";
            }
            WFV::addRewireTargetForDCBlock(cast<BasicBlock>(dcBB), const_cast<BasicBlock*>(block));
        }
        if(mVerbose) outs() << "\n";
        mandatory = true;
    }

    //-----------------------------------------------------------------------//
    // 3. b is a latch of a DIVERGENT loop
    //-----------------------------------------------------------------------//
    Loop* loop = loopInfo.getLoopFor(block);
    if (loop)
    {
        if (block == loop->getLoopLatch() &&
            WFV::hasMetadata(loop, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE))
        {
            if(mVerbose) outs() << "  Block '" << block->getName() << "' is MANDATORY (3)!\n";
            // Rewire targets are all blocks that cause exits to be MANDATORY.
            // Since it is easier to add this information when such an exit is found, we do it
            // there.
            mandatory = true;
        }
    }

    //-----------------------------------------------------------------------//
    // 4. b is an exit block of a loop l with loop latch e. v is another block of
    //    the same loop that ends with a varying branch. There exist two *disjoint*
    //    paths p1 and p2 starting at the true and false edge of v, respectively,
    //    that do not include the back edge of l. p1 goes from v to b while p2 goes
    //    from v to e.
    //-----------------------------------------------------------------------//
    // Check if this block is an exit block of a loop. This can never be the case
    // if it has more than one or no predecessor (thanks to loop simplification).
    const BasicBlock* exitBlockPredBB = block->getUniquePredecessor();
    if (exitBlockPredBB)
    {
        // Obviously, if the predecessor is not in a different loop than the current
        // block, this is not a loop exit.
        Loop* predLoop = loopInfo.getLoopFor(exitBlockPredBB);
        if (predLoop && predLoop != loop)
        {
            // We have to test the criterion for all nested loops that this block is an exit of.
            while (predLoop)
            {
                if (!predLoop->isLoopExiting(exitBlockPredBB)) break;
                if (isMandatoryExit(block, predLoop)) mandatory = true;
                predLoop = predLoop->getParentLoop();
            }
        }
    }

    return mandatory;
}

bool
VectorizationAnalysis::isMandatoryExit(const BasicBlock* block,
                                       const Loop*       loop) const
{
    if (mDisableControlFlowDivAnalysis)
    {
        if (WFV::hasMetadata(loop, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE))
        {
            if(mVerbose) outs() << "  Loop '" << block->getName()
                << "' is DIVERGENT, exit block is MANDATORY "
                << "(mDisableDivergenceAnalysis set)!\n";
                return true;
        }
        return false;
    }

    const BasicBlock* exitBlockPredBB = block->getUniquePredecessor();

    // If the predecessor block is the loop latch, all paths from varying branches
    // inside the loop by definition can not have disjoint paths to the latch and
    // the block, so the block can not be MANDATORY due to this criterion.
    const BasicBlock* latch = loop->getLoopLatch();
    if(mVerbose) if (exitBlockPredBB == latch) outs() << "    exiting block is the latch itself "
            << "- no candidate for divergence criterion (4).\n";
    if (exitBlockPredBB == latch) return false;

    // Check all edges in the set of the predecessor for one
    // 1. whose parent is inside the loop, and
    // 2. whose other edge reaches the latch.
    // Test case for criterion 2: TestDivergenceAnalysisLoop2.

#ifdef VA_VERIFY
    BlockSet divCauseBlocks;
    bool newMandatory = checkForDisjointLoopPaths(block, loop, &divCauseBlocks);
#else
    bool newMandatory = checkForDisjointLoopPaths(block, loop, nullptr);
#endif

#ifdef VA_VERIFY
    assert (mReachableEdgeMap.count(block));
    assert (mReachableEdgeMap.find(block)->second);
    assert (mReachableEdgeMap.find(block)->second->mReachableEdges);
    const EdgeMapType* exitEdgeMap =
        mReachableEdgeMap.find(block)->second->mReachableEdges;

    assert (mReachableEdgeMap.count(latch));
    assert (mReachableEdgeMap.find(latch)->second);
    assert (mReachableEdgeMap.find(latch)->second->mReachableEdges);
    const EdgeMapType* latchEdgeMap =
        mReachableEdgeMap.find(latch)->second->mReachableEdges;

    if(mVerbose) {
        outs() << "  exitEdgeMap(" << block->getName()
            << "):\n" << *exitEdgeMap << "\n";
        outs() << "  latchEdgeMap(" << latch->getName()
            << "):\n" << *latchEdgeMap << "\n";
    }

    if (exitEdgeMap == latchEdgeMap)
    {
        if(mVerbose) outs() << "  both edges refer to the same incoming set "
            << "- no divergence (4)!\n";
        return false;
    }
    BlockSet altDivCauseBlocks;
    bool mandatory = false;
    for (const auto &it : *exitEdgeMap)
    {
        const BranchInst*            branch = it.first;
        const std::pair<bool, bool>& edges  = it.second;
        const BasicBlock*            parent = branch->getParent();
        if(mVerbose) outs() << "  varying branch in block: " << parent->getName() << "\n";

        assert ((edges.first || edges.second) &&
                "Invariant violated: at least one edge must be reachable!");

        // Criterion 1.
        if(mVerbose) if (!loop->contains(parent)) outs() << "    parent is not in same loop "
                << "- no candidate for divergence criterion (4).\n";
        if (!loop->contains(parent)) continue;

        // Criterion 2.
        // Check if there is a disjoint path that leads from the other edge to the latch.
        if (!latchEdgeMap->count(branch))
        {
            if(mVerbose) outs() << "  branch does not reach latch "
                    << "- no divergence (4)!\n";
            continue;
        }

        EdgeMapType::const_iterator it2 = latchEdgeMap->find(branch);
        if (it2 == latchEdgeMap->end()) continue;

        const bool exitTrue   = edges.first;
        const bool latchTrue  = it2->second.first;
        const bool exitFalse  = edges.second;
        const bool latchFalse = it2->second.second;
        if(mVerbose) {
            outs() << (exitTrue   ? "   - exitTrue\n" : "");
            outs() << (latchTrue  ? "   - latchTrue\n" : "");
            outs() << (exitFalse  ? "   - exitFalse\n" : "");
            outs() << (latchFalse ? "   - latchFalse\n" : "");
        }

        const bool case1 = exitTrue && latchFalse;
        const bool case2 = exitFalse && latchTrue;
        if (!case1 && !case2) continue;

        // If only one of the two cases exists, we can not conclude that the
        // paths can not overlap. This is because a case can exist because of
        // outer loops, which we must not take into account for this criterion.
        // (See nsmx09 for an example where a block would be marked MANDATORY
        // although it is OPTIONAL.)
        //if (case1 ^ case2)
        //{
            //DEBUG_VA( outs() << "  Block '" << block->getName() << "' is MANDATORY (5)!\n"; );
            //return true;
        //}
        //assert (case1 && case2);

        // Now we have to make sure that the potentially disjoint paths are
        // actually disjoint without using back edges of outer loops etc.

        // The exit block is MANDATORY if it is reachable from the varying
        // branch without passing the latch of the current loop (latches of
        // inner loops are allowed) and without passing a block where all
        // instances join that diverged at that branch and are still active
        // in the loop (= disjoint paths).

        // Implement:
        // - new isReachable() that stops at the latch and collects paths.
        // - If more than one path from branch to exit is found, check if any
        //   block appears on *all* paths. If none, or if only one path is
        //   found, the exit is MANDATORY.
        // NOTE: I think the above is what is implemented below ;).

        WFV::PathVecType pathsToExit;
        WFV::PathVecType pathsToLatch;

        SmallPtrSet<const BasicBlock*, 16> visitedBlocks;
        WFV::PathType currentPath;
        visitedBlocks.insert(latch); // Do not pass the latch.
        WFV::collectLoopPaths(block,
                              parent,
                              loop,
                              currentPath,
                              pathsToExit,
                              visitedBlocks);
        if(mVerbose) {
            outs() << "\n  Paths to exit:\n";
            printPaths(pathsToExit, outs()); outs() << "\n";
        }

        // If no path to the exit was found, this means they all go through the
        // latch and thus overlap with any path to the latch. Thus, the block
        // is not MANDATORY due to this varying branch.
        if (pathsToExit.empty()) continue;

        // Otherwise, also collect paths to the latch and check if there is any
        // pair of paths p1 \in pathsToExit, p2 \in pathsToLatch that are disjoint.
        visitedBlocks.clear();
        currentPath.clear();
        WFV::collectLoopPaths(latch,
                              parent,
                              loop,
                              currentPath,
                              pathsToLatch,
                              visitedBlocks);

        if(mVerbose) {
            outs() << "\n  Paths to latch:\n";
            printPaths(pathsToLatch, outs()); outs() << "\n";
        }

        if (hasDisjointPaths(pathsToExit, pathsToLatch, true /* ignoreStartBlock */))
        {
            if(mVerbose) {
                outs() << "  Block '" << block->getName() << "' is MANDATORY (4)!\n";
                outs() << "    due to exit '" << parent->getName() << "'.\n";
            }

            // Add both the current block as well as the latch as rewire targets for the branch parent.
            WFV::addRewireTargetForDCBlock(const_cast<BasicBlock*>(parent), const_cast<BasicBlock*>(block));
            WFV::addRewireTargetForDCBlock(const_cast<BasicBlock*>(parent), const_cast<BasicBlock*>(latch));
            altDivCauseBlocks.insert(const_cast<BasicBlock*>(block));
            mandatory = true;
        }
    } // for all edges that reach this block


    // DEBUG--
    if (newMandatory != mandatory) {
    	bool unequalSets = false;
		errs() << "Evaluating sets of divergence causing blocks in loops\n";
		errs() << "Missing blocks {";
		for (auto altBlock : altDivCauseBlocks) {
			if (! divCauseBlocks.count(static_cast<BasicBlock*>(altBlock))) {
				errs() << altBlock->getName() << ", ";
				unequalSets = true;
			}
		}
		errs() << "}\n";

		errs() << "Invalid blocks {";
		for (auto newBlock : divCauseBlocks) {
			if (! altDivCauseBlocks.count(static_cast<BasicBlock*>(newBlock))) {
				errs() << newBlock->getName() << ", ";
				unequalSets = true;
			}
		}
		errs() << "}\n";

		assert(! unequalSets && "sets of divergence causing blocks do not agree!");
	}


    assert(! newMandatory || mandatory); // FIXME testing too many exitEdges
    assert(newMandatory || !mandatory);
    // -- DEBUG
#else
    bool mandatory = newMandatory;
#endif
    if(mVerbose)
        if (!mandatory)
            outs() << "  reachable edges do not fulfill loop exit criterion " << "- no divergence (4)!\n";
    return mandatory;
}

#ifdef WFV_ENABLE_LEGACY_API
bool
VectorizationAnalysis::hasDisjointPaths(const WFV::PathVecType& pathsA,
                                        const WFV::PathVecType& pathsB,
                                        const bool              ignoreStartBlock) const
{
    for (const auto &pathA : pathsA)
    {
        for (const auto &pathB : pathsB)
        {
            if (pathsAreDisjoint(*pathA, *pathB, ignoreStartBlock)) {
#ifdef VERBOSE
            	llvm::errs() << "Found disjoint path:\n";
				WFV::DumpPath(*pathA);
				WFV::DumpPath(*pathB);
#endif // VA_VERIFY
            	return true;
            }
        }
    }

    return false;
}

bool
VectorizationAnalysis::pathsAreDisjoint(const WFV::PathType& pathA,
                                        const WFV::PathType& pathB,
                                        const bool           ignoreStartBlock) const
{
    for (const auto &pathABlock : pathA)
    {
        if (ignoreStartBlock && pathABlock == *pathA.begin()) continue;
        for (const auto &pathBBlock : pathB)
        {
            if (ignoreStartBlock && pathBBlock == *pathB.begin()) continue;
            if (pathABlock == pathBBlock) return false;
        }
    }

    return true;
}
#endif

// Mark phis in divergent blocks as OP_VARYING / RES_VECTOR.
// Mark LCSSA phis of divergent loops as OP_UNIFORM / RES_VECTOR.
// Returns true if some mark was set, false otherwise.
bool
VectorizationAnalysis::updateUniformPhisWithDivergenceInfo(Function* scalarFn)
{
    bool changed = false;

    for (auto &BB : *scalarFn)
    {
        BasicBlock* block = &BB;

        // Ignore non-divergent and non-optional divergent-loop exit blocks.
        const bool isExitOfDivergentLoop = WFV::isExitOfDivergentLoop(*block, *mLoopInfo);
        if (!WFV::hasMetadata(block, WFV::WFV_METADATA_DIVERGENT_TRUE) &&
            !isExitOfDivergentLoop)
        {
            continue;
        }

        for (auto &I : BB)
        {
            Instruction* inst = &I;

            // We are only interested in phis.
            if (inst == &*block->getFirstInsertionPt()) break;
            assert (isa<PHINode>(inst));
            assert (!isExitOfDivergentLoop || cast<PHINode>(inst)->getNumIncomingValues() == 1);

            if (mValueInfoMap->hasMapping(*inst)) continue;

            // Ignore all phis that are already marked as VARYING/SEQUENTIAL.
            if (WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_VARYING) ||
                WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
                WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                continue;
            }

            if(mVerbose) outs() << "  updating phi: " << *inst << "\n";

            // Mark phis in divergent blocks as OP_VARYING/RES_VECTOR.
            // Mark LCSSA phis of divergent loops as RES_VECTOR only.
            changed |= recursivelyMarkVarying(inst, isExitOfDivergentLoop ? nullptr : block);
        }
    }

    return changed;
}

// Mark operations with side-effects depending on the properties of their parent block.
// Returns true if some mark was set, false otherwise.
// This is crap: Operations with side effects must NEVER be uniform in the first place!
//               They actually should be considered as inputs if they return a value!!
bool
VectorizationAnalysis::updateUniformSideEffectOperations(Function* scalarFn)
{
    bool changed = false;

    for (auto &BB : *scalarFn)
    {
        const bool hasFullyUniformMask =
            WFV::hasMetadata(&BB, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) ||
            WFV::hasMetadata(&BB, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE);

        for (auto &I : BB)
        {
            Instruction* inst = &I;

            if (mValueInfoMap->hasMapping(*inst)) continue;

            // We are only interested in operations with side effects.
            if (!WFV::mayHaveSideEffects(*inst, mFunctionInfoMap)) continue;

            // Loads can be left uniform.
            if (isa<LoadInst>(inst)) continue;

            // Stores in blocks with fully uniform mask can be left uniform.
            if (hasFullyUniformMask && isa<StoreInst>(inst)) continue;

            // Ignore our own metadata calls.
            if (WFV::isMetadataCall(inst)) continue;

            // Ignore all operations that are already marked as VARYING/SEQUENTIAL.
            if (WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_VARYING) ||
                WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
                WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                continue;
            }


            // Instruction which may have side effects is in a block that can't
            // be proven to be executed by all or no instances. Thus, it can't
            // be uniform (we mark it as OP_SEQUENTIAL/_GUARDED during split
            // analysis later).
            if(mVerbose) outs() << "  updating operation with possible side effects: "
                << *inst << "\n";

            // Mark the instruction as OP_VARYING / RES_VECTOR.
            changed |= recursivelyMarkVarying(inst, &BB);
        }
    }

    return changed;
}

// Update UNIFORM/VARYING information of alloca operations.
// The marks of alloca's, in contrast to other instructions, depend upon how
// they are used. If an alloca has VARYING uses, the alloca has to be VARYING
// as well. Also, if a store or call use of an alloca is inside divergent
// control flow (i.e. its parent block is not ABA or ABAON), the alloca has to
// be VARYING.
bool
VectorizationAnalysis::updateUniformAllocas(Function* scalarFn)
{
    bool changed = false;

    for (auto &BB : *scalarFn)
    {
        BasicBlock* block = &BB;

        for (auto &I : BB)
        {
            Instruction* inst = &I;

            // We are only interested in allocas.
            if (!isa<AllocaInst>(inst)) continue;

            // Ignore all allocas that are already marked as VARYING/SEQUENTIAL.
            if (WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_VARYING) ||
                WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
                WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                continue;
            }

            if (mValueInfoMap->hasMapping(*inst)) continue;

            // If the alloca has a VARYING use, it is VARYING as well.
            // If the alloca has a store/call use in a block that is neither ABA
            // nor ABAON, it is VARYING.
            for (Instruction::user_iterator U=inst->user_begin(),
                 UE=inst->user_end(); U!=UE; ++U)
            {
                assert (isa<Instruction>(*U) && "all uses have to be instructions (!?)");
                Instruction* useI = cast<Instruction>(*U);

                if (WFV::hasMetadata(useI, WFV::WFV_METADATA_OP_UNIFORM))
                {
                    if (!isa<StoreInst>(useI) && !isa<CallInst>(useI)) continue;

                    BasicBlock* useBB = useI->getParent();
                    if (WFV::hasMetadata(useBB, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) ||
                        WFV::hasMetadata(useBB, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE))
                    {
                        continue;
                    }
                }

                if(mVerbose) outs() << "  updating alloca: " << *inst << "\n";

                // Mark the alloca as OP_VARYING / RES_VECTOR.
                changed |= recursivelyMarkVarying(inst, block);

                break;
            }
        }
    }

    return changed;
}

// Mark phis in headers of divergent loops as RES_VARYING.
// Returns true if some mark was set, false otherwise.
// TODO: Don't we introduce result phis exactly for this reason?!
bool
VectorizationAnalysis::updateUniformLALBValues(Function* scalarFn)
{
    bool changed = false;

    for (auto &L : *mLoopInfo)
    {
        changed |= updateUniformLALBValues(L);
    }

    return changed;
}

bool
VectorizationAnalysis::updateUniformLALBValues(Loop* loop)
{
    assert (loop);

    bool changed = false;

    for (auto &SL : *loop)
    {
        changed |= updateUniformLALBValues(SL);
    }

    if (WFV::hasMetadata(loop, WFV::WFV_METADATA_LOOP_DIVERGENT_FALSE)) return changed;

    for (Loop::block_iterator BB=loop->block_begin(); BB!=loop->block_end(); ++BB)
    {
        BasicBlock* curBB = *BB;

        for (BasicBlock::iterator I=curBB->begin(); I!=curBB->end(); ++I)
        {
            // Ignore instructions that are already marked as VARYING/SEQUENTIAL.
            if (WFV::hasMetadata(&*I, WFV::WFV_METADATA_OP_VARYING) ||
                WFV::hasMetadata(&*I, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
                WFV::hasMetadata(&*I, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                continue;
            }
            // Ignore phis that are already marked as RES_VECTOR.
            if (isa<PHINode>(&*I) &&
                (WFV::hasMetadata(&*I, WFV::WFV_METADATA_RES_VECTOR) ||
                 WFV::hasMetadata(&*I, WFV::WFV_METADATA_RES_SCALARS)))
            {
                continue;
            }

            if (mValueInfoMap->hasMapping(*I)) continue;

            // Find a use of this instruction outside the loop.
            bool outsideUseFound = false;
            Instruction* outsideUse = nullptr;
            for (Instruction::user_iterator U=I->user_begin(),
                    UE=I->user_end(); U!=UE && !outsideUseFound; ++U)
            {
                assert (isa<Instruction>(*U) && "all uses have to be instructions (!?)");
                Instruction* useI = cast<Instruction>(*U);
                if (!loop->contains(useI->getParent()))
                {
                    outsideUseFound = true;
                }
#if 0
                // This is too conservative (#23).
                if (isa<PHINode>(useI) &&
                    useI->getParent() == loop->getHeader())
                {
                    outsideUseFound = true;
                }
#endif
                if (outsideUseFound)
                {
                    if(mVerbose) {
                        outs() << "  found loop live value: " << *I << "\n";
                        outs() << "    with use outside loop-boundary: " << *useI << "\n";
                    }
                    outsideUse = useI;
                }
            }
            if (!outsideUseFound) continue;

            // Find a phi in the header of this loop that this instruction depends on.
            // If we find such a phi, we have to expect this value to change in every
            // iteration, so it must be varying.
            PHINode* phi = findLoopPhiForInstruction(&*I, loop);
            if (!phi)
            {
                if(mVerbose) outs() << "  does not depend upon loop iterations!\n";
                continue;
            }

            if (isa<PHINode>(outsideUse))
            {
                // Mark the phi as RES_VECTOR only.
                // TODO: Are there cases where we have to mark a phi as OP_VARYING here?
                changed |= recursivelyMarkVarying(outsideUse, nullptr);
            }
            else
            {
                // Mark the instruction as OP_VARYING / RES_VECTOR.
                changed |= recursivelyMarkVarying(outsideUse, curBB);
            }
        }
    }

    return changed;
}

PHINode*
VectorizationAnalysis::findLoopPhiForInstruction(Instruction* inst, Loop* loop)
{
    BasicBlock* parentBB = inst->getParent();
    if (!loop->contains(parentBB)) return nullptr;

    BasicBlock* headerBB = loop->getHeader();

    // If we are in the header, see if 'inst' is one of the
    // header's phi-instructions. If that is the case, we
    // have our connection. Otherwise we go on iterating over
    // operands until we have left the loop.
    if (headerBB == parentBB)
    {
        for (BasicBlock::iterator I=headerBB->begin(); I!=headerBB->end(); ++I)
        {
            if (headerBB->getFirstInsertionPt() == I) break;
            if (inst == &*I) return cast<PHINode>(I);
        }
    }

    // Recursively search for a phi.
    for (Instruction::op_iterator OP=inst->op_begin(); OP!=inst->op_end(); ++OP)
    {
        if (!isa<Instruction>(OP)) continue;
        Instruction*  opI = cast<Instruction>(OP);
        if (!loop->contains(opI->getParent())) continue;

        if (PHINode* phi = findLoopPhiForInstruction(opI, loop)) return phi;
    }

    return nullptr;
}


bool
VectorizationAnalysis::markDivergentLoops(const LoopInfo& loopInfo)
{
    bool changed = false;
    for (auto &L : loopInfo)
    {
        changed |= markDivergentLoop(L, loopInfo);
    }
    return changed;
}

bool
VectorizationAnalysis::markDivergentLoop(Loop* loop, const LoopInfo& loopInfo)
{
    assert (loop);

    bool changed = false;

    if (mDisableControlFlowDivAnalysis)
    {
        changed = markValueAs(loop->getHeader(), WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE);

        // We have to mark all exit branch operations as OP_VARYING.
        SmallVector<BasicBlock*, 2> exitingBlocks;
        loop->getExitingBlocks(exitingBlocks);

        for (auto &BB : exitingBlocks)
        {
            TerminatorInst* terminator = BB->getTerminator();
            markValueAs(terminator, WFV::WFV_METADATA_OP_VARYING);
        }

        for (auto &SL : *loop)
        {
            changed |= markDivergentLoop(SL, loopInfo);
        }
    }
    else
    {
        changed = markValueAs(loop->getHeader(),
                              isLoopDivergent(loop, loopInfo) ?
                                  WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE :
                                  WFV::WFV_METADATA_LOOP_DIVERGENT_FALSE);

        for (auto &SL : *loop)
        {
            changed |= markDivergentLoop(SL, loopInfo);
        }
    }

    return changed;
}

// A loop is divergent if there is a block in the loop that ends with a
// varying branch and is not strictly post-dominated by another block that
// belongs to the same loop.
bool
VectorizationAnalysis::isLoopDivergent(Loop* loop, const LoopInfo& loopInfo)
{
    for (auto &it : loop->getBlocks())
    {
        BasicBlock* block = it;

        const TerminatorInst* terminator = block->getTerminator();
        if (WFV::hasMetadata(terminator, WFV::WFV_METADATA_OP_UNIFORM)) continue;

        assert (mPostDomTree->getNode(block));
        assert (mPostDomTree->getNode(block)->getIDom());
        const BasicBlock* postDom = mPostDomTree->getNode(block)->getIDom()->getBlock();
        assert (postDom);
        if (loop->contains(postDom)) continue;

        // The block ends with a varying branch and is not strictly post-dominated by
        // the latch -> the loop is divergent.
        return true;
    }

    // All blocks either end with a uniform branch or are strictly post-dominated by
    // the loop latch, so the loop is non-divergent.
    return false;
}

// We have to be sure to create loop exit masks for every nested divergent
// loop. Therefore, we first mark all those loops that are divergent
// and "top-level", meaning they are not nested or nested directly
// inside a non-divergent loop.
// EXAMPLE: divergent loop inside non-divergent loop inside divergent loop.
void
VectorizationAnalysis::markNestedDivergentTopLevelLoops()
{
    for (LoopInfo::iterator L=mLoopInfo->begin(), LE=mLoopInfo->end(); L!=LE; ++L)
    {
        Loop* loop = *L;
        if (WFV::hasMetadata(loop, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE))
        {
            markValueAs(loop->getHeader(), WFV::WFV_METADATA_LOOP_TOP_LEVEL_DIVERGENT);
            if (loop->getSubLoops().empty())
            {
                markValueAs(loop->getHeader(), WFV::WFV_METADATA_LOOP_INNERMOST_DIVERGENT);
            }
        }
        markNestedDivergentTopLevelLoops(loop);
    }
}

// We must not generate mask operations for sub loops before their parents,
// so we only mark the "top-level" divergent loops (= at uniform loops, add
// all divergent sub loops, at divergent loops, do not add any direct sub loop but
// continue recursion into them).
// NOTE: Should only be called through markNestedDivergentTopLevelLoops().
void
VectorizationAnalysis::markNestedDivergentTopLevelLoops(Loop* loop)
{
    const bool loopIsUniform =
        WFV::hasMetadata(loop, WFV::WFV_METADATA_LOOP_DIVERGENT_FALSE);

    bool allSubLoopsUniform = true;
    for (Loop::iterator SL=loop->begin(); SL != loop->end(); ++SL)
    {
        Loop* subLoop = *SL;
        const bool subLoopIsUniform =
            WFV::hasMetadata(subLoop, WFV::WFV_METADATA_LOOP_DIVERGENT_FALSE);

        if (loopIsUniform && !subLoopIsUniform)
        {
            // DIVERGENT inner loop inside NON_DIVERGENT outer loop
            // -> mark inner loop as "top level"
            markValueAs(subLoop->getHeader(),
                        WFV::WFV_METADATA_LOOP_TOP_LEVEL_DIVERGENT);
            allSubLoopsUniform = false;
        }
        markNestedDivergentTopLevelLoops(subLoop);
    }

    if (!loopIsUniform && allSubLoopsUniform)
    {
        // Only UNIFORM inner loops inside DIVERGENT outer loop.
        // -> Mark outer loop as "innermost".
        // TODO: This is conservative, but can we gain something from
        //       correctly handling mixed non-divergent/divergent inner loops
        //       (same nesting level)?
        markValueAs(loop->getHeader(), WFV::WFV_METADATA_LOOP_INNERMOST_DIVERGENT);
    }
}

////////////////////////////////////////////////////////////////////////////
//                       INDEX & ALIASING ANALYSIS                        //
////////////////////////////////////////////////////////////////////////////

// IMPORTANT: Index and alignment info always refer to the result of the
//            operation! This means that if we want to know whether we can
//            use a vector load instead of a gather, we have to look at the
//            indices of the GEP instruction if there is one.

// Marking of same/consecutive/random and aligned/unaligned works differently
// than uniform/varying: We cannot simply mark everything and then remove
// marks but rather have to only mark what we can prove.
// Therefore, we start by marking uniform values as SAME and user-defined
// values according to the input.
// Only then we start recursively marking the rest of the function.
void
VectorizationAnalysis::analyzeConsecutiveAlignedInfo(Function* scalarFn)
{

    SmallPtrSet<Value*, 32> markedValues;

    // Add all instructions that were marked from outside.
    for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
    {
        Instruction* inst = &*I;
        if (mValueInfoMap->hasMapping(*inst)) markedValues.insert(inst);
    }

    if (mDisableMemAccessAnalysis)
    {
        for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
        {
            Instruction* inst = &*I;
            if (inst->getType()->isVoidTy()) continue;
            if (mValueInfoMap->hasMapping(*inst)) continue;
            if (WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_UNIFORM)) // TODO: Shouldn't this test for RES_UNIFORM?
            {
                WFV::setMetadata(inst, WFV::WFV_METADATA_INDEX_SAME);
            }
            else
            {
                WFV::setMetadata(inst, WFV::WFV_METADATA_INDEX_RANDOM);
            }
            WFV::setMetadata(inst, WFV::WFV_METADATA_ALIGNED_FALSE);
        }
        return;
    }

    // Mark all arguments.
    // Mark arguments that were marked from outside accordingly.
    // Mark uniform arguments that are pointers as INDEX_SAME / ALIGN_TRUE.
    // Mark varying pointer arguments as INDEX_CONSECUTIVE / ALIGN_TRUE.
    // These are requirements for the caller of the vectorized function.
    if(mVerbose) outs() << "\nmarking index/alignment info of arguments...\n";
    for (Function::arg_iterator A=scalarFn->arg_begin(),
            AE=scalarFn->arg_end(); A!=AE; ++A)
    {
        if (mValueInfoMap->hasMapping(*A))
        {
            assert (WFV::hasMetadata(&*A, WFV::WFV_METADATA_INDEX_SAME) ||
                    WFV::hasMetadata(&*A, WFV::WFV_METADATA_INDEX_CONSECUTIVE) ||
                    WFV::hasMetadata(&*A, WFV::WFV_METADATA_INDEX_RANDOM));
            assert (WFV::hasMetadata(&*A, WFV::WFV_METADATA_ALIGNED_TRUE) ||
                    WFV::hasMetadata(&*A, WFV::WFV_METADATA_ALIGNED_FALSE));
            markedValues.insert(&*A);
            if(mVerbose) outs() << "argument already marked from outside: " << *A << "\n";
            continue;
        }

        if (WFV::hasMetadata(&*A, WFV::WFV_METADATA_RES_UNIFORM))
        {
            WFV::setMetadata(&*A, WFV::WFV_METADATA_INDEX_SAME);
            WFV::setMetadata(&*A,
                             A->getType()->isPointerTy() ?
                                 WFV::WFV_METADATA_ALIGNED_TRUE :
                                 WFV::WFV_METADATA_ALIGNED_FALSE);
            if(mVerbose)
                outs() << "marked UNIFORM argument: " << *A << " as "
                    << "INDEX_SAME / "
                    << (A->getType()->isPointerTy() ?
                        "ALIGNED_TRUE" : "ALIGNED_FALSE") << "!\n";
        }
        else
        {
            WFV::setMetadata(&*A,
                             A->getType()->isPointerTy() ?
                                 WFV::WFV_METADATA_INDEX_CONSECUTIVE :
                                 WFV::WFV_METADATA_INDEX_RANDOM);
            WFV::setMetadata(&*A,
                             A->getType()->isPointerTy() ?
                                 WFV::WFV_METADATA_ALIGNED_TRUE :
                                 WFV::WFV_METADATA_ALIGNED_FALSE);

            if(mVerbose)
                outs() << "marked VARYING argument: " << *A << " as "
                    << (A->getType()->isPointerTy() ?
                    "INDEX_CONSECUTIVE / ALIGNED_TRUE!\n" :
                    "INDEX_RANDOM / ALIGNED_FALSE!\n");
        }
        markedValues.insert(&*A);
    }

    //
    // Collect all "outputs" of the function and conditional branches
    // as starting points for a post-reversed DFS.
    //

    SmallPtrSet<Instruction*, 32> workSet;

    // If the function returns something, all returns are outputs :).
    // Otherwise, ignore.
    if (!scalarFn->getFunctionType()->getReturnType()->isVoidTy())
    {
        SmallPtrSet<BasicBlock*, 2> returnBlocks;
        WFV::findReturnBlocks(*scalarFn, returnBlocks);
        for (auto BB : returnBlocks)
        {
            workSet.insert(BB->getTerminator());
        }
    }

    // All calls and stores have to count as outputs.
    // All conditional branches also have to be added to workSet because
    // they depend on other values that have to be marked.
    // NOTE: Adding calls is not enough: they might have been added by the
    //       user and thus are marked already. We have to add their operands.
    for (Function::iterator BB=scalarFn->begin(), BBE=scalarFn->end(); BB!=BBE; ++BB)
    {
        for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I)
        {
            if (!isa<CallInst>(I) &&
                !isa<StoreInst>(I) &&
                !isa<BranchInst>(I))
            {
                continue;
            }

            // Ignore our own metadata calls.
            if (WFV::isMetadataCall(&*I)) continue;

            if (BranchInst* br = dyn_cast <BranchInst>(I))
            {
                if (br->isUnconditional())
                {
                    // ignore, but mark as INDEX_SAME / ALIGN_FALSE
                    //WFV::setMetadata(br, WFV::WFV_METADATA_INDEX_SAME);
                    //WFV::setMetadata(br, WFV::WFV_METADATA_ALIGNED_FALSE);
                    //markedValues.insert(br);
                    continue;
                }
            }

            if (markedValues.count(&*I))
            {
                // Value is already marked.
                // Add operands to be sure that we mark everything.
                for (Instruction::op_iterator O=I->op_begin(), OE=I->op_end(); O!=OE; ++O)
                {
                    if (!isa<Instruction>(O)) continue;
                    Instruction* opI = cast<Instruction>(O);
                    if (markedValues.count(opI)) continue;
                    workSet.insert(opI);
                }
                continue;
            }

            // Ignore instructions already marked via API calls.
            // Note that checking this here still allows to add the operands.
            if (mValueInfoMap->hasMapping(*I)) continue;

            workSet.insert(&*I);
        }
    }


    //
    // Now mark all other instructions according to their dependencies.
    //

    if(mVerbose) {
        outs() << "\nworkSet:\n";
        for (auto it : workSet) {
            outs() << " * " << *it << "\n";
        }
    }

    if(mVerbose) outs() << "\nmarking instructions...\n";
    for (auto it : workSet)
    {
        Instruction* inst = it;
        const char* ii = nullptr;
        const char* ai = nullptr;
        markIndexAlignValueAndOps(inst, markedValues, &ii, &ai);

        // TODO: Move these checks to a verification function that tests
        //       *all* instructions.
        assert ((inst->getType()->isVoidTy() ||
                !WFV::hasMetadata(inst, WFV::WFV_METADATA_INDEX_SAME) ||
                WFV::hasMetadata(inst, WFV::WFV_METADATA_RES_UNIFORM)) &&
                "if value is INDEX_SAME it must be RES_UNIFORM!");
        assert ((inst->getType()->isVoidTy() ||
                !WFV::hasMetadata(inst, WFV::WFV_METADATA_RES_UNIFORM) ||
                WFV::hasMetadata(inst, WFV::WFV_METADATA_INDEX_SAME)) &&
                "if value is RES_UNIFORM it must be INDEX_SAME!");

        markedValues.insert(inst);
    }
}

void
VectorizationAnalysis::markIndexAlignValueAndOps(Value*                   value,
                                                 SmallPtrSet<Value*, 32>& markedValues,
                                                 const char**             indexInfo,
                                                 const char**             alignInfo)
{
    assert (value);
    assert (isa<Instruction>(value) ||
            isa<Argument>(value) ||
            isa<Constant>(value));

    if(mVerbose) outs() << "markIndexAlignValueAndOps(1): " << *value << "\n";

    // Handle values that were already marked.
    if (markedValues.count(value))
    {
        getIndexAlignedInfo(value, indexInfo, alignInfo);
        if(mVerbose) outs() << "  already marked as " << *indexInfo << " / " << *alignInfo
                << " - ignored!\n";
        return;
    }

    assert (!isa<Argument>(value) &&
            "all arguments have to be marked already!");

    if (Constant* c = dyn_cast<Constant>(value))
    {
        *indexInfo = deriveIndexInfo(c);
        *alignInfo = deriveAlignedInformation(c);
        return;
    }

    // If this is a loop PHI, make sure to recurse into the predecessor
    // outside of the loop first to break cycles. The phi is then marked
    // according to this predecessor.
    if (isa<PHINode>(value) &&
        mLoopInfo->isLoopHeader(cast<PHINode>(value)->getParent()))
    {
        PHINode* phi = cast<PHINode>(value);
        assert (phi->getNumIncomingValues() == 2);
        Loop* loop = mLoopInfo->getLoopFor(phi->getParent());

        // Mark predecessor of incoming value from outside loop
        BasicBlock* preheaderBB = loop->getLoopPreheader();
        Value* preheaderVal = phi->getIncomingValueForBlock(preheaderBB);

        const char* phIndex = nullptr;
        const char* phAlign = nullptr;
        markIndexAlignValueAndOps(preheaderVal, markedValues, &phIndex, &phAlign);
        if(mVerbose) outs() << "markIndexAlignValueAndOps(2): " << *phi << "\n";
        assert (strcmp(phIndex, "") != 0);
        assert (strcmp(phAlign, "") != 0);

        // Mark the phi according to this predecessor, unless the phi is
        // known to be VARYING and the predecessor is INDEX_SAME - this
        // violates an important assumption, so we do not set a wrong mark
        // here to prevent this in the first place.
        // NOTE: Unfortunately, marking the phi as INDEX_RANDOM  might
        //       introduce some imprecision in cases where the other
        //       incoming value is INDEX_CONSECUTIVE.
        WFV::setMetadata(phi, phIndex);
        WFV::setMetadata(phi, phAlign);
        if(mVerbose) outs() << "  marked loop phi: " << *phi << "\n";

        markedValues.insert(phi);

        // Set indexInfo/alignInfo for fixpoint iteration.
        *indexInfo = phIndex;
        *alignInfo = phAlign;

        // Now recurse into other operand (back edge).
        bool changed = true;
        while (changed)
        {
            SmallPtrSet<Value*, 32> markedLoopValues(markedValues);

            const int preheaderIdx = phi->getBasicBlockIndex(preheaderBB);
            const int backedgeIdx = preheaderIdx == 0 ? 1 : 0;
            Value* backedgeVal = phi->getIncomingValue(backedgeIdx);

            const char* beIndex = nullptr;
            const char* beAlign = nullptr;
            markIndexAlignValueAndOps(backedgeVal, markedLoopValues, &beIndex, &beAlign);
            if(mVerbose) outs() << "markIndexAlignValueAndOps(3): " << *phi << "\n";
            assert (strcmp(beIndex, "") != 0);
            assert (strcmp(beAlign, "") != 0);

            // If necessary, update marks of phi (if backedge-marks differ from
            // preheader-marks).
            std::vector<const char*> iiVec;
            std::vector<const char*> aiVec;
            iiVec.push_back(beIndex);
            iiVec.push_back(phIndex);
            aiVec.push_back(beAlign);
            aiVec.push_back(phAlign);

            const char* ii = deriveIndexInfo(phi, iiVec);
            const char* ai = deriveAlignmentInfo(phi, aiVec);
            WFV::setMetadata(phi, ii);
            WFV::setMetadata(phi, ai);
            if(mVerbose) outs() << "  updated loop phi: " << *phi << "\n";

            // If we did update the phi, we have to recurse again
            changed = (*indexInfo && *alignInfo) ?
                (strcmp(ii, *indexInfo) || strcmp(ai, *alignInfo)) :
                false;

            *indexInfo = ii;
            *alignInfo = ai;

            if (!changed ||
                (ii == WFV::WFV_METADATA_INDEX_RANDOM && ai == WFV::WFV_METADATA_ALIGNED_FALSE))
            {
                markedValues.insert(markedLoopValues.begin(), markedLoopValues.end());
            }
        }

        return;
    }

    // Arguments have to be marked already, same for constants now.
    assert (isa<Instruction>(value));
    Instruction* valI = cast<Instruction>(value);

    // Collect info of operands.
    std::vector<const char*> aiVec;
    std::vector<const char*> iiVec;

    for (Instruction::op_iterator O=valI->op_begin(), OE=valI->op_end(); O!=OE; ++O)
    {
        Value* opVal = *O;
        if (isa<BasicBlock>(opVal)) continue; // handle phis correctly
        if (isa<Function>(opVal)) continue; // handle calls correctly

        const char* opIndex = nullptr;
        const char* opAlign = nullptr;
        markIndexAlignValueAndOps(opVal, markedValues, &opIndex, &opAlign);
        assert (strcmp(opIndex, "") != 0);
        assert (strcmp(opAlign, "") != 0);

        iiVec.push_back(opIndex);
        aiVec.push_back(opAlign);
    }
    if(mVerbose) outs() << "markIndexAlignValueAndOps(4): " << *valI << "\n";

    // If this is an operation without return value, ignore it.
    // Note that we can only stop after marking all operands.
    if (value->getType()->isVoidTy())
    {
        if(mVerbose) outs() << "  has void type - ignored!\n";
        return;
    }

    // If this is a call or a non-uniform load, we don't know anything about
    // its return value if it was not marked by user.
    // If the return type of a non-uniform instruction can not be vectorized,
    // we don't know anything about it unless it was marked by user already.
    // Non-uniform non-vectorizable instructions can not be INDEX_CONSECUTIVE.
    const bool isCall = isa<CallInst>(valI);
    const bool isLoad = isa<LoadInst>(valI);
    const bool isCast = isa<CastInst>(valI);
    const bool isGEP  = isa<GetElementPtrInst>(valI);
    const bool isUniform = WFV::hasMetadata(valI, WFV::WFV_METADATA_OP_UNIFORM);
    const bool isVectorizable = WFV::isVectorizableInst(*valI);

    const bool noInfo = !isUniform &&
        (isCall || isLoad || (!isCast && !isGEP && !isVectorizable));

    if (noInfo)
    {
        assert (WFV::hasMetadata(valI, WFV::WFV_METADATA_OP_VARYING) ||
                WFV::hasMetadata(valI, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
                WFV::hasMetadata(valI, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED));
        assert (!WFV::hasMetadata(valI, WFV::WFV_METADATA_RES_UNIFORM));

        WFV::setMetadata(valI, WFV::WFV_METADATA_INDEX_RANDOM);
        WFV::setMetadata(valI, WFV::WFV_METADATA_ALIGNED_FALSE);
        if(mVerbose) outs() << "  marked instruction as INDEX_RANDOM / ALIGN_FALSE!\n";

        *indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
        *alignInfo = WFV::WFV_METADATA_ALIGNED_FALSE;

        if(mVerbose) outs() << "  marked value: " << *valI << "\n";
        markedValues.insert(valI);

        return;
    }

    // Derive alignment and index info depending on instruction and marks of
    // operands.
    const char* ii = deriveIndexInfo(valI, iiVec);
    const char* ai = deriveAlignmentInfo(valI, aiVec);
    WFV::setMetadata(valI, ii);
    WFV::setMetadata(valI, ai);

    if(mVerbose) outs() << "  marked value: " << *valI << "\n";

    markedValues.insert(valI);

    assert (strcmp(ii, "") != 0);
    assert (strcmp(ai, "") != 0);

    *indexInfo = ii;
    *alignInfo = ai;

    return;
}

void
VectorizationAnalysis::getIndexAlignedInfo(Value*       value,
                                           const char** indexInfo,
                                           const char** alignInfo) const
{
    assert (value);
    assert (isa<Instruction>(value) ||
            isa<Argument>(value));

    *indexInfo = WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_SAME) ?
        WFV::WFV_METADATA_INDEX_SAME :
        WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_CONSECUTIVE) ?
            WFV::WFV_METADATA_INDEX_CONSECUTIVE :
            WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_RANDOM) ?
                WFV::WFV_METADATA_INDEX_RANDOM :
                WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_STRIDED) ?
                    WFV::WFV_METADATA_INDEX_STRIDED :
                    WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_SHUFFLE) ?
                        WFV::WFV_METADATA_INDEX_SHUFFLE :
                        "";

    *alignInfo = WFV::hasMetadata(value, WFV::WFV_METADATA_ALIGNED_TRUE) ?
        WFV::WFV_METADATA_ALIGNED_TRUE :
        WFV::hasMetadata(value, WFV::WFV_METADATA_ALIGNED_FALSE) ?
            WFV::WFV_METADATA_ALIGNED_FALSE :
            "";

    return;
}

// Derive index info depending on the instruction and operands marks
const char*
VectorizationAnalysis::deriveIndexInfo(Instruction* inst,
                                       const std::vector<const char*>& iiVec) const
{
    assert (inst);

    const char* indexInfo = nullptr;
    const bool isSub = inst->getOpcode() == Instruction::Sub  || inst->getOpcode() == Instruction::FSub;

    switch (inst->getOpcode())
    {
        case Instruction::Add:
        case Instruction::FAdd:
        case Instruction::Sub:
        case Instruction::FSub:
        {
            assert (iiVec.size() == 2);
            // Adding/subtracting two "same" indices yields a "same" index.
            if (strcmp(iiVec[0], WFV::WFV_METADATA_INDEX_SAME) == 0 &&
                strcmp(iiVec[1], WFV::WFV_METADATA_INDEX_SAME) == 0)
            {
                indexInfo = WFV::WFV_METADATA_INDEX_SAME;
                break;
            }
            if (strcmp(iiVec[0], WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0 &&
                strcmp(iiVec[1], WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0)
            {
                if (inst->getOpcode() == Instruction::Sub ||
                    inst->getOpcode() == Instruction::FSub)
                {
                    // Subtracting two consecutive indices yields a "same" index.
                    indexInfo = WFV::WFV_METADATA_INDEX_SAME;
                    break;
                }
                else
                {
                    // Adding two consecutive indices yields a strided index.
                    // TODO: do something useful with this info :)
                    //indexInfo = INDEX_STRIDED;
                    indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
                    break;
                }
            }

             // Adding/subtracting a consecutive and a "same" index yields a consecutive index.
             if ((strcmp(iiVec[0], WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0 &&
                  strcmp(iiVec[1], WFV::WFV_METADATA_INDEX_SAME) == 0) ||
                (!isSub && strcmp(iiVec[1], WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0 &&
                           strcmp(iiVec[0], WFV::WFV_METADATA_INDEX_SAME) == 0))
            {
                indexInfo = WFV::WFV_METADATA_INDEX_CONSECUTIVE;
                break;
            }
            indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
            break;
        }

        case Instruction::GetElementPtr:
        {
            // If any operand is "random", return "random".
            // If all operands are "same", return "same".
            // If one index is "consecutive" while all others are "same", return "consecutive".
            // Otherwise, return INDEX_RANDOM.
            GetElementPtrInst* gep = cast<GetElementPtrInst>(inst);


            if(mVerbose) errs() << "\nAnalyzing " << *gep << "\n";
            // the basePtr is SOA
            Value * ptrOperand = gep->getPointerOperand();

            bool flatBasePtr = WFV::IsFlatPointer(*ptrOperand->getType());
            if(mVerbose) errs() <<   "Base flat : " << flatBasePtr << "\n";

            bool flatTargetPtr = WFV::IsFlatPointer(*gep->getType());
            if(mVerbose) errs() << "Target flat : " << flatTargetPtr << "    " << *gep->getType() << "\n";

            bool isSOA = !flatBasePtr && strcmp(iiVec[0], WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0;
            if(mVerbose) errs() << "      isSOA : " << isSOA << "\n";

            bool lastConsecutive = false;


			bool     random      = false;
			bool     allSame     = true;
			unsigned consecFound = 0;
			for (unsigned i=0, e=gep->getNumOperands(); i!=e; ++i)
			{
				if(mVerbose) errs() << "ii[" << i << "] " <<iiVec[i] << "\n";

				lastConsecutive = false;
				if (strcmp(iiVec[i], WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0)
				{
					++consecFound;
					allSame = false;
					lastConsecutive = true;
					continue;
				}

				if (strcmp(iiVec[i], WFV::WFV_METADATA_INDEX_SAME) != 0)
				{
					random = true;
					break;
				}
			}

			if (! random) {
				if (allSame) {
					if(mVerbose) errs() << "-> Uniform GEP\n";
					indexInfo = WFV::WFV_METADATA_INDEX_SAME; break;
				} else if (consecFound == 1 && isSOA) {
					if(mVerbose) errs() << "-> Consecutive SOA GEP\n";
					indexInfo = WFV::WFV_METADATA_INDEX_CONSECUTIVE; break;
				} // SOA base pointer with uniform accesses remains SOA
				else if (consecFound == 1 && lastConsecutive && flatTargetPtr) {
					if(mVerbose) errs() << "-> Consecutive flat GEP\n";
					indexInfo = WFV::WFV_METADATA_INDEX_CONSECUTIVE; break;
				} // UNIFORM base pointer, last index is consecutive on a flat pointer type
			}

			// default behavior
			if(mVerbose) errs() << "-> Varying GEP\n";
			/*
            unsigned registerSize = mTTI->getRegisterBitWidth(true);
            Type *ptrType = inst->getOperand(0)->getType();
            unsigned gepElementSize = mSimdFunction->getParent()->getDataLayout().getTypeSizeInBits(ptrType);
            if( registerSize / gepElementSize <= mVectorizationFactor)
                WFV::setMetadata(inst, WFV::WFV_METADATA_RES_VECTOR);
            else
            */
			    WFV::setMetadata(inst, WFV::WFV_METADATA_RES_SCALARS);
			indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
			break;
        }

        case Instruction::Mul:
        case Instruction::FMul:
        {
            assert (iiVec.size() == 2);
            // Multiplying two "same" indices yields a "same" index.
            if (strcmp(iiVec[0], WFV::WFV_METADATA_INDEX_SAME) == 0 &&
                strcmp(iiVec[1], WFV::WFV_METADATA_INDEX_SAME) == 0)
            {
                indexInfo = WFV::WFV_METADATA_INDEX_SAME;
                break;
            }
            // Multiplying a "same" index with a consecutive one yields a strided index.
            if ((strcmp(iiVec[0], WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0 &&
                 strcmp(iiVec[1], WFV::WFV_METADATA_INDEX_SAME) == 0) ||
                (strcmp(iiVec[1], WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0 &&
                 strcmp(iiVec[0], WFV::WFV_METADATA_INDEX_SAME) == 0))
            {
                // TODO: do something useful with this info :)
                //indexInfo = INDEX_STRIDED;
                indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
                break;
            }
            indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
            break;
        }

        // These instructions retain "same" if operands are all "same",
        // nothing else.
        case Instruction::UDiv:
        case Instruction::SDiv:
        case Instruction::URem:
        case Instruction::SRem:
        case Instruction::And:
        case Instruction::Or:
        case Instruction::Xor:
        case Instruction::Shl:
        case Instruction::LShr:
        case Instruction::AShr:
        case Instruction::Call:
        case Instruction::ICmp:
        case Instruction::FCmp:
        {
            for (const auto &it : iiVec)
            {
                if (strcmp(it, WFV::WFV_METADATA_INDEX_SAME) != 0)
                {
                    indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
                    break;
                }
            }
            indexInfo = WFV::WFV_METADATA_INDEX_SAME;
            break;
        }

        // These instructions retain any information if all operands have
        // the same mark except for casts to void pointer type.
        // TODO: This is not true! If casting consecutive/aligned i32* to i16*,
        //       the access becomes strided! Likewise, if casting i32* to i64*,
        //       the access becomes possibly unaligned.
        case Instruction::Trunc:
        case Instruction::SExt:
        case Instruction::FPTrunc:
        case Instruction::FPExt:
        case Instruction::ZExt:
        case Instruction::FPToUI:
        case Instruction::FPToSI:
        case Instruction::UIToFP:
        case Instruction::SIToFP:
        case Instruction::IntToPtr:
        case Instruction::PtrToInt:
        case Instruction::BitCast:
        {
            if (WFV::returnsVoidPtr(*inst))
            {
                indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
                break;
            }

            bool allSame = true;
            bool allConsecutive = true;
            bool allStrided = true;
            for (unsigned i=0, e=iiVec.size(); i!=e; ++i)
            {
                allSame &= strcmp(iiVec[i], WFV::WFV_METADATA_INDEX_SAME) == 0;
                allConsecutive &= strcmp(iiVec[i], WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0;
                allStrided &= strcmp(iiVec[i], WFV::WFV_METADATA_INDEX_STRIDED) == 0;
            }
            assert (!(allSame && allConsecutive));
            assert (!(allSame && allStrided));
            assert (!(allStrided && allConsecutive));

            if (allSame) indexInfo = WFV::WFV_METADATA_INDEX_SAME;
            else if (allConsecutive) indexInfo = WFV::WFV_METADATA_INDEX_CONSECUTIVE;
            else if (allStrided) indexInfo = WFV::WFV_METADATA_INDEX_STRIDED;
            else indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
            break;
        }

        case Instruction::Store:
        {
            for (const auto &it : iiVec)
            {
                if (strcmp(it, WFV::WFV_METADATA_INDEX_SAME) != 0)
                {
                    indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
                    break;
                }
            }
            indexInfo = WFV::WFV_METADATA_INDEX_SAME;
            break;
        }

        case Instruction::Load:
        {
            LoadInst* load = cast<LoadInst>(inst);

            // Special case:
            // If we load from a pointer-to-pointer that is marked
            // INDEX_CONSECUTIVE, the result is still INDEX_CONSECUTIVE.
            // TODO: Unsure if this is safe :)
            Value* pointer = load->getPointerOperand();
            if (pointer->getType()->getContainedType(0)->isPointerTy())
            {
                if (isa<Instruction>(pointer) || isa<Argument>(pointer))
                {
                    const char* ii, *ai;
                    getIndexAlignedInfo(pointer, &ii, &ai);
                    if (strcmp(ii, WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0)
                    {
                        indexInfo = WFV::WFV_METADATA_INDEX_CONSECUTIVE;
                        break;
                    }
                }
            }

            const unsigned ptrIdx = load->getPointerOperandIndex();
            if (strcmp(iiVec[ptrIdx], WFV::WFV_METADATA_INDEX_SAME) == 0)
            {
                indexInfo = WFV::WFV_METADATA_INDEX_SAME;
                break;
            }
            indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
            break;
        }

        case Instruction::PHI:
        {
            PHINode* phi = cast<PHINode>(inst);
            bool allSame = true;
            bool allConsecutive = true;
            bool allStrided = true;
            for (unsigned i=0, e=phi->getNumIncomingValues(); i!=e; ++i)
            {
                // We ignored basic blocks while collecting indexInfo,
                // so now we can directly index iiVec per incoming value.
                allSame &= strcmp(iiVec[i], WFV::WFV_METADATA_INDEX_SAME) == 0;
                allConsecutive &= strcmp(iiVec[i], WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0;
                allStrided &= strcmp(iiVec[i], WFV::WFV_METADATA_INDEX_STRIDED) == 0;
            }
            assert (!(allSame && allConsecutive));
            assert (!(allSame && allStrided));
            assert (!(allStrided && allConsecutive));

            // If the phi is OP_VARYING (e.g. due to divergent control flow),
            // it will be transformed into a select which - even if both operands are
            // SAME - can not be proven to be SAME as well.
            if (WFV::hasMetadata(phi, WFV::WFV_METADATA_OP_VARYING))
            {
                assert (WFV::isExitOfDivergentLoop(*phi->getParent(), *mLoopInfo) ||
                        !WFV::hasMetadata(phi->getParent(), WFV::WFV_METADATA_DIVERGENT_FALSE));
                indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
                break;
            }

            if (allSame) indexInfo = WFV::WFV_METADATA_INDEX_SAME;
            else if (allConsecutive) indexInfo = WFV::WFV_METADATA_INDEX_CONSECUTIVE;
            else if (allStrided) indexInfo = WFV::WFV_METADATA_INDEX_STRIDED;
            else indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
            break;
        }

        case Instruction::Select:
        {
            assert (iiVec.size() == 3);

            // If the condition is not UNIFORM, we cannot say anything
            // about the result.
            if (strcmp(iiVec[0], WFV::WFV_METADATA_INDEX_SAME) != 0)
            {
                assert ((isa<Constant>(cast<SelectInst>(inst)->getCondition()) ||
                         !WFV::hasMetadata(cast<SelectInst>(inst)->getCondition(), WFV::WFV_METADATA_RES_UNIFORM)) &&
                        "condition must not be RES_UNIFORM!");
                indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
                break;
            }

            assert ((isa<Constant>(cast<SelectInst>(inst)->getCondition()) ||
                     WFV::hasMetadata(cast<SelectInst>(inst)->getCondition(), WFV::WFV_METADATA_RES_UNIFORM)) &&
                    "condition must be RES_UNIFORM!");

            // If the condition is INDEX_SAME, we know that the result
            // has a certain property if both incoming values have it.
            bool allSame = strcmp(iiVec[1], WFV::WFV_METADATA_INDEX_SAME) == 0 &&
            strcmp(iiVec[2], WFV::WFV_METADATA_INDEX_SAME) == 0;
            bool allConsecutive = strcmp(iiVec[1], WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0 &&
            strcmp(iiVec[2], WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0;
            bool allStrided = strcmp(iiVec[1], WFV::WFV_METADATA_INDEX_STRIDED) == 0 &&
            strcmp(iiVec[2], WFV::WFV_METADATA_INDEX_STRIDED) == 0;

            if (allSame) indexInfo = WFV::WFV_METADATA_INDEX_SAME;
            else if (allConsecutive) indexInfo = WFV::WFV_METADATA_INDEX_CONSECUTIVE;
            else if (allStrided) indexInfo = WFV::WFV_METADATA_INDEX_STRIDED;
            else indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
            break;
        }

        case Instruction::Alloca:
        {
            assert (iiVec.size() == 1);

            if (WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_UNIFORM))
            {
                indexInfo = WFV::WFV_METADATA_INDEX_SAME;
            }
            else if (WFV::isVectorizableNonDerivedType(*inst->getType()->getPointerElementType()))
            {
                // Can use vector load -> CONSECUTIVE.
                indexInfo = WFV::WFV_METADATA_INDEX_CONSECUTIVE;
            }
            else
            {
                indexInfo = WFV::WFV_METADATA_INDEX_RANDOM;
            }
            break;
        }

        default:
        {
            // Retain any information if all operands have the same mark.
            if (iiVec.size() == 0)
            {
                indexInfo = WFV::WFV_METADATA_INDEX_SAME;
                break;
            }
            bool allSame = true;
            bool allConsecutive = true;
            bool allStrided = true;
            for (const auto &it : iiVec)
            {
                allSame &= strcmp(it, WFV::WFV_METADATA_INDEX_SAME) == 0;
                allConsecutive &= strcmp(it, WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0;
                allStrided &= strcmp(it, WFV::WFV_METADATA_INDEX_STRIDED) == 0;
            }
            assert (!(allSame && allConsecutive));
            assert (!(allSame && allStrided));
            assert (!(allStrided && allConsecutive));

            if (allSame) indexInfo = WFV::WFV_METADATA_INDEX_SAME;
            else if (allConsecutive) indexInfo = WFV::WFV_METADATA_INDEX_CONSECUTIVE;
            else if (allStrided) indexInfo = WFV::WFV_METADATA_INDEX_STRIDED;
            else indexInfo = WFV::WFV_METADATA_INDEX_RANDOM; // weakest information
            break;
        }
    }

    assert(indexInfo && "was not set!");

    // A non-RES_UNIFORM instruction can never be INDEX_SAME, but it can be
    // INDEX_CONSECUTIVE.
    if (strcmp(indexInfo, WFV::WFV_METADATA_INDEX_SAME) == 0 &&
        !WFV::hasMetadata(inst, WFV::WFV_METADATA_RES_UNIFORM))
    {
        return WFV::WFV_METADATA_INDEX_RANDOM;
    }

    return indexInfo;
}

// Derive alignment info depending on the instruction and operands marks
// TODO: div/rem/others?
const char*
VectorizationAnalysis::deriveAlignmentInfo(Instruction* inst,
                                           const std::vector<const char*>& aiVec) const
{
    assert (inst);

    switch (inst->getOpcode())
    {
        // These instructions retain alignment if all operands are aligned
        case Instruction::Add:
        case Instruction::Sub:
        case Instruction::FAdd:
        case Instruction::FSub:
        {
            assert (aiVec.size() == 2);
            if (strcmp(aiVec[0], WFV::WFV_METADATA_ALIGNED_TRUE) != 0 ||
                strcmp(aiVec[1], WFV::WFV_METADATA_ALIGNED_TRUE) != 0)
            {
                return WFV::WFV_METADATA_ALIGNED_FALSE;
            }
            return WFV::WFV_METADATA_ALIGNED_TRUE;
        }

        // These instructions retain alignment if one operand is aligned
        case Instruction::Mul:
        case Instruction::FMul:
        {
            assert (aiVec.size() == 2);
            if (strcmp(aiVec[0], WFV::WFV_METADATA_ALIGNED_TRUE) == 0 ||
                strcmp(aiVec[1], WFV::WFV_METADATA_ALIGNED_TRUE) == 0)
            {
                return WFV::WFV_METADATA_ALIGNED_TRUE;
            }
            return WFV::WFV_METADATA_ALIGNED_FALSE;
        }

        // These instructions retain any information if all operands have
        // the same mark except for casts to void pointer type.
        case Instruction::Trunc:
        case Instruction::SExt:
        case Instruction::FPTrunc:
        case Instruction::FPExt:
        case Instruction::ZExt:
        case Instruction::FPToUI:
        case Instruction::FPToSI:
        case Instruction::UIToFP:
        case Instruction::SIToFP:
        case Instruction::IntToPtr:
        case Instruction::PtrToInt:
        case Instruction::BitCast:
        {
            if (WFV::returnsVoidPtr(*inst)) return WFV::WFV_METADATA_INDEX_RANDOM;

            assert (aiVec.size() == 1);
            if (strcmp(aiVec[0], WFV::WFV_METADATA_ALIGNED_TRUE) == 0)
            {
                return WFV::WFV_METADATA_ALIGNED_TRUE;
            }
            return WFV::WFV_METADATA_ALIGNED_FALSE;
        }

        // GEP retains alignment if all indices are aligned
        // TODO: really? :P
        case Instruction::GetElementPtr:
        {
            GetElementPtrInst* gep = cast<GetElementPtrInst>(inst);
            const unsigned numIndices = gep->getNumIndices();
            const unsigned idxBegin = gep->getNumOperands() - numIndices;
            for (unsigned i=idxBegin, e=gep->getNumOperands(); i!=e; ++i)
            {
                if (strcmp(aiVec[i], WFV::WFV_METADATA_ALIGNED_TRUE) != 0)
                {
                    return WFV::WFV_METADATA_ALIGNED_FALSE;
                }
            }
            return WFV::WFV_METADATA_ALIGNED_TRUE;
        }

        // Phi is aligned if all incoming values are aligned
        case Instruction::PHI:
        {
            PHINode* phi = cast<PHINode>(inst);
            for (unsigned i=0, e=phi->getNumIncomingValues(); i!=e; ++i)
            {
                // We ignored basic blocks while collecting indexInfo,
                // so now we can directly index iiVec per incoming value.
                if (strcmp(aiVec[i], WFV::WFV_METADATA_ALIGNED_TRUE) != 0)
                {
                    return WFV::WFV_METADATA_ALIGNED_FALSE;
                }
            }

            return WFV::WFV_METADATA_ALIGNED_TRUE;
        }

        case Instruction::Alloca:
        {
            return WFV::WFV_METADATA_ALIGNED_TRUE;
        }

        // All other instructions (conservatively) produce non-aligned values
        default:
        {
            return WFV::WFV_METADATA_ALIGNED_FALSE; // weakest information
        }
    }
}

const char*
VectorizationAnalysis::deriveIndexInfo(const Constant* c) const
{
    assert (c);
    assert (!isa<BasicBlock>(c));
    assert (!isa<Function>(c));

    return WFV::WFV_METADATA_INDEX_SAME;
}

// TODO: implement support for natural numbers stored as floats?
const char*
VectorizationAnalysis::deriveAlignedInformation(const Constant* c) const
{
    assert (c);
    assert (!isa<BasicBlock>(c));
    assert (!isa<Function>(c));

    // An undef value is never aligned.
    if (isa<UndefValue>(c)) return WFV::WFV_METADATA_ALIGNED_FALSE;

    // Integer that are divisible by the simd width are ALIGN_TRUE.
    if (c->getType()->isIntegerTy())
    {
        const ConstantInt* cint = cast<ConstantInt>(c);
        const uint64_t& intValue = *cint->getValue().getRawData();
        if (intValue % mVectorizationFactor == 0) return WFV::WFV_METADATA_ALIGNED_TRUE;
        else return WFV::WFV_METADATA_ALIGNED_FALSE;
    }

    // Other than that, only integer vector constants can be aligned.
    if (!c->getType()->isVectorTy()) return WFV::WFV_METADATA_ALIGNED_FALSE;

    // A zero-vector is aligned.
    if (isa<ConstantAggregateZero>(c)) return WFV::WFV_METADATA_ALIGNED_TRUE;

    if (const ConstantDataVector* cdv = dyn_cast<ConstantDataVector>(c))
    {
        if (!cdv->getElementType()->isIntegerTy())
        {
            return WFV::WFV_METADATA_ALIGNED_FALSE;
        }

        const uint64_t intValue = cdv->getElementAsInteger(0);

        if (intValue % mVectorizationFactor != 0) return WFV::WFV_METADATA_ALIGNED_FALSE;

        bool isSame   = true;
        bool isConsec = true;
        for (unsigned i=1, e=cdv->getNumElements(); i<e; ++i)
        {
            const uint64_t val = cdv->getElementAsInteger(i);
            if (isSame && intValue == val)
            {
                isConsec = false;
                continue;
            }
            else
            {
                isSame   = false;
            }

            if (isConsec && intValue + i == val)
            {
                isSame   = false;
                continue;
            }
            else
            {
                isConsec = false;
            }

            if (!isSame && !isConsec) break;
        }

        return (isSame || isConsec) ?
            WFV::WFV_METADATA_ALIGNED_TRUE :
            WFV::WFV_METADATA_ALIGNED_FALSE;
    }

    assert (isa<ConstantVector>(c));
    const ConstantVector* cv = cast<ConstantVector>(c);

    if (!cv->getType()->getElementType()->isIntegerTy()) return WFV::WFV_METADATA_ALIGNED_FALSE;

    assert (isa<ConstantInt>(cv->getOperand(0)));
    const ConstantInt* celem = cast<ConstantInt>(cv->getOperand(0));
    const uint64_t& intValue = *celem->getValue().getRawData();

    // The vector is aligned if its first element is aligned and the
    // constant is either SAME or CONSECUTIVE
    if (intValue % mVectorizationFactor != 0) return WFV::WFV_METADATA_ALIGNED_FALSE;

    // TODO: There might be other cases (e.g. STRIDED) where we want to
    //       return true...
    const char* ii = deriveIndexInfo(c);
    if (strcmp(ii, WFV::WFV_METADATA_INDEX_SAME) == 0)
    {
        return WFV::WFV_METADATA_ALIGNED_TRUE;
    }
    else if (strcmp(ii, WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0)
    {
        return WFV::WFV_METADATA_ALIGNED_TRUE;
    }
    return WFV::WFV_METADATA_ALIGNED_FALSE;
}


////////////////////////////////////////////////////////////////////////////
//                          SPLITTTING ANALYSIS                           //
////////////////////////////////////////////////////////////////////////////

// During this analysis, we only find primary split targets (instructions
// that have to be split). It is not decided which other instructions have
// to be split as well until the "pack-unpack" optimization phase right before
// instruction vectorization.
// The following, non-uniform instructions have to be split:
// 1) Instructions that have a non-vectorizable return type.
// 2) Instructions that have sequential operands.
// 3) Calls and stores/loads that depend upon divergent control-flow.
// 4) Loads and stores that have non-vectorizable values to load/store.
// 5) GEPs with non-uniform indices.
// TODO: This has to be done in a fixpoint iteration:
//       Consider a call with side effects that returns a pointer and is in a
//       block with a non-fully uniform mask. This call would be marked
//       OP_VARYING/RES_VECTOR by the uniform analysis.
//       Here, this call then is first not marked as RES_SCALARS (RES_VECTOR
//       with pointer return type is okay if not OP_SEQUENTIAL), but then marked
//       OP_SEQUENTIAL_GUARDED, and because of its return type its mark is
//       changed to RES_SCALARS.
//       If one of its uses was tested before, it may not have been changed to
//       OP_SEQUENTIAL because the operand (the call) was still RES_VECTOR.
void
VectorizationAnalysis::analyzeSplitInfo(Function* scalarFn)
{

    bool changed = true;

    while (changed)
    {
        changed = false;

        if(mVerbose) outs() << "\nMarking instructions that have to be split"
            << " and executed sequentially instead of vectorized...\n";

        // Mark instructions whose result can not be combined to vectors as
        // RES_SCALARS (which implies OP_SEQUENTIAL / OP_SEQUENTIAL_GUARDED).
        // Uses of these instructions also have to be OP_SEQUENTIAL or
        // OP_SEQUENTIAL_GUARDED, but may have vector results again.
        // This does not apply to:
        // - instructions with void return type
        // - uniform instructions
        // - consecutive instructions of pointer type (which indicate that a vector
        //   load/store can be used)
        for (auto &BB : *scalarFn)
        {
            for (auto &I : BB)
            {
                if (!requiresScalarResults(I)) continue;
                changed |= markValueAs(&I, WFV::WFV_METADATA_RES_SCALARS);
            }
        }

        for (auto &BB : *scalarFn)
        {
            for (auto &I : BB) {
                Instruction *inst = &I;

                if (!requiresSplit(*inst)) continue;

                // NOTE: If a call has to be split only due to operands or its
                //       result type, it is not necessary to guard it.
                bool needsGuard = mayRequireGuards(*inst);

                // if the hardware supports masked loads/stores do not split the instruction
                if (StoreInst * store = dyn_cast<StoreInst>(inst)) {
                    Type *type = store->getValueOperand()->getType();
                    Value *ptr = store->getPointerOperand();
                    if (needsGuard && mTTI->isLegalMaskedStore(type) &&
                            WFV::hasMetadata(ptr, WFV::WFV_METADATA_INDEX_CONSECUTIVE)) {
                        if(mVerbose) outs() << "masked store possible for" << store << "\n";
                        changed |= markValueAs(inst, WFV::WFV_METADATA_OP_MASKED);
                        continue;
                    }
                }

                if (LoadInst * load = dyn_cast<LoadInst>(inst)) {
                    Type *type = cast<PointerType>(load->getPointerOperand()->getType())->getElementType();
                    Value *ptr = load->getPointerOperand();
                    if (needsGuard && mTTI->isLegalMaskedLoad(type) &&
                            WFV::hasMetadata(ptr, WFV::WFV_METADATA_INDEX_CONSECUTIVE)) {
                        if(mVerbose) outs() << "masked load possible for" << load << "\n";
                        changed |= markValueAs(inst, WFV::WFV_METADATA_OP_MASKED);
                        continue;
                    }
                }

                changed |= markValueAs(inst,
                                       needsGuard ?
                                       WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED :
                                       WFV::WFV_METADATA_OP_SEQUENTIAL);

                // If the result type is a pointer, it can't be a pointer to a vector
                // anymore, but has to be a vector of pointers. Since we can't express
                // this, it has to be split into scalar values (RES_SCALARS) which in
                // turn results in OP_SEQUENTIAL uses of this inst again.
                if (inst->getType()->isPointerTy())
                {
                    changed |= markValueAs(inst, WFV::WFV_METADATA_RES_SCALARS);
                }
            } // I
        } // BB

    } // while (changed)
}

bool
VectorizationAnalysis::requiresScalarResults(const Instruction& inst) const
{
    // Exclude instructions that can never be RES_SCALARS.
    if (inst.getType()->isVoidTy()) return false;

    // Exclude instructions that can never be OP_SEQUENTIAL.
    // TODO: This might be wrong for loop header phis:
    if (WFV::hasMetadata(&inst, WFV::WFV_METADATA_OP_UNIFORM)) return false;

    // If the instruction is OP_VARYING / INDEX_CONSECUTIVE,
    // it can remain RES_VECTOR even if the type is not okay since
    // we will not actually create vectors.
    if (WFV::hasMetadata(&inst, WFV::WFV_METADATA_INDEX_CONSECUTIVE)) return false;

    // Check instructions that were marked from outside.
    if (mValueInfoMap->hasMapping(inst)) return false;

#if 0
    // Check all other instructions except for those with
    // special handling (load/store/gep/phi/select/call).
    if (WFV::mayHaveSideEffects(inst, mFunctionInfoMap) || requiresSpecialHandling(inst)) return;
#endif

    // If this is an OP_VARYING alloca of a type with nested pointers that has
    // OP_SEQUENTIAL uses, we have to split it up as well to prevent pointer-
    // to-vector vs. vector-of-pointers problems. Essentially, this means that
    // if there is an OP_SEQUENTIAL use, we consider any pointer to be a vector
    // of pointers, which we currently do not support natively.
    if (isa<AllocaInst>(inst) && WFV::hasNestedPointer(*inst.getType()))
    {
        for (Instruction::const_user_iterator U=inst.user_begin(), UE=inst.user_end(); U!=UE; ++U)
        {
            assert (isa<Instruction>(*U));
            const Instruction* useI = cast<Instruction>(*U);
            if (WFV::hasMetadata(useI, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
                WFV::hasMetadata(useI, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                return true;
            }
        }
    }

    // For performance reasons, if the alloca is an array (that is not tiny) and
    // has OP_SEQUENTIAL uses, we also mark it SEQUENTIAL to prevent too many
    // copy operations.
    if (isa<AllocaInst>(inst) &&
        (inst.getType()->getPointerElementType()->isArrayTy() &&
         inst.getType()->getPointerElementType()->getArrayNumElements() > 16))
    {
        for (Instruction::const_user_iterator U=inst.user_begin(), UE=inst.user_end(); U!=UE; ++U)
        {
            assert (isa<Instruction>(*U));
            const Instruction* useI = cast<Instruction>(*U);
            if (WFV::hasMetadata(useI, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
                WFV::hasMetadata(useI, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                return true;
            }
        }
    }

    // If the return type can not be vectorized, it has to be RES_SCALARS.
    // NOTE: A pointer return type *can* be okay, since it may be transformed to
    //       a pointer to a vector whereas a vector of pointers would not be okay.
    //       However, this depends on the OP_SEQUENTIAL/_GUARDED property, so we
    //       can not yet decide that.
    // TODO: Using returnsVoidPtr() here effectively disallows <W x i8>*.
    if (!WFV::returnsVoidPtr(inst) && WFV::isVectorizableType(*inst.getType())) return false;

    return true;
}

// An instruction requires guards only if it may have side effects.
// An instruction requires guards only if its parent block has a non-fully-uniform mask.
// Exceptions:
// - We never guard loads but leave this to blending (may not be okay if race conditions occur).
// - We don't have to guard calls that have a mask parameter (these are not considered
//   as "may have side effects").
// - TODO: Do some instructions from isVectorizableInst() require guards?
bool
VectorizationAnalysis::mayRequireGuards(const Instruction& inst) const
{
    if (!WFV::mayHaveSideEffects(inst, mFunctionInfoMap)) return false;

    if (mDisableAllAnalyses) return true;

    // TODO: Unsure whether guarding scalar loads with non-uniform mask
    //       could be faster than executing all W loads.
    //if (isa<LoadInst>(inst)) return false;

    const BasicBlock* parentBB = inst.getParent();

    const bool isAlwaysByAllOrNone =
        WFV::hasMetadata(parentBB, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) ||
        WFV::hasMetadata(parentBB, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE);

    return !isAlwaysByAllOrNone;
}

bool
VectorizationAnalysis::requiresSplit(const Instruction& inst) const
{
    // Ignore our own metadata calls.
    if (WFV::isMetadataCall(&inst)) return false;

    if(mVerbose) outs() << "testing if instruction has to be split: " << inst << "...\n";

    // Ignore instructions that were marked previously.
    if (WFV::hasMetadata(&inst, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
        WFV::hasMetadata(&inst, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
    {
        if(mVerbose) outs() << "  has been marked as OP_SEQUENTIAL/_GUARDED already "
            << "- ignored: " << inst << "\n";
        assert ((!inst.getType()->isPointerTy() ||
                 WFV::hasMetadata(&inst, WFV::WFV_METADATA_RES_SCALARS)) &&
                "OP_SEQUENTIAL instruction with pointer type has to be RES_SCALARS!");
        return false;
    }

    if (mValueInfoMap->hasMapping(inst))
    {
        if(mVerbose) outs() << "  has been marked via API function - ignored: "
            << inst << "\n";
        return false;
    }

    // Determine properties relevant for splitting of any instruction.
    // - Instructions that are RES_SCALARS and non-uniform have to be split.
    // - Instructions that have at least one operand that is RES_SCALARS
    //   (or has non-vectorizable type in case of a constant) always
    //   have to be split.
    const bool isOpUniform = WFV::hasMetadata(&inst, WFV::WFV_METADATA_OP_UNIFORM);
    const bool mayHaveSideEffects = WFV::mayHaveSideEffects(inst, mFunctionInfoMap);
    const bool hasSequentialOp = hasSequentialOperand(inst);
    const bool isResScalars = WFV::hasMetadata(&inst, WFV::WFV_METADATA_RES_SCALARS);


    // Uniform instructions without side effects never have to be split.
    if (isOpUniform && !mayHaveSideEffects && !hasSequentialOp)
    {
        if(mVerbose) outs() << "  is OP_UNIFORM - ignored: " << inst << "\n";
        return false;
    }

    assert ((isOpUniform ||
             inst.getType()->isVoidTy() ||
             WFV::isVectorizableType(*inst.getType()) ||
             WFV::hasMetadata(&inst, WFV::WFV_METADATA_INDEX_CONSECUTIVE) ||
             WFV::hasMetadata(&inst, WFV::WFV_METADATA_RES_SCALARS)) &&
            "instruction with non-vectorizable type has to be RES_SCALARS or INDEX_CONSECUTIVE!");

    if (isResScalars)
    {
        // TODO: If a call has to be split only due to operands or its
        //       result type, it is not necessary to guard it.
        assert (!isOpUniform);
        if(mVerbose) outs() << "  is RES_SCALARS - marking as OP_SEQUENTIAL/_GUARDED: "
            << inst << "\n";
        return true;
    }

    if (hasSequentialOp)
    {
        // TODO: If a call has to be split only due to operands or its
        //       result type, it is not necessary to guard it.
        if(mVerbose) outs() << "  has operand that requires sequential execution - "
            << "marking as OP_SEQUENTIAL/_GUARDED: " << inst << "\n";
        return true;
    }

    // Do not perform a split if the value produces a result
    // that is INDEX_CONSECUTIVE: This can be handled by
    // the instruction vectorizer (broadcast & add <0,1,2,3>).
    if (mDisableMemAccessAnalysis)
    {
        if (WFV::hasMetadata(&inst, WFV::WFV_METADATA_INDEX_CONSECUTIVE) &&
            !isa<GetElementPtrInst>(inst))
        {
            if(mVerbose) outs() << "  is INDEX_CONSECUTIVE - ignored!\n";
            return false;
        }
    }
    else
    {
        if (WFV::hasMetadata(&inst, WFV::WFV_METADATA_INDEX_CONSECUTIVE))
        {
            if(mVerbose) outs() << "  is INDEX_CONSECUTIVE - ignored!\n";
            return false;
        }
    }

    // Instructions that can not be vectorized always have to be duplicated.
    if (!WFV::isVectorizableInst(inst) &&
        !WFV::hasMetadata(&inst, WFV::WFV_METADATA_OP_UNIFORM))
    {
        if(mVerbose) outs() << "  is not vectorizable - marking as OP_SEQUENTIAL...\n";
        return true;
    }

    // If this is no special instruction, we are done with the analysis,
    // and it remains OP_VARYING.
    if (!mayHaveSideEffects && !requiresSpecialHandling(inst))
    {
        return false;
    }

    switch (inst.getOpcode())
    {
        case Instruction::Load:          return requiresSplit(cast<LoadInst>(inst));
        case Instruction::Store:         return requiresSplit(cast<StoreInst>(inst));
        case Instruction::Call:          return requiresSplit(cast<CallInst>(inst));
        case Instruction::GetElementPtr: return requiresSplit(cast<GetElementPtrInst>(inst));
        case Instruction::PHI:           return requiresSplit(cast<PHINode>(inst));
        case Instruction::Select:        return requiresSplit(cast<SelectInst>(inst));
        default:
        {
            errs() << "ERROR: bad instruction for special handling found: " << inst << "\n";
            assert (false && "bad instruction for special handling found!");
            return true;
        }
    }
}

// The following options exist for a load:
// a) The pointer is INDEX_SAME -> create scalar load (automatically broadcasted later)
// b) The pointer is INDEX_CONSECUTIVE / ALIGN_TRUE -> create vector load
// c) The pointer is INDEX_CONSECUTIVE / ALIGN_FALSE -> create unaligned vector load
// d) The pointer is INDEX_RANDOM -> create "gather" (split into W scalar loads + merge)
// e) The pointer has a non-simple type -> create "gather" (split into W scalar loads + merge)
bool
VectorizationAnalysis::requiresSplit(const LoadInst& load) const
{
    // If the uniform value analysis marked the load as OP_UNIFORM
    // then it can remain so.
    if (WFV::hasMetadata(&load, WFV::WFV_METADATA_OP_UNIFORM))
    {
        if(mVerbose) outs() << "  is OP_UNIFORM - ignored!\n";
        return false;
    }

    const Value* pointer = load.getPointerOperand();

    // TODO: if pointer is no instruction/argument, is it always INDEX_RANDOM?
    // NOTE: Until optimized codegen is implemented, we consider SHUFFLE and STRIDED as RANDOM.
    const bool hasRandomPtr = (!isa<Instruction>(pointer) && !isa<Argument>(pointer)) ||
        WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_RANDOM) ||
        WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_SHUFFLE) ||
        WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_STRIDED) ||
        (!WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_SAME) &&
         !WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_CONSECUTIVE) &&
         !WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_SHUFFLE) &&
         !WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_STRIDED));
    const bool hasNonSimpleNonPointerTargetType =
        !load.getType()->isPointerTy() &&
        !WFV::isVectorizableType(*load.getType());
    const bool hasNonFullyUniformMask =
        !WFV::hasMetadata(load.getParent(), WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) &&
        !WFV::hasMetadata(load.getParent(), WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE);

    if (!mDisableMemAccessAnalysis && !hasRandomPtr && !hasNonSimpleNonPointerTargetType && !hasNonFullyUniformMask)
    {
        if(mVerbose) outs() << "    load can be VECTORIZED!\n";
        assert (WFV::hasMetadata(&load, WFV::WFV_METADATA_OP_VARYING));
        return false;
    }

    if(mVerbose) {
        if (hasRandomPtr) outs() << "    has INDEX_RANDOM pointer operand!\n";
        if (hasNonSimpleNonPointerTargetType)
            outs() << "    has pointer operand with non-simple, non-pointer type!\n";

        outs() << "    requires splitting ('gather')!\n";
    }
    return true;
}

// A store has to be split if
// - its pointer operand is INDEX_RANDOM
// - the block's entry mask is not FULLY_UNIFORM
// - the store is of non-simple type (neither int nor float)
bool
VectorizationAnalysis::requiresSplit(const StoreInst& store) const
{
    // If the uniform value analysis marked the store as OP_UNIFORM,
    // then it can remain so.
    // NOTE: We do not consider race conditions as side effects
    //       that we have to retain (4x store compared to 1x store
    //       may have a higher chance of resulting in a race condition).
    if (WFV::hasMetadata(&store, WFV::WFV_METADATA_OP_UNIFORM))
    {
        if(mVerbose) outs() << "  is OP_UNIFORM - ignored!\n";
        return false;
    }

    const Value* value   = store.getValueOperand();
    const Value* pointer = store.getPointerOperand();

    // TODO: if pointer is no instruction/argument, is it always INDEX_RANDOM?
    // NOTE: Until optimized codegen is implemented, we consider SHUFFLE and STRIDED as RANDOM.
    const bool hasRandomPtr = (!isa<Instruction>(pointer) && !isa<Argument>(pointer)) ||
        WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_RANDOM) ||
        WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_SHUFFLE) ||
        WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_STRIDED) ||
        (!WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_SAME) &&
         !WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_CONSECUTIVE) &&
         !WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_SHUFFLE) &&
         !WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_STRIDED));
    const bool hasNonFullyUniformMask =
        !WFV::hasMetadata(store.getParent(), WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) &&
        !WFV::hasMetadata(store.getParent(), WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE);
    const bool hasNonSimpleNonPointerTargetType =
        !value->getType()->isPointerTy() &&
        !WFV::isVectorizableType(*value->getType());

    if (!mDisableMemAccessAnalysis &&
        !hasRandomPtr &&
        !hasNonFullyUniformMask &&
        !hasNonSimpleNonPointerTargetType)
    {
        if(mVerbose) outs() << "    store can be VECTORIZED!\n";
        assert (WFV::hasMetadata(&store, WFV::WFV_METADATA_OP_VARYING));
        return false;
    }

    if(mVerbose) {
        if (hasRandomPtr)outs() << "  has INDEX_RANDOM pointer operand!\n";
        if (hasNonFullyUniformMask) outs() << "  has mask that is not FULLY_UNIFORM!\n";
        if (hasNonSimpleNonPointerTargetType) outs() << "    has pointer operand with non-simple/non-pointer type!\n";

        outs() << "    requires splitting ('scatter')!\n";
    }
    return true;
}

// A GEP has to be split if
// - it has any RES_VECTOR/RES_SCALARS indices.
// - the resulting pointer does not have a recognized vector shape (e.g. CONSECUTIVE)
bool
VectorizationAnalysis::requiresSplit(const GetElementPtrInst& gep) const
{
    assert (!WFV::hasMetadata(&gep, WFV::WFV_METADATA_OP_UNIFORM));

    if (mDisableMemAccessAnalysis)
    {
        return true;
    }

    // NOTE: We have already checked with hasSequentialOperand(), so here we
    //       only have to look for varying indices.
    if (!hasVaryingIndex(gep))
    {
        assert (WFV::hasMetadata(&gep, WFV::WFV_METADATA_OP_VARYING));
        return false;
    }

    if(mVerbose) outs() << "  requires splitting (has varying index)!\n";
    return true;
}

// A phi has to be split if
// - it has pointer-type
// - it has non-vectorizable type
// NOTE: A pointer-phi does not have to be split in all cases, only if it
//       will really hold W different pointers at runtime - then, it does
//       not matter whether it will be a pointer-select or still be a
//       pointer-phi (in case of uniform control-flow), we have to split in
//       both cases.
bool
VectorizationAnalysis::requiresSplit(const PHINode& phi) const
{
    assert (!WFV::hasMetadata(&phi, WFV::WFV_METADATA_OP_UNIFORM));
    assert (WFV::hasMetadata(&phi, WFV::WFV_METADATA_OP_VARYING));

    if (phi.getType()->isPointerTy())
    {
        if(mVerbose) outs() << "  requires splitting (VARYING phi of pointer type)\n";
        return true;
    }

    if (!WFV::isVectorizableType(*phi.getType()))
    {
        if(mVerbose) outs() << "  requires splitting (VARYING phi of non-vectorizable type)\n";
        return true;
    }

    return false;
}

// A select has to be split if
// - it is a pointer-select and not uniform
bool
VectorizationAnalysis::requiresSplit(const SelectInst& select) const
{
    assert (!WFV::hasMetadata(&select, WFV::WFV_METADATA_OP_UNIFORM));

    const bool hasPointerTy = select.getType()->isPointerTy();
    const bool isStructuredTy = !WFV::IsPrimitiveType(*select.getType());

    assert (hasPointerTy || WFV::hasMetadata(&select, WFV::WFV_METADATA_OP_VARYING));

    if(mVerbose) {
        if (hasPointerTy) outs() << "  requires splitting (VARYING pointer select)!\n";
        if (isStructuredTy) outs() << "  requires splitting (SOA type in select)!\n";
    }
    return hasPointerTy || isStructuredTy;
}

bool
VectorizationAnalysis::requiresSplit(const CallInst& call) const
{
    //if (mDisableAllAnalyses) return true; // This is a bit too conservative.
    const Function* callee = call.getCalledFunction();
    if (!callee)
    {
    	return true; // TODO overly pessimistic assumptions for called global variables
    }

    assert (callee);
    // - split (OP_SEQUENTIAL) if the block is "always by all",
    // - split and guard (OP_SEQUENTIAL_GUARDED) otherwise.
    // NOTE: There is no option to splitting unknown calls. We
    //       *always* have to assume the call has visible side-
    //       effects or even returns different values for every
    //       call, so there is no way around executing it W times.
    //       This is even true if the call is readonly or readnone.
    const bool hasMapping = mFunctionInfoMap->hasMapping(*callee);
    const bool mayHaveSideEffects = hasMapping ?
        mFunctionInfoMap->mayHaveSideEffects(*callee) : true;

    return mayHaveSideEffects;
}

// Returns false if the GEP requires splitting, true otherwise.
bool
VectorizationAnalysis::hasVaryingIndex(const GetElementPtrInst& gep) const
{
    for (GetElementPtrInst::const_op_iterator IDX=gep.idx_begin(),
            IDXE=gep.idx_end(); IDX!=IDXE; ++IDX)
    {
        assert (isa<Value>(IDX));
        const Value* idxV = cast<Value>(IDX);

        if (isa<Constant>(idxV)) continue;
        if (WFV::hasMetadata(idxV, WFV::WFV_METADATA_RES_UNIFORM)) continue;
        if (WFV::hasMetadata(idxV, WFV::WFV_METADATA_INDEX_CONSECUTIVE)) continue;

        return true;
    }

    return false;
}

bool
VectorizationAnalysis::requiresSpecialHandling(const Instruction& inst) const
{
    return isa<GetElementPtrInst>(inst) ||
        isa<PHINode>(inst) ||
        isa<SelectInst>(inst);
}

bool
VectorizationAnalysis::hasSequentialOperand(const Instruction& inst) const
{
    for (Instruction::const_op_iterator O=inst.op_begin(),
        OE=inst.op_end(); O!=OE; ++O)
    {
        if (operandRequiresSequentialExec(**O)) return true;
    }

    return false;
}

bool
VectorizationAnalysis::operandRequiresSequentialExec(const Value& value) const
{
    assert (!value.getType()->isVoidTy() && "how can an operand be of type void?!");
    if (value.getType()->isLabelTy()) return false;
    if (value.getType()->isFunctionTy()) return false;
    if (isa<PointerType>(value.getType()) &&
        value.getType()->getPointerElementType()->isFunctionTy()) return false;

    if (isa<Instruction>(value) || isa<Argument>(value))
    {
        assert (!(WFV::hasMetadata(&value, WFV::WFV_METADATA_RES_SCALARS) &&
                  WFV::hasMetadata(&value, WFV::WFV_METADATA_INDEX_CONSECUTIVE)) &&
                "value can not be RES_SCALARS and INDEX_CONSECUTIVE!");

        return WFV::hasMetadata(&value, WFV::WFV_METADATA_RES_SCALARS);
    }

    return false;
}

////////////////////////////////////////////////////////////////////////////
//                           MASK INFO ANALYSIS                           //
////////////////////////////////////////////////////////////////////////////

bool
VectorizationAnalysis::analyzeMaskInfo(Function* scalarFn)
{
    if(mVerbose) outs() << "\nMarking mask operations...\n";

    bool changed = false;
    for (Function::iterator BB=scalarFn->begin(), BBE=scalarFn->end(); BB!=BBE; ++BB)
    {
        for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I)
        {
            if (BranchInst* br = dyn_cast<BranchInst>(I))
            {
                if (br->isConditional())
                {
                    if (!isa<Instruction>(br->getCondition())) continue;
                    changed |= markAsMask(cast<Instruction>(br->getCondition()));
                }
            }
            else if (SelectInst* sel = dyn_cast<SelectInst>(I))
            {
                // TODO: HERE! Also mark arguments as masks?
                if (!isa<Instruction>(sel->getCondition())) continue;
                changed |= markAsMask(cast<Instruction>(sel->getCondition()));
            }
        }
    }

    return changed;
}

bool
VectorizationAnalysis::markAsMask(Instruction* inst)
{
    assert (inst);

    if(mVerbose) outs() << "masking instruction";

    if (WFV::hasMetadata(inst, WFV::WFV_METADATA_MASK)) return false;
    if (!inst->getType()->isIntegerTy(1)) return false;

    bool changed = markValueAs(inst, WFV::WFV_METADATA_MASK);

    // Stop at compare instructions
    if (isa<CmpInst>(inst)) return changed;

    // If this is no compare instruction, go backwards and mark operands as MASK.
    for (Instruction::op_iterator O=inst->op_begin(), OE=inst->op_end(); O!=OE; ++O)
    {
        if (!isa<Instruction>(*O)) continue;

        Instruction* opInst = cast<Instruction>(*O);
        changed |= markAsMask(opInst);
    }

    return changed;
}

////////////////////////////////////////////////////////////////////////////
//                                 MISC                                   //
////////////////////////////////////////////////////////////////////////////

bool
VectorizationAnalysis::markValueAs(Value* value, const char* mark)
{
    assert (value && mark);

    if(mVerbose) outs() << "  marking value: " << value->getName() << " as " << mark << "...\n";

    if(strcmp(mark, WFV::WFV_METADATA_ALIGNED_TRUE) == 0 &&
            WFV::hasMetadata(value, WFV::WFV_METADATA_ALIGNED_FALSE))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_OP_UNIFORM) == 0 &&
           (WFV::hasMetadata(value, WFV::WFV_METADATA_OP_VARYING) ||
            WFV::hasMetadata(value, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
            WFV::hasMetadata(value, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED)))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_OP_VARYING) == 0 &&
            (WFV::hasMetadata(value, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
            WFV::hasMetadata(value, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED)))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_OP_SEQUENTIAL) == 0 &&
            WFV::hasMetadata(value, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_RES_UNIFORM) == 0 &&
            (WFV::hasMetadata(value, WFV::WFV_METADATA_RES_VECTOR) ||
            WFV::hasMetadata(value, WFV::WFV_METADATA_RES_SCALARS)))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_RES_VECTOR) == 0 &&
            WFV::hasMetadata(value, WFV::WFV_METADATA_RES_SCALARS))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_INDEX_SAME) == 0 &&
            (WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_CONSECUTIVE) ||
            WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_SHUFFLE) ||
            WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_STRIDED) ||
            WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_RANDOM)))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_INDEX_CONSECUTIVE) == 0 &&
            (WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_SHUFFLE) ||
            WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_STRIDED) ||
            WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_RANDOM)))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_INDEX_SHUFFLE) == 0 &&
            (WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_STRIDED) ||
            WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_RANDOM)))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_INDEX_STRIDED) == 0 &&
            (WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_SHUFFLE) ||
            WFV::hasMetadata(value, WFV::WFV_METADATA_INDEX_RANDOM)))
        return false;

    if(strcmp(mark, WFV::WFV_METADATA_DIVERGENT_FALSE) == 0 &&
            WFV::hasMetadata(value, WFV::WFV_METADATA_DIVERGENT_TRUE))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) == 0 &&
            WFV::hasMetadata(value, WFV::WFV_METADATA_ALWAYS_BY_ALL_FALSE))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE) == 0 &&
            WFV::hasMetadata(value, WFV::WFV_METADATA_ALWAYS_BY_ALL_FALSE))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) == 0 &&
            WFV::hasMetadata(value, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_LOOP_DIVERGENT_FALSE) == 0 &&
            WFV::hasMetadata(value, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE))
        return false;
    if(strcmp(mark, WFV::WFV_METADATA_OPTIONAL) == 0 &&
            WFV::hasMetadata(value, WFV::WFV_METADATA_MANDATORY))
        return false;

    if (WFV::hasMetadata(value, mark))
    {
        if(mVerbose) outs() << "    previously marked as " << mark << " - ignored!\n";
        return false;
    }

    WFV::setMetadata(value, mark);
    return true;
}
#ifdef WFV_ENABLE_LEGACY_API
bool
operator==(const VectorizationAnalysis::EdgeMapType& edgeMap1,
           const VectorizationAnalysis::EdgeMapType& edgeMap2)
{
    if (edgeMap1.size() != edgeMap2.size()) return false;

    for (const auto &it : edgeMap1)
    {
        const BranchInst*            branch = it.first;
        const std::pair<bool, bool>& edges  = it.second;
        if (!edgeMap2.count(branch)) return false;
        const auto it2 = edgeMap2.find(branch);
        if (it2->second.first != edges.first) return false;
        if (it2->second.second != edges.second) return false;
    }
    return true;
}

bool
operator!=(const VectorizationAnalysis::EdgeMapType& edgeMap1,
           const VectorizationAnalysis::EdgeMapType& edgeMap2)
{
    return !(edgeMap1 == edgeMap2);
}

raw_ostream&
operator<<(raw_ostream& ro, const VectorizationAnalysis::EdgeMapType& edgeMap)
{
    for (const auto &it : edgeMap)
    {
        const BranchInst*            branch       = it.first;
        const std::pair<bool, bool>& edges        = it.second;
        const BasicBlock*            branchParent = branch->getParent();

        ro << "    * " << branchParent->getName() << ""
                << (edges.first  ? " (T)" : "")
                << (edges.second ? " (F)" : "");
        ro << "\n";
    }
    return ro;
}

raw_ostream&
operator<<(raw_ostream& ro, const VectorizationAnalysis::DivergenceInfo& info)
{
    if (info.mReference)
    {
        ro << "    references block '" << info.mReference->getName() << "'!\n";
    }

    if (info.mReachableEdges)
    {
        ro << *info.mReachableEdges;
    }
    else
    {
        ro << "    not reachable!\n";
    }

    return ro;
}

raw_ostream&
operator<<(raw_ostream& ro, const VectorizationAnalysis::ReachableEdgesMapType& edgeMap)
{
    for (const auto &it : edgeMap)
    {
        if (!it.second) continue; // Not reachable.

        const BasicBlock* block = it.first;
        ro << "  block: " << block->getName() << "\n";
        ro << *it.second;
    }

    return ro;
}
#endif
bool
WFV::VerifyVectorizationAnalysis(const Function& f)
{
    bool verified = true;

    for (Function::const_iterator BB=f.begin(), BBE=f.end(); BB!=BBE; ++BB)
    {
        const bool isMandatory         = WFV::hasMetadata(&*BB, WFV::WFV_METADATA_MANDATORY);
        const bool isOptional          = WFV::hasMetadata(&*BB, WFV::WFV_METADATA_OPTIONAL);
        const bool isDivergent         = WFV::hasMetadata(&*BB, WFV::WFV_METADATA_DIVERGENT_TRUE);
        const bool isNonDivergent      = WFV::hasMetadata(&*BB, WFV::WFV_METADATA_DIVERGENT_FALSE);
        const bool isAlwaysByAll       = WFV::hasMetadata(&*BB, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE);
        const bool isAlwaysByAllOrNone = WFV::hasMetadata(&*BB, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE);
        const bool isNotAlwaysByAll    = WFV::hasMetadata(&*BB, WFV::WFV_METADATA_ALWAYS_BY_ALL_FALSE);
        const bool isLoopDiv           = WFV::hasMetadata(&*BB, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE);
        const bool isLoopNonDiv        = WFV::hasMetadata(&*BB, WFV::WFV_METADATA_LOOP_DIVERGENT_FALSE);

        // A block must have no or non-ambiguous marks.
        verified &= (unsigned)isMandatory + (unsigned)isOptional <= 1U;
        verified &= (unsigned)isDivergent + (unsigned)isNonDivergent <= 1U;
        verified &= (unsigned)isAlwaysByAll +
                (unsigned)isAlwaysByAllOrNone +
                (unsigned)isNotAlwaysByAll <= 1U;

        // A loop must have no or non-ambiguous marks.
        verified &= (unsigned)isLoopDiv + (unsigned)isLoopNonDiv <= 1U;

        for (BasicBlock::const_iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I)
        {
            // An instruction must have no or non-ambiguous marks per "class"
            const bool isOpUniform    = WFV::hasMetadata(&*I, WFV::WFV_METADATA_OP_UNIFORM);
            const bool isOpParallel   = WFV::hasMetadata(&*I, WFV::WFV_METADATA_OP_VARYING);
            const bool isOpSequential = WFV::hasMetadata(&*I, WFV::WFV_METADATA_OP_SEQUENTIAL);
            const bool isOpSeqGuarded = WFV::hasMetadata(&*I, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED);

            verified &= (unsigned)isOpUniform +
                    (unsigned)isOpParallel +
                    (unsigned)isOpSequential +
                    (unsigned)isOpSeqGuarded <= 1U;

            const bool isResUniform   = WFV::hasMetadata(&*I, WFV::WFV_METADATA_RES_UNIFORM);
            const bool isResVector    = WFV::hasMetadata(&*I, WFV::WFV_METADATA_RES_VECTOR);
            const bool isResScalars   = WFV::hasMetadata(&*I, WFV::WFV_METADATA_RES_SCALARS);

            verified &= (unsigned)isResUniform +
                    (unsigned)isResVector +
                    (unsigned)isResScalars <= 1U;

            const bool isAligned      = WFV::hasMetadata(&*I, WFV::WFV_METADATA_ALIGNED_TRUE);
            const bool isNotAligned   = WFV::hasMetadata(&*I, WFV::WFV_METADATA_ALIGNED_FALSE);

            verified &= (unsigned)isAligned + (unsigned)isNotAligned <= 1U;

            const bool isSame         = WFV::hasMetadata(&*I, WFV::WFV_METADATA_INDEX_SAME);
            const bool isConsecutive  = WFV::hasMetadata(&*I, WFV::WFV_METADATA_INDEX_CONSECUTIVE);
            const bool isShuffle      = WFV::hasMetadata(&*I, WFV::WFV_METADATA_INDEX_SHUFFLE);
            const bool isStrided      = WFV::hasMetadata(&*I, WFV::WFV_METADATA_INDEX_STRIDED);
            const bool isRandom       = WFV::hasMetadata(&*I, WFV::WFV_METADATA_INDEX_RANDOM);

            verified &= (unsigned)isSame +
                    (unsigned)isConsecutive +
                    (unsigned)isShuffle +
                    (unsigned)isStrided +
                    (unsigned)isRandom <= 1U;
        }

        // There must be at most one function call to the metadataFn
        bool callFound = false;
        for (BasicBlock::const_iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I)
        {
            if (!isa<CallInst>(I)) continue;
            const CallInst* call = cast<CallInst>(I);
            if (call->getCalledFunction() != argMetadataFn) continue;

            verified &= !callFound;
            callFound = true;
        }
    }

    return verified;
}
