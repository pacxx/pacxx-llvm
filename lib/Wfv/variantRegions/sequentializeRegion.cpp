/**
 * @file   sequentializeRegion.cpp
 * @date   19.03.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */

#include "wfv/variantRegions/sequentializeRegion.h"
#include "wfv/variantRegions/utils.h"
#include "wfv/wfvInfo.h"
#include "wfv/utils/wfvTools.h"

#include "wfv/wfvInterface.h"
#include "wfv/utils/metadata.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/SSAUpdater.h"

using namespace llvm;

namespace WFV {
namespace RegionBased {

VariantRegionType SequentializeRegion::ID = 2; // Needs to be unique.

SequentializeRegion::SequentializeRegion(Function* region,
                                         WFVInfo*  info)
    : VariantRegion(SequentializeRegion::ID), mInfo(info)
{
    assert (region && info);

    // Set properties of the only "variant".
    VariantProperties* VP = new VariantProperties();

    VP->mParent = this;
    VP->mDisableWFV = false;
    VP->mVectorizationFactor = info->mVectorizationFactor;
    VP->mDisableMemAccessAnalysis = info->mDisableMemAccessAnalysis;
    VP->mDisableControlFlowDivAnalysis = info->mDisableControlFlowDivAnalysis;
    VP->mDisableAllAnalyses = info->mDisableAllAnalyses;

    // There is always a mask argument, but here we decide whether
    // we want to ignore it.
    // mRegionCall is not set yet.
    CallInst* call = cast<CallInst>(*region->use_begin());
    if (isNotAlwaysByAllOrNone(*call->getParent()) &&
        isVaryingRegionCall(*call))
    {
        VP->mMaskPosition = region->getFunctionType()->getNumParams()-1;
        assert (region->getFunctionType()->getParamType(VP->mMaskPosition)->isIntegerTy(1));
    }
    else
    {
        VP->mMaskPosition = -1;
    }

    if (regionMayHaveSideEffects(*region, *info))
    {
        DEBUG_WFV( outs() << "SequentializeRegion may have side effects!\n"; );
        VP->mMayHaveSideEffects = true;
    }
    else
    {
        VP->mMayHaveSideEffects = false;
    }

    //VP->mUsePumpedReorganization = false;
    //VP->mPumpedReorganizationFactor = 1;

    addVariantProperties(VP);
}

SequentializeRegion::~SequentializeRegion()
{
    mInputs->clear();
    mOutputs->clear();
    delete mInputs;
    delete mOutputs;
}

VariantRegion*
SequentializeRegion::Create(Function* region, WFVInfo* info)
{
    return new SequentializeRegion(region, info);
}

// Run transformation.
bool
SequentializeRegion::run()
{
    DEBUG_WFV( outs() << "\nRunning SequentializeRegion variant generator...\n"; );
    assert (getNumVariants() == 1);

    // - Create a clone.
    // - Create a SIMD target.
    // - Mark every instruction as OP_SEQUENTIAL or OP_SEQUENTIAL_GUARDED.
    // - Run WFV.
    // - Add SIMD mapping to parent WFV.
    // -> Call to this variant will be replaced by call to "vectorized" function
    //    that actually executes everything sequentially, but instruction by
    //    instruction instead of the whole region sequentially.

    VariantProperties& VP = *getVariantProperties(0);

    ////////////////////////////////////////////////////////////////////////////
    // Clone the region function once, so we can roll back in case of errors.
    ////////////////////////////////////////////////////////////////////////////

    ValueToValueMapTy valueMap;
    Function* V = CloneFunction(mRegionFunction, valueMap);
    VP.mScalarVariant = V;

    assert (V);
    V->setCallingConv(mRegionFunction->getCallingConv());
    V->setAttributes(mRegionFunction->getAttributes());
    V->setAlignment(mRegionFunction->getAlignment());
    V->setLinkage(GlobalValue::InternalLinkage);
    V->setName(mRegionFunction->getName()+".wfv.variant");
    mInfo->mModule->getFunctionList().push_back(V);

    // Map all user-defined uniform/consecutive/aligned values
    // from the original scalar source function to the new function.
    // TODO
    //mInfo->mValueInfoMap.mapValueInformation(valueMap);

    ////////////////////////////////////////////////////////////////////////////
    // Create SIMD target declaration for this variant.
    ////////////////////////////////////////////////////////////////////////////

    Function* V_SIMD = createSIMDTargetDecl(V, VP);
    VP.mSimdVariant = V_SIMD;

    ////////////////////////////////////////////////////////////////////////////
    // Create WFVInterface.
    ////////////////////////////////////////////////////////////////////////////

    WFVInterface::WFVInterface recWFV(mInfo->mModule,
                                      mInfo->mContext,
                                      V,
                                      V_SIMD,
                                      mInfo->mTTI,
                                      VP.mVectorizationFactor,
                                      VP.mMaskPosition,
                                      VP.mDisableMemAccessAnalysis,
                                      VP.mDisableControlFlowDivAnalysis,
                                      VP.mDisableAllAnalyses,
                                      mInfo->mVerbose);


    ////////////////////////////////////////////////////////////////////////////
    // Add WFV Metadata to extracted function:
    // Mark every instruction as OP_SEQUENTIAL or OP_SEQUENTIAL_GUARDED
    // and RES_SCALARS.
    ////////////////////////////////////////////////////////////////////////////

    SmallPtrSet<BasicBlock*, 2> returns;
    WFV::findReturnBlocks(*V, returns);
    assert (returns.size() == 1);
    BasicBlock* returnBB = *returns.begin();

    for (auto &BB : *V)
    {
        for (auto &I : BB)
        {
            if (isa<TerminatorInst>(I)) continue;

            // Don't update the mark of output store operations.
            if (&BB == returnBB && isa<StoreInst>(&I)) continue;

            // We have to make sure that the values that are stored back
            // are RES_VECTOR, otherwise the store operation is sequentialized.
            bool isResultStoredBack = false;
            for (Instruction::use_iterator U=I.use_begin(), UE=I.use_end(); U!=UE; ++U)
            {
                Instruction* useI = cast<Instruction>(*U);
                BasicBlock* useBB = useI->getParent();
                if (useBB != returnBB || !isa<StoreInst>(useI)) continue;

                // The instruction has a use that is a result store operation!
                isResultStoredBack = true;
            }

#if 0
            // TODO: This should also work.
            if (WFV::mayHaveSideEffects(I, &mInfo->mFunctionInfoMap))
            {
                WFV::setMetadata(&I, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED);
            }
            else
            {
                WFV::setMetadata(&I, WFV::WFV_METADATA_OP_SEQUENTIAL);
            }

            if (!I.getType()->isVoidTy())
            {
                if (isResultStoredBack)
                {
                    WFV::setMetadata(&I, WFV::WFV_METADATA_RES_VECTOR);
                }
                else
                {
                    WFV::setMetadata(&I, WFV::WFV_METADATA_RES_SCALARS);
                }
            }
#else
            const bool mayHaveSideEffects =
                WFV::mayHaveSideEffects(I, &mInfo->mFunctionInfoMap);
            const bool isResScalars = !I.getType()->isVoidTy() && !isResultStoredBack;

            recWFV.addSIMDSemantics(I,
                                    false,
                                    false,
                                    !mayHaveSideEffects,
                                    mayHaveSideEffects,
                                    false,
                                    isResultStoredBack,
                                    isResScalars,
                                    false,
                                    false,
                                    false);
#endif
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Run WFV recursively.
    ////////////////////////////////////////////////////////////////////////////

    const bool success = recWFV.run();

    if (!success) return false; // TODO: roll back here?

    ////////////////////////////////////////////////////////////////////////////
    // We don't have to change any marks in the source function.
    // See WFV::RegionBased::setDefaultMetadata().
    ////////////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////////////
    // Add mapping of this variant to WFVInfo (call to R will be replaced
    // by V_SIMD during "parent" WFV).
    // Note that V is never used, so we add a mapping of R to V_SIMD.
    ////////////////////////////////////////////////////////////////////////////

    mInfo->addSIMDMapping(*mRegionFunction, *V_SIMD, VP.mMaskPosition, VP.mMayHaveSideEffects);

    //outs() << "\n\nafter SequentializeRegion generation:\n";
    //outs() << "region function: " << *V_SIMD << "\n";
    //outs() << "source function: " << *mSourceFunction << "\n";

    // If everything went fine, delete the copied region (not used).
    assert (V->use_empty());
    V->eraseFromParent();

    return true;
}

namespace {

struct
OutputArg
{
    unsigned    mArgIndex;
    AllocaInst* mAlloca;
    StoreInst*  mStore;
    LoadInst*   mReload;
};

// TODO: Move to utils.
void
getOutputArgs(CallInst*                   simdCall,
              const unsigned              numOutputs,
              SmallVector<OutputArg*, 2>& outputArgs)
{
    assert (simdCall);
    assert (outputArgs.empty());

    const unsigned numArgs     = simdCall->getNumArgOperands()-1; // Subtract the mask arg.
    const unsigned firstOutIdx = numArgs - numOutputs;

    for (unsigned i=firstOutIdx; i<numArgs; ++i)
    {
        Value* alloca = simdCall->getArgOperand(i);
        assert (isa<AllocaInst>(alloca));
        assert (alloca->getNumUses() == 2); // The call and the reload.
        assert (isa<LoadInst>(*alloca->use_begin()));

        // Get corresponding argument of variant function.
        Function* V = simdCall->getCalledFunction();
        Function::arg_iterator A = V->arg_begin();
        std::advance(A, i);
        Argument* variantArg = &*A;

        // The argument should only have one use that is a store.
        assert (variantArg->getNumUses() == 1);
        assert (isa<StoreInst>(*variantArg->use_begin()));
        StoreInst* storeI = cast<StoreInst>(*variantArg->use_begin());

        OutputArg* outputArg = new OutputArg();
        outputArg->mArgIndex = i;
        outputArg->mAlloca = cast<AllocaInst>(alloca);
        outputArg->mStore = storeI;
        outputArg->mReload = cast<LoadInst>(*alloca->use_begin());
        outputArgs.push_back(outputArg);
    }

    assert (outputArgs.size() == numOutputs);
}

} // unnamed namespace

// Merge variant back into parent function *after* WFV on parent is finished.
bool
SequentializeRegion::merge()
{
    DEBUG_WFV( outs() << "\nMerging SequentializeRegion variant back into parent...\n"; );
    assert (getNumVariants() == 1);

    VariantProperties& VP = *getVariantProperties(0);

    // Find the call to the vectorized function (mRegionCall is not valid anymore).
    assert (mRegionFunction->use_empty());
    assert (VP.mSimdVariant->getNumUses() == 1);
    CallInst* simdCall = cast<CallInst>(*VP.mSimdVariant->use_begin());

    // Get the output arguments with all their corresponding values.
    const unsigned numOutputs = mOutputs->size();
    SmallVector<OutputArg*, 2> outputArgs;
    getOutputArgs(simdCall, numOutputs, outputArgs);

    // Inline the call.
    InlineFunctionInfo IFI;
    InlineFunction(simdCall, IFI);

    // Get rid of allocas/stores/loads of result values.
    for (unsigned i=0, e=numOutputs; i<e; ++i)
    {
        OutputArg* outputArg = outputArgs[i];

        SmallVector<Instruction*, 2> instVec;

        Value* ptr = outputArg->mReload->getPointerOperand();
        assert (isa<AllocaInst>(ptr));
        AllocaInst* alloca = cast<AllocaInst>(ptr);
        for (Value::use_iterator U=alloca->use_begin(), UE=alloca->use_end(); U!=UE; ++U)
        {
            assert (isa<Instruction>(*U));
            assert (isa<LoadInst>(*U) || isa<StoreInst>(*U));
            Instruction* useI = cast<Instruction>(*U);
            instVec.push_back(useI);
        }

        SSAUpdater S;
        LoadAndStorePromoter LASP(instVec, S, "");

        LASP.run(instVec);

        assert (outputArg->mAlloca->use_empty());
        outputArg->mAlloca->eraseFromParent();
    }

    return true;
}

// Verify transformation.
bool
SequentializeRegion::verify() const
{
    if (getNumVariants() != 1) return false;

    const VariantProperties& VP = *getVariantProperties(0);
    const bool verified = !VP.mSimdVariant->isDeclaration();

    return verified;
}

} // namespace RegionBased
} // namespace WFV
