/**
 * @file   disableVectorizationRegion.cpp
 * @date   13.03.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */

#include "wfv/variantRegions/disableVectorizationRegion.h"
#include "wfv/variantRegions/utils.h"
#include "wfv/wfvInfo.h"
#include "wfv/utils/wfvTools.h"

#include "wfv/wfvInterface.h"
#include "wfv/utils/metadata.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"

using namespace llvm;

namespace WFV {
namespace RegionBased {

VariantRegionType DisableVectorizationRegion::ID = 1; // Needs to be unique.

DisableVectorizationRegion::DisableVectorizationRegion(Function* region,
                                                       WFVInfo*  info)
    : VariantRegion(DisableVectorizationRegion::ID), mInfo(info)
{
    assert (region && info);

    // Set properties of the only "variant".
    VariantProperties* VP = new VariantProperties();

    VP->mParent = this;
    VP->mDisableWFV = true;
    VP->mVectorizationFactor = info->mVectorizationFactor;
    VP->mMaskPosition = -1; // Mask can be ignored (becomes unused argument).
    VP->mDisableMemAccessAnalysis = false;
    VP->mDisableControlFlowDivAnalysis = false;
    VP->mDisableAllAnalyses = false;
    //VP->mUsePumpedReorganization = false;
    //VP->mPumpedReorganizationFactor = 1;

    if (regionMayHaveSideEffects(*region, *info))
    {
        DEBUG_WFV( outs() << "DisableVectorizationRegion may have side effects!\n"; );
        VP->mMayHaveSideEffects = true;
    }
    else
    {
        VP->mMayHaveSideEffects = false;
    }

    addVariantProperties(VP);
}

DisableVectorizationRegion::~DisableVectorizationRegion()
{
    mInputs->clear();
    mOutputs->clear();
    delete mInputs;
    delete mOutputs;
}

VariantRegion*
DisableVectorizationRegion::Create(Function* region, WFVInfo* info)
{
    return new DisableVectorizationRegion(region, info);
}

// Run transformation.
bool
DisableVectorizationRegion::run()
{
    assert (getNumVariants() == 1);

    // - Don't create a clone.
    // - Dont' create a SIMD target.
    // - Don't add a SIMD mapping.
    // - Dont' run WFV.
    // -> Parent WFV will treat the call as "unknown", yielding W sequential calls.
    //  TODO: This will not work as intended if we implement "automatic" recursion
    //        of WFV into functions that don't have a mapping but whose code is available.
    //        We could prevent this by creating a mapping and generating a "vectorized"
    //        function as in the sequentalizeRegion variant, but simply copying the code
    //        W times.

    ////////////////////////////////////////////////////////////////////////////
    // There's no code generation to be done for this variant.
    ////////////////////////////////////////////////////////////////////////////

    VariantProperties& VP = *getVariantProperties(0);
    VP.mScalarVariant = mRegionFunction;
    VP.mSimdVariant = nullptr;

    ////////////////////////////////////////////////////////////////////////////
    // There's no metadata to be added to the extracted function.
    ////////////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////////////
    // Add WFV Metadata to source function.
    // See WFV::RegionBased::setDefaultMetadata().
    ////////////////////////////////////////////////////////////////////////////

    // There is exactly one call to the extracted function, which must be
    // marked OP_SEQUENTIAL or OP_SEQUENTIAL_GUARDED (if there may be side effects).
    assert (mRegionCall);
    if (VP.mMayHaveSideEffects)
    {
        WFV::setMetadata(mRegionCall, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED);
    }
    else
    {
        WFV::setMetadata(mRegionCall, WFV::WFV_METADATA_OP_SEQUENTIAL);
    }

    // We don't have to change the marks of the allocas and reloads that
    // correspond to the result values. setDefaultMetadata() does the job.

    //outs() << "\n\nafter DisableVectorizationRegion generation:\n";
    //outs() << "region function: " << *mRegionFunction << "\n";
    //outs() << "source function: " << *mSourceFunction << "\n";

    return true;
}

// Merge variant back into parent function *after* WFV on parent is finished.
bool
DisableVectorizationRegion::merge()
{
    // There's nothing to do for this variant, the call remains in the
    // parent function.
    return true;
}

// Verify transformation.
bool
DisableVectorizationRegion::verify() const
{
    if (getNumVariants() != 1) return false;

    const VariantProperties& VP = *getVariantProperties(0);
    const bool verified = !VP.mSimdVariant;

    return verified;
}

} // namespace RegionBased
} // namespace WFV
