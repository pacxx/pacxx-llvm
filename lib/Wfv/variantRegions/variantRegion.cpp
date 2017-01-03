/**
 * @file   variantRegion.cpp
 * @date   13.03.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */

#include "wfv/variantRegions/variantRegion.h"
#include "wfv/utils/metadata.h"
#include "wfv/variantRegions/disableVectorizationRegion.h"
#include "wfv/variantRegions/sequentializeRegion.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"

#include <cassert>
#include <stdexcept>

using namespace llvm;

namespace WFV {
namespace RegionBased {

VariantRegion::VariantRegion(const VariantRegionType& variantID)
    : mSourceFunction(nullptr),
    mRegionFunction(nullptr),
    mInputs(nullptr),
    mOutputs(nullptr),
    mRegionCall(nullptr),
    mVariantRegionType(variantID)
{
}

VariantRegion::~VariantRegion()
{
    mVariantProperties.clear();
}

const VariantRegionType&
VariantRegion::getVariantRegionType() const
{
    return mVariantRegionType;
}

unsigned
VariantRegion::getNumVariants() const
{
    return mVariantProperties.size();
}

// Variant property accessors.
bool
VariantRegion::getDisableWFV(const unsigned index) const
{
    assert (index < getNumVariants());
    return mVariantProperties[index]->mDisableWFV;
}

unsigned
VariantRegion::getVectorizationFactor(const unsigned index) const
{
    assert (index < getNumVariants());
    return mVariantProperties[index]->mVectorizationFactor;
}

bool
VariantRegion::getDisableMemAccessAnalysis(const unsigned index) const
{
    assert (index < getNumVariants());
    return mVariantProperties[index]->mDisableMemAccessAnalysis;
}

bool
VariantRegion::getDisableControlFlowDivAnalysis(const unsigned index) const
{
    assert (index < getNumVariants());
    return mVariantProperties[index]->mDisableControlFlowDivAnalysis;
}

bool
VariantRegion::getDisableAllAnalyses(const unsigned index) const
{
    assert (index < getNumVariants());
    return mVariantProperties[index]->mDisableAllAnalyses;
}

int
VariantRegion::getMaskPos(const unsigned index) const
{
    assert (index < getNumVariants());
    return mVariantProperties[index]->mMaskPosition;
}

bool
VariantRegion::getMayHaveSideEffects(const unsigned index) const
{
    assert (index < getNumVariants());
    return mVariantProperties[index]->mMayHaveSideEffects;
}

VariantProperties*
VariantRegion::getVariantProperties(const unsigned index) const
{
    assert (index < getNumVariants());
    assert (mVariantProperties[index]);
    return mVariantProperties[index];
}

void
VariantRegion::addVariantProperties(VariantProperties* VP)
{
    assert (VP);
    mVariantProperties.push_back(VP);
}

// Helper function to determine the type of a region.
const VariantRegionType&
getVariantRegionType(const Function& regionFn)
{
    // By construction, the first instruction in the single successor of the entry
    // block is the "start" instruction that has the variant mark.
    // NOTE: See wfvRegionBased.cpp::findVariantRegion().
    const BasicBlock* entryBB = &regionFn.getEntryBlock();
    assert (entryBB->getTerminator()->getNumSuccessors() == 1);
    const BasicBlock* startBB = entryBB->getTerminator()->getSuccessor(0);

    const Instruction* startInst = &*startBB->begin();
    assert (WFV::hasMetadata(startInst));

    // TODO: This is not well designed. Capsulate the functionality directly
    //       in each variant region implementation.
    if (WFV::hasMetadata(startInst, WFV::WFV_METADATA_VARIANT_DISABLE_VECT))
    {
        return DisableVectorizationRegion::ID;
    }
    else if (WFV::hasMetadata(startInst, WFV::WFV_METADATA_VARIANT_SEQUENTIALIZE))
    {
        return SequentializeRegion::ID;
    }
    //else if (WFV::hasMetadata(startInst, WFV::WFV_METADATA_VARIANT_BOSCC))
    //{
        //return BOSCCRegion::ID;
    //}
    // ...

    errs() << "ERROR: Unknown variant region found!\n";
    assert (false && "Unknown variant region found!");
    return DisableVectorizationRegion::ID;
}


} // namespace RegionBased
} // namespace WFV
