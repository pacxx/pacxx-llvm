/**
 * @file   variantRegionFactory.cpp
 * @date   13.03.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */

#include "wfv/variantRegions/variantRegionFactory.h"

#include "wfv/variantRegions/variantRegion.h"
#include "wfv/variantRegions/disableVectorizationRegion.h"
#include "wfv/variantRegions/sequentializeRegion.h"

#include <cassert>

using namespace llvm;


namespace WFV {
namespace RegionBased {

VariantRegionFactory::VariantRegionFactory()
{
    Register(DisableVectorizationRegion::ID, &DisableVectorizationRegion::Create);
    Register(SequentializeRegion::ID,        &SequentializeRegion::Create);
    //Register(BOSCCRegion::ID,                &BOSCCRegion::Create);
    //...
    //DYNAMIC_BOSCC;
    //PUMPED_COMPACTION;
    //WFV_SLP;
    //PUMPED_WFV_SLP;
}

VariantRegionFactory::VariantRegionFactory(const VariantRegionFactory& other)
{
}

VariantRegionFactory&
VariantRegionFactory::operator=(const VariantRegionFactory& other)
{
    return *this;
}

VariantRegionFactory::~VariantRegionFactory()
{
    mVariantRegionConstructorMap.clear();
}

VariantRegionFactory*
VariantRegionFactory::Get()
{
    static VariantRegionFactory instance;
    return &instance;
}

void
VariantRegionFactory::Register(VariantRegionType&    type,
                               CreateVariantRegionFn createFnPtr)
{
    assert (!mVariantRegionConstructorMap.count(type) && "Variant region type already registered!");
    mVariantRegionConstructorMap[type] = createFnPtr;
}

VariantRegion*
VariantRegionFactory::CreateVariantRegion(const VariantRegionType& type,
                                          Function*                region,
                                          WFVInfo*                 info)
{
    assert (region && info);

    // Return the corresponding object.
    FactoryMapType::iterator it = mVariantRegionConstructorMap.find(type);
    if (it == mVariantRegionConstructorMap.end()) return nullptr;
    return it->second(region, info);
}

} // namespace RegionBased
} // namespace WFV
