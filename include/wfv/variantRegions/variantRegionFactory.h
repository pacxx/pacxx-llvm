/**
 * @file   variantRegionFactory.h
 * @date   13.03.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */
#ifndef _WFVVARIANTREGIONFACTORY_H
#define	_WFVVARIANTREGIONFACTORY_H

#include "llvm/ADT/DenseMap.h"

#include "wfv/variantRegions/variantRegion.h"

// LLVM forward declarations.
namespace llvm {
class Function;
}

// WFVInfo forward declaration.
class WFVInfo;

using namespace llvm;

namespace WFV {
namespace RegionBased {


class VariantRegionFactory {
private:
    VariantRegionFactory();
    VariantRegionFactory(const VariantRegionFactory&);
    VariantRegionFactory& operator=(const VariantRegionFactory&);

    typedef DenseMap<VariantRegionType, CreateVariantRegionFn> FactoryMapType;
    FactoryMapType mVariantRegionConstructorMap;
public:
    ~VariantRegionFactory();

    static VariantRegionFactory* Get();

    void Register(VariantRegionType&    type,
                  CreateVariantRegionFn createFnPtr);

    VariantRegion* CreateVariantRegion(const VariantRegionType& type,
                                       Function*                region,
                                       WFVInfo*                 info);
};

} // namespace RegionBased
} // namespace WFV


#endif	/* _WFVVARIANTREGIONFACTORY_H */
