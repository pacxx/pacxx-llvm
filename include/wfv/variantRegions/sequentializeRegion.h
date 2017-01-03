/**
 * @file   sequentializeRegion.h
 * @date   19.03.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */
#ifndef _WFVSEQUENTIALIZEREGION_H
#define	_WFVSEQUENTIALIZEREGION_H

#include "wfv/variantRegions/variantRegion.h"

// LLVM forward declarations.
namespace llvm {
class Function;
}

using namespace llvm;


namespace WFV {
namespace RegionBased {

class SequentializeRegion : public VariantRegion {
private:
    SequentializeRegion(Function* region, WFVInfo* info);
    virtual ~SequentializeRegion();

public:
    static VariantRegionType ID;
    WFVInfo* mInfo;

    static VariantRegion* Create(Function* region, WFVInfo* info);

    // Abstract virtual functions for actual variant region behavior.
    virtual bool run();
    virtual bool merge();
    virtual bool verify() const;
};

} // namespace RegionBased
} // namespace WFV


#endif	/* _WFVSEQUENTIALIZEREGION_H */
