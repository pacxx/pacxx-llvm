/**
 * @file   disableVectorizationRegion.h
 * @date   13.03.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */
#ifndef _WFVDISABLEVECTORIZATIONREGION_H
#define	_WFVDISABLEVECTORIZATIONREGION_H

#include "wfv/variantRegions/variantRegion.h"

// LLVM forward declarations.
namespace llvm {
class Function;
}

using namespace llvm;


namespace WFV {
namespace RegionBased {

class DisableVectorizationRegion : public VariantRegion {
private:
    DisableVectorizationRegion(Function* region, WFVInfo* info);
    virtual ~DisableVectorizationRegion();

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


#endif	/* _WFVDISABLEVECTORIZATIONREGION_H */
