/**
 * @file   variantRegion.h
 * @date   13.03.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */
#ifndef _WFVVARIANTREGION_H
#define	_WFVVARIANTREGION_H

#include "llvm/ADT/SetVector.h"

// LLVM forward declarations.
namespace llvm {
class Value;
class Function;
class CallInst;
}

// WFVInfo forward declaration.
class WFVInfo;

using namespace llvm;


namespace WFV {
namespace RegionBased {

// Forward declaration.
class VariantRegion;

// Typedef for variant identification.
typedef unsigned VariantRegionType;

// Typedef for constructor functions.
typedef VariantRegion* (*CreateVariantRegionFn)(Function*, WFVInfo*);

// Common properties of all variant types.
struct VariantProperties
{
    VariantRegion* mParent;

    bool     mDisableWFV;
    unsigned mVectorizationFactor;
    bool     mDisableMemAccessAnalysis;
    bool     mDisableControlFlowDivAnalysis;
    bool     mDisableAllAnalyses;
    int      mMaskPosition;
    bool     mMayHaveSideEffects;
    //bool   mUsePumpedReorganization;
    //unsigned mPumpedReorganizationFactor;

    // The current variant (clone of mRegionFunction).
    Function* mScalarVariant;
    // The vectorized variant (or declaration of it before WFV was run).
    Function* mSimdVariant;
};

class VariantRegion
{
public:
    typedef SmallVector<VariantProperties*, 2> VariantPropertyVecType;

    // The original parent function from which the region was extracted.
    Function* mSourceFunction;
    // The extracted region function.
    Function* mRegionFunction;

    // Input and output values of the call to mRegionFunction in mSourceFunction.
    SetVector<Value*>* mInputs;
    SetVector<Value*>* mOutputs;

    // The call to mRegionFunction in mSourceFunction.
    CallInst* mRegionCall;

private:
    const VariantRegionType& mVariantRegionType;
    VariantPropertyVecType mVariantProperties;

public:
    explicit VariantRegion(const VariantRegionType& variantID);
    virtual ~VariantRegion() = 0;

    // Direct accessors to variant properties.
    //
    const VariantRegionType& getVariantRegionType() const;
    unsigned getNumVariants() const;

    bool     getDisableWFV                   (const unsigned index) const;
    unsigned getVectorizationFactor          (const unsigned index) const;
    bool     getDisableMemAccessAnalysis     (const unsigned index) const;
    bool     getDisableControlFlowDivAnalysis(const unsigned index) const;
    bool     getDisableAllAnalyses           (const unsigned index) const;
    int      getMaskPos                      (const unsigned index) const;
    bool     getMayHaveSideEffects           (const unsigned index) const;

    // TODO: remove?
    VariantProperties* getVariantProperties(const unsigned index) const;

    // TODO: remove?
    void addVariantProperties(VariantProperties* VP);

    // Abstract virtual functions for actual variant region behavior.
    virtual bool run() = 0;
    virtual bool merge() = 0;
    virtual bool verify() const = 0;
};

// Helper function to determine the type of a region.
const VariantRegionType& getVariantRegionType(const Function& regionFn);

} // namespace RegionBased
} // namespace WFV


#endif	/* _WFVVARIANTREGION_H */
