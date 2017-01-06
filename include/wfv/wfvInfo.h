/**
 * @file   wfvInfo.h
 * @date   31.01.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */
#ifndef _WFVINFO_H
#define	_WFVINFO_H

#include "wfv/wfvConfig.h"
#include "wfv/utils/functionInfoMap.h"
#include "wfv/utils/valueInfoMap.h"

#include "llvm/Pass.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Transforms/Utils/ValueMapper.h" // ValueToValueMapTy
#include "llvm/IR/DataLayout.h"            // DataLayout
#include "llvm/IR/DerivedTypes.h"                 // VectorType
#include "llvm/IR/Constants.h"                    // ConstantInt
#include "llvm/Support/Timer.h"               // TimerGroup

// forward declaration of initializer
namespace llvm {
	void initializeWFVInfoPass(PassRegistry&);
}


using namespace llvm;

//namespace {

class WFVInfo : public ImmutablePass {
public:
    static char ID; // Pass identification, replacement for typeid

    WFVInfo();
    explicit WFVInfo(const WFVInfo& other);
    explicit WFVInfo(Module*         M,
                     LLVMContext*    C,
                     Function* scalarFunction,
                     Function*       simdFunction,
                     TargetTransformInfo* TTI,
                     const unsigned  vectorizationFactor,
                     const int       maskPosition=-1,
                     const bool      disableMemAccessAnalysis=false,
                     const bool      disableControlFlowDivAnalysis=false,
                     const bool      disableAllAnalyses=false,
                     const bool      pacxx=false,
                     const bool      verbose=false,
                     TimerGroup*     timerGroup=nullptr);

    ~WFVInfo();
    virtual void releaseMemory();
    virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    virtual bool runOnModule(Module& M);

    bool addSIMDMapping(const Function& scalarFunction,
                        const Function& simdFunction,
                        const int       maskPosition,
                        const bool      mayHaveSideEffects);

    bool addCommonMappings(const bool useSSE,
                           const bool useSSE41,
                           const bool useSSE42,
                           const bool useAVX,
                           const bool useNEON);

    bool addSIMDSemantics(const Function& f,
                          const bool      isOpUniform,
                          const bool      isOpVarying,
                          const bool      isOpSequential,
                          const bool      isOpSequentialGuarded,
                          const bool      isResultUniform,
                          const bool      isResultVector,
                          const bool      isResultScalars,
                          const bool      isAligned,
                          const bool      isIndexSame,
                          const bool      isIndexConsecutive);

    bool addSIMDSemantics(const Argument& arg,
                          const bool      isResultUniform,
                          const bool      isResultVector,
                          const bool      isResultScalars,
                          const bool      isAligned,
                          const bool      isIndexSame,
                          const bool      isIndexConsecutive);

    bool addSIMDSemantics(const Instruction& inst,
                          const bool         isOpUniform,
                          const bool         isOpVarying,
                          const bool         isOpSequential,
                          const bool         isOpSequentialGuarded,
                          const bool         isResultUniform,
                          const bool         isResultVector,
                          const bool         isResultScalars,
                          const bool         isAligned,
                          const bool         isIndexSame,
                          const bool         isIndexConsecutive);

    Module*              mModule;
    LLVMContext*         mContext;
    DataLayout*          mDataLayout;
    Function*      mScalarFunction;
    Function*            mSimdFunction;
    TargetTransformInfo *mTTI;

    // Information about user-defined functions/values.
    WFV::ValueInfoMap    mValueInfoMap;
    WFV::FunctionInfoMap mFunctionInfoMap;

    // Target information.
    const unsigned       mVectorizationFactor;

    // Position of mask argument (if any, -1 otherwise).
    const int            mMaskPosition;

    // Misc information.
    const bool           mDisableMemAccessAnalysis;
    const bool           mDisableControlFlowDivAnalysis;
    const bool           mDisableAllAnalyses;
    bool                 mInitialized;
    const bool           mPacxx;
    const bool           mVerbose;
    bool*                mFailure;
    TimerGroup*          mTimerGroup;

    // WFV state information.
    bool                 mIsAnalyzed;

    // Vectorized datatypes.
    VectorType*          mVectorTyFloatSIMD;
    VectorType*          mVectorTyIntSIMD;
    VectorType*          mVectorTyDoubleSIMD;
    VectorType*          mVectorTyLongSIMD;
    VectorType*          mVectorTyBoolSIMD;
    Type*                mScalarTyIntSIMD;

    // LLVM Constants.
    Constant*            mConstVecSIMDInt32MinusOne;
    Constant*            mConstVecSIMDF32One;
    ConstantInt*         mConstIntSIMDRegWidthZero;
    ConstantInt*         mConstInt32Zero;
    ConstantInt*         mConstInt32One;
    ConstantInt*         mConstInt32Two;
    ConstantInt*         mConstInt32Three;
    ConstantInt*         mConstBoolTrue;
    ConstantInt*         mConstBoolFalse;
    Constant*            mConstAlignmentSIMD;

    // Alignment information.
    unsigned             mAlignmentScalar;
    unsigned             mAlignmentPtr;
    unsigned             mAlignmentSIMDPtr;
    unsigned             mAlignmentSIMD;

    // NOTE: we must not pre-generate this due to possibly different address spaces
    const PointerType* getPointerVectorType(const PointerType* oldType) const;


private:
    inline Constant* createPacketConstantInt(const int c) const;
    inline Constant* createPacketConstantFloat(const float c) const;

    bool mWFVLibLinked;
    void addCommonMappingsSSE(const bool useSSE41, const bool useSSE42);
    void addCommonMappingsAVX();
    void addCommonMappingsNEON();

    bool initialize();
};

//} // unnamed namespace

#endif	/* _WFVINFO_H */
