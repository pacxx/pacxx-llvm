/**
 * @file   utils.cpp
 * @date   19.03.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */

#include "wfv/variantRegions/utils.h"
#include "wfv/variantRegions/variantRegion.h"
#include "wfv/utils/metadata.h"
#include "wfv/utils/wfvTools.h"
#include "wfv/wfvInfo.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"

using namespace llvm;
using namespace WFV::RegionBased;


bool
WFV::RegionBased::isNotAlwaysByAllOrNone(const BasicBlock& block)
{
    return !WFV::hasMetadata(&block, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) &&
        !WFV::hasMetadata(&block, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE);
}

// Determines whether 'call' has at least one non-uniform argument.
// NOTE: This relies on WFV metadata being available for all arguments
//       of 'call' except the result pointers.
bool
WFV::RegionBased::isVaryingRegionCall(const CallInst& call)
{
    assert (call.getType()->isVoidTy());

    for (unsigned i=0, e=call.getNumArgOperands(); i<e; ++i)
    {
        Value* arg = call.getArgOperand(i);

        if (isa<Constant>(arg)) continue;

        // TODO: Globals?
        assert (!isa<GlobalValue>(arg) && "global values as arguments not implemented!");
        assert (!isa<GlobalVariable>(arg) && "global variables as arguments not implemented!");

        // Ignore output arguments (their properties depend on the
        // input arguments).
        if (isa<AllocaInst>(arg) && !WFV::hasMetadata(arg))
        {
            continue;
        }

        if (!WFV::hasMetadata(arg, WFV::WFV_METADATA_RES_UNIFORM))
        {
            return true;
        }
    }

    return false;
}

bool
WFV::RegionBased::regionMayHaveSideEffects(const Function& regionFn,
                                           const WFVInfo&  info)
{
    SmallPtrSet<const BasicBlock*, 2> returns;
    WFV::findReturnBlocks(regionFn, returns);
    assert (returns.size() == 1);
    const BasicBlock* returnBB = *returns.begin();

    // Determine if there is anything in this function that may have a side effect.
    for (auto &BB : regionFn)
    {
        // We do not count operations in the return block as side effects, since
        // they are only writing back the function's results.
        if (&BB == returnBB) continue;

        for (auto &I : BB)
        {
            if (!WFV::mayHaveSideEffects(I, &info.mFunctionInfoMap)) continue;
            return true;
        }
    }

    return false;
}

// Create SIMD target declaration for this variant.
Function*
WFV::RegionBased::createSIMDTargetDecl(Function*                V,
                                       const VariantProperties& VP)
{
    assert (V);
    assert (V == VP.mScalarVariant);
    assert (V->use_empty() && "newly cloned function must not have uses!");

    CallInst* call = VP.mParent->mRegionCall;

    SmallVector<Type*, 4> paramTypes;
    for (unsigned i=0, e=call->getNumArgOperands(); i<e; ++i)
    {
        Value* arg = call->getArgOperand(i);
        Type* scalarType = arg->getType();

        // Even if no mask is required, we still want to vectorize this argument.
        if ((int)i == VP.mMaskPosition ||
            i == call->getNumArgOperands()-1)
        {
            Type* simdType = WFV::vectorizeSIMDType(scalarType, VP.mVectorizationFactor);
            assert (simdType);
            paramTypes.push_back(simdType);
            continue;
        }

        if (isa<Constant>(arg))
        {
            paramTypes.push_back(scalarType);
            continue;
        }

        // TODO: Globals?
        assert (!isa<GlobalValue>(arg) && "global values as arguments not implemented!");
        assert (!isa<GlobalVariable>(arg) && "global variables as arguments not implemented!");

        if (WFV::hasMetadata(arg, WFV::WFV_METADATA_RES_UNIFORM))
        {
            paramTypes.push_back(scalarType);
            continue;
        }

        Type* simdType = WFV::vectorizeSIMDType(scalarType, VP.mVectorizationFactor);
        assert (simdType);
        paramTypes.push_back(simdType);
    }

    Type* oldReturnType = call->getType();
    assert (oldReturnType == V->getReturnType());

    Type* newReturnType = oldReturnType;
    if (!oldReturnType->isVoidTy() &&
        !WFV::hasMetadata(call, WFV::WFV_METADATA_RES_UNIFORM))
    {
        newReturnType = WFV::vectorizeSIMDType(oldReturnType, VP.mVectorizationFactor);
    }

    FunctionType* newFnType = FunctionType::get(newReturnType, paramTypes, false);

    Function* simdFn = Function::Create(newFnType,
                                        Function::InternalLinkage,
                                        V->getName()+"_SIMD",
                                        V->getParent());
    simdFn->setCallingConv(V->getCallingConv());
    simdFn->setAlignment(V->getAlignment());
    simdFn->setAttributes(V->getAttributes());

    return simdFn;
}
