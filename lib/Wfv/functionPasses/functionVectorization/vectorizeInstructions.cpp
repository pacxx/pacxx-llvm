/**
 * @file   vectorizeInstructions.cpp
 * @date   31.05.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */

#include "wfv/functionPasses/functionVectorizer.h"
#include "wfv/utils/metadata.h"
#include "wfv/utils/wfvTools.h"

#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"

#include "llvm/IR/IRBuilder.h"

#include <stdexcept>

using namespace llvm;


bool
FunctionVectorizer::vectorizeInstructions(Function* f)
{
    assert (f);

    // Custom replacement for "visit(*f)".
    for (inst_iterator I=inst_begin(f), E=inst_end(f); I!=E; )
    {
        Instruction* inst = &*I++;

        // Ignore special WFV function calls except for pack call
        // which has a function mapping.
        if (isUnpackWFunctionCall(inst)) continue;
        if (WFV::isMetadataCall(inst)) continue;

        // Ignore special WFV argument casts.
        if (isArgCast(*inst)) continue;

        // Ignore functions that are OP_SEQUENTIAL/OP_SEQUENTIAL_GUARDED.
        if (WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
            WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
        {
            continue;
        }

        // Ignore operations that are OP_UNIFORM and not RES_VECTOR.
        if (WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_UNIFORM) &&
            !WFV::hasMetadata(inst, WFV::WFV_METADATA_RES_VECTOR))
        {
            continue;
        }

        // Ignore operations that are INDEX_CONSECUTIVE unless they are GEPs
        // or allocas. This is because we still may need to modify them to
        // return the correct type (i.e. create a pointer cast).
        if (WFV::hasMetadata(inst, WFV::WFV_METADATA_INDEX_CONSECUTIVE) &&
            !isa<GetElementPtrInst>(inst) &&
            !isa<AllocaInst>(inst) &&
            !isa<BitCastInst>(inst))
        {
            continue;
        }

        if(mInfo->mVerbose) outs() << "vectorizeInstruction(" << *inst << " )\n";
        if(!visit(inst))
            return false;
    }

    // Now, replace all uses of calls to entryMaskUseFn by their argument.
    // These calls were only introduced to ensure correct reg2mem/mem2reg
    // for "invisible" mask users, i.e., instructions that only use a mask
    // after instruction vectorization. Without these dummies, the corresponding
    // masks may not dominate the instruction that later uses them anymore.
    if (Function* tmpFn = f->getParent()->getFunction("entryMaskUseFn"))
    {
        for (Function::user_iterator U=tmpFn->user_begin(), UE=tmpFn->user_end(); U!=UE; ++U)
        {
            if(CallInst* call = dyn_cast<CallInst>(*U)) {
                call->replaceAllUsesWith(call->getArgOperand(0));
                call->eraseFromParent();
            }
        }
        tmpFn->eraseFromParent();
    }
    if (Function* tmpFn = f->getParent()->getFunction("entryMaskUseFnSIMD"))
    {
        for (Function::user_iterator U=tmpFn->user_begin(), UE=tmpFn->user_end(); U!=UE; ++U)
        {
            if(CallInst* call = dyn_cast<CallInst>(*U)) {
                call->replaceAllUsesWith(call->getArgOperand(0));
                call->eraseFromParent();
            }
        }
        tmpFn->eraseFromParent();
    }

    // TODO: Move to separate file or into a separate pass.
    for (inst_iterator I=inst_begin(f), E=inst_end(f); I!=E; ++I)
    {
        Instruction* inst = &*I;

        if (WFV::hasMetadata(inst, WFV::WFV_METADATA_PACK_UNPACK)) continue;

        if (WFV::hasMetadata(inst, WFV::WFV_METADATA_RES_VECTOR))
        {
            // RES_VECTOR / INDEX_CONSECUTIVE is allowed to be scalar.
            if (!isa<ReturnInst>(inst) &&
                !WFV::isVectorizedType(*inst->getType()) &&
                !WFV::hasMetadata(inst, WFV::WFV_METADATA_INDEX_CONSECUTIVE))
            {
                outs() << "ERROR: instruction is RES_VECTOR but has no "
                    << "vectorized type:" << *inst << "\n";
                    assert (false);
            }
        }
        else
        {
            if (WFV::isVectorizedType(*inst->getType()))
            {
                outs() << "ERROR: instruction is not RES_VECTOR but has "
                    << "vectorized type:" << *inst << "\n";
                    assert (false);
            }
        }
    }
    return true;
}


//
// Implementation of the InstVisitor interface
//

bool
FunctionVectorizer::visitInstruction(Instruction& I)
{
    errs() << "FATAL ERROR: Can not vectorize instruction: " << I << "\n";
    return false;
}

bool
FunctionVectorizer::visitReturnInst(ReturnInst &I)
{
	Function * func = I.getParent()->getParent();
	Type * retType = func->getReturnType();
	Type * intType = Type::getInt32Ty(I.getContext());

	if (retType->isVoidTy())
		return true;

	Value * retVal = I.getReturnValue();
	auto * retValType = retVal->getType();

	// analysis succeeded, return the only remaining value
	if (retValType == retType)
		return true;

	// analysis has failed or race condition, return the first vector element instead
	Value * extractedValue = generateHorizontalExtract(retVal, ConstantInt::get(intType, 0), "extract", nullptr, &I, *mInfo);
    if(!extractedValue) return false;
	I.setOperand(0, extractedValue);

    return true;
}

bool
FunctionVectorizer::visitBranchInst(BranchInst &I)
{
    assert (!I.isUnconditional() && "unconditional branch must not be OP_VARYING!");

    Value* cond = I.getCondition();

    assert ((mInfo->mDisableControlFlowDivAnalysis || !WFV::hasMetadata(cond, WFV::WFV_METADATA_RES_UNIFORM)) &&
            "condition is uniform, why should we vectorize this branch?");

    assert (WFV::hasMetadata(cond, WFV::WFV_METADATA_MASK) &&
            "condition should be a mask!");

    // Insert ptest
    Instruction* ptest = createPTest(cond, &I);
    if(!ptest)
        return false;

    I.setCondition(ptest);

    // Set branch to UNIFORM to prevent broadcasting of the condition.
    WFV::setMetadata(&I, WFV::WFV_METADATA_OP_UNIFORM);
    WFV::setMetadata(&I, WFV::WFV_METADATA_RES_UNIFORM);

    return true;
}

bool
FunctionVectorizer::visitAllocaInst(AllocaInst &I)
{
    I.mutateType(WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor));
    return true;
}

bool
FunctionVectorizer::visitLoadInst(LoadInst     &I)
{

    Value* pointer = I.getPointerOperand();
    if(mInfo->mVerbose) outs() << "  pointer: " << *pointer << "\n";

    assert (!WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_SAME) &&
            "OP_VARYING load should not have INDEX_SAME pointer!");

    assert (WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_CONSECUTIVE));

#ifdef WFV_FORCE_ALIGNED_MEMOPS
    const bool isAligned = true;
#elif WFV_FORCE_UNALIGNED_MEMOPS
    const bool isAligned = false;
#else
    const bool isAligned = WFV::hasMetadata(pointer, WFV::WFV_METADATA_ALIGNED_TRUE);
#endif
    const unsigned alignment = isAligned ? mInfo->mAlignmentSIMD : 1;

    if (!isAligned)
    {
        if(mInfo->mVerbose) outs() << "  unaligned vector load required!\n";
    }

    // Create pointer cast (redundant if pointer is a pointer to a
    // vector later, but required if e.g. the target array is uniform and
    // the pointer is INDEX_CONSECUTIVE).
    Value* pktPtrCast = createPointerCast(pointer, &I);

    if(WFV::hasMetadata(&I, WFV::WFV_METADATA_OP_MASKED)) {
        if(mInfo->mVerbose) {
            outs() << "generating masked load \n";
        }
        LoadInst *load = &I;
        Value* mask = mMaskAnalysis->getEntryMask(*load->getParent());
        PointerType *PtrTy = cast<PointerType>(pktPtrCast->getType());
        Type *DataType = PtrTy->getElementType();
        Value *PassThru = UndefValue::get(DataType);
        Value *Ops[] = {pktPtrCast,
                        ConstantInt::get(Type::getInt32Ty(*mInfo->mContext), alignment),
                        mask,
                        PassThru};
        Type *OverloadedTypes[] = {DataType, pktPtrCast->getType()};
        Value *Fn = Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::masked_load, OverloadedTypes);

        CallInst *CI = CallInst::Create(Fn, Ops, "masked_load", load);
        WFV::setMetadata(CI, WFV::WFV_METADATA_RES_VECTOR);

        load->mutateType(CI->getType());
        load->replaceAllUsesWith(CI);
        load->eraseFromParent();
        return true;
    }
    else {
        I.mutateType(WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor));

        assert(I.getOperand(0) == I.getPointerOperand() &&
               "LLVM API changed, fix code!");

        I.setOperand(0, pktPtrCast);
        I.setAlignment(alignment);
    }

#ifndef WFV_SILENT_MODE
    outs() << "LOAD WAS VECTORIZED" << (isAligned ? "!\n" : " (UNALIGNED)!\n");
#endif

    return true;
}

bool
FunctionVectorizer::visitStoreInst(StoreInst   &I)
{
    if(mInfo->mVerbose) {
        outs() << "  pointer: " << *I.getPointerOperand() << "\n";
        outs() << "  value: " << *I.getValueOperand() << "\n";
    }

    Value* pointer = I.getPointerOperand();
    Value* value   = I.getValueOperand();

    if(mInfo->mVerbose) outs() << "  block entry mask is FULLY_UNIFORM and pointer "
            << "is not INDEX_RANDOM -> can use vector store!\n";

    //if (analysisResults->isSame(pointer))
    if (WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_SAME))
    {
        // If the pointer is the same for all instances, but a VARYING value
        // should be stored, this means that there is a race condition.
        // NOTE: This should have been detected during vectorization
        //       analysis already, but if desired we just do "something" ;).
        if (!WFV::hasMetadata(value, WFV::WFV_METADATA_RES_UNIFORM))
        {
            errs() << "WARNING: Creating scalar store that writes last "
                    << "value of vector to UNIFORM pointer (fixed behavior "
                    << "for race conditions)!\n";

        } else {
            // Otherwise, all instances store the same value to the same
            // location, so we can simply do a single scalar store.
            if(mInfo->mVerbose) outs() << "    pointer operand is INDEX_SAME, "
                    << "have to store W times to the same location "
                    << "(can optimize: single scalar store)!\n";
        }

        Value* lastElemIdx = ConstantInt::get(*mInfo->mContext,
                                              APInt(32, mInfo->mVectorizationFactor-1));
        Value* lastElem = generateHorizontalExtract(value,
                                                    lastElemIdx,
                                                    "",
                                                    &I,
                                                    &I,
                                                    *mInfo);
        if(!lastElem)
            return false;

        I.replaceUsesOfWith(value, lastElem);

        return true;
    }

    assert (WFV::hasMetadata(pointer, WFV::WFV_METADATA_INDEX_CONSECUTIVE));

    // If we attempt to store to a UNIFORM / INDEX_CONSECUTIVE pointer,
    // we can optimize store operations that store vectors (no compound
    // data types) by creating a vector store via a pointer cast.
    // Otherwise (e.g. to store to a uniform array of structs), we have
    // to do a "scatter" (= split), which was determined by the
    // vectorization analysis.

#ifdef WFV_FORCE_ALIGNED_MEMOPS
    const bool isAligned = true;
#elif WFV_FORCE_UNALIGNED_MEMOPS
    const bool isAligned = false;
#else
    const bool isAligned = WFV::hasMetadata(pointer, WFV::WFV_METADATA_ALIGNED_TRUE);
#endif
    const unsigned alignment = isAligned ? mInfo->mAlignmentSIMD : 1;

    if (!isAligned)
    {
        if(mInfo->mVerbose) outs() << "  unaligned vector store required!\n";
    }

    // Create pointer cast (redundant if depending on GEP that also creates
    // this bitcast, but required if e.g. directly loading from the pointer
    // (element 0).
    Value* pktPtrCast = createPointerCast(pointer, &I);

    if(WFV::hasMetadata(&I, WFV::WFV_METADATA_OP_MASKED)) {
        if(mInfo->mVerbose) {
            outs() << "generating masked store\n";
        }
        StoreInst *store = &I;
        Value* mask = mMaskAnalysis->getEntryMask(*store->getParent());
        Value* value = store->getValueOperand();
        PointerType *PtrTy = cast<PointerType>(pktPtrCast->getType());
        Type *DataTy = PtrTy->getElementType();
        Value *Ops[] = {value,
                        pktPtrCast,
                        ConstantInt::get(Type::getInt32Ty(*mInfo->mContext), alignment),
                        mask};
        Type *OverloadedTypes[] = {DataTy, PtrTy};
        Value *Fn = Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::masked_store, OverloadedTypes);

        CallInst *CI = CallInst::Create(Fn, Ops, "", store);

        store->mutateType(CI->getType());
        store->replaceAllUsesWith(CI);
        store->eraseFromParent();

        return true;
    }
    else {
        assert(I.getOperand(0) == I.getValueOperand() &&
               I.getOperand(1) == I.getPointerOperand() &&
               "LLVM API changed, fix code!");

        I.setOperand(1, pktPtrCast);
        I.setAlignment(alignment);
    }

#ifndef WFV_SILENT_MODE
    outs() << "STORE WAS VECTORIZED" << (isAligned ? "!\n" : " (UNALIGNED)!\n");
#endif

    return true;
}

namespace {

// Helper for visitGetElementPtrInst().
Instruction*
createDummyPointerIfNecessary(Value*         pointer,
                              const unsigned vectorizationFactor,
                              Instruction*   insertBefore)
{
    assert (pointer && insertBefore);

    Type*  oldPointerType = pointer->getType();

    const bool requiresVectorPointer = !WFV::hasMetadata(pointer, WFV::WFV_METADATA_RES_UNIFORM);
    if (!requiresVectorPointer) return nullptr;

    const bool pointerIsVectorized   = WFV::isVectorizedType(*oldPointerType);
    assert (!pointerIsVectorized || requiresVectorPointer);

    Type* vecPointerType = pointerIsVectorized ?
        oldPointerType :
        WFV::vectorizeSIMDType(oldPointerType, vectorizationFactor);

    return WFV::createDummy(vecPointerType, insertBefore);
}

}

bool
FunctionVectorizer::visitGetElementPtrInst(GetElementPtrInst &I)
{
    // Values to be split should be caught before calling this function.
    assert (!WFV::hasMetadata(&I, WFV::WFV_METADATA_OP_SEQUENTIAL) &&
            !WFV::hasMetadata(&I, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED));

    // We only have uniform indices, so we can simply create a vector GEP.
    if(mInfo->mVerbose) outs() << "  GEP can be vectorized!\n";

    // Create vectorized dummy-pointer if necessary.
    // It is possible that a GEP is OP_VARYING but returns a pointer to a scalar
    // which then has to be bitcast for succeeding load/store operations to
    // actually reference a vector of values starting at the returned address.
    Value*       pointer = I.getPointerOperand();
    Instruction* dummy   = createDummyPointerIfNecessary(pointer, mInfo->mVectorizationFactor, &I);

    // Indices remain untouched.
    // Stupid constructor does neither allow supplying idx_begin/end
    // directly nor does GEP provide anything to retrieve whole operand-list.
    std::vector<Value*> indices;
    for (GetElementPtrInst::op_iterator IDX=I.idx_begin();
         IDX != I.idx_end(); ++IDX)
    {
        indices.push_back(*IDX);
    }

    // Create new GEP either with vectorized or with scalar pointer
    // depending on vectorization analysis.
    Instruction *newGEP = GetElementPtrInst::Create(nullptr,
                                                    dummy ? dummy : pointer,
                                                    ArrayRef<Value*>(indices),
                                                    "",
                                                    &I);
    WFV::copyMetadata(newGEP, I);

    if (dummy) {
        newGEP->replaceUsesOfWith(dummy, pointer);
        dummy->eraseFromParent();
    }

    if(mInfo->mVerbose) outs() << "  inserted new GEP: " << *newGEP << "\n";

    // If the pointer is UNIFORM, we now have to create a pointer
    // cast of the result of the GEP (the correct address has to
    // be computed using the UNIFORM type, which is then bitcasted
    // to vector type).
    // NOTE: This is guaranteed to return an instruction if an instruction
    //       is given as first argument.
    newGEP = cast<Instruction>(createPointerCast(newGEP, &I));

    assert (WFV::isVectorizedType(*newGEP->getType()));

    WFV::uncheckedReplaceAllUsesWith(&I, newGEP);
    I.eraseFromParent();

    return true;
}

bool
FunctionVectorizer::visitPHINode(PHINode       &I)
{
    I.mutateType(WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor));
    return true;
}

bool
FunctionVectorizer::visitTruncInst(TruncInst &I)
{
    I.mutateType(WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor));
    return true;
}

bool
FunctionVectorizer::visitZExtInst(ZExtInst &I)
{
    I.mutateType(WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor));
    return true;
}

bool
FunctionVectorizer::visitSExtInst(SExtInst &I)
{
    I.mutateType(WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor));
    return true;
}

bool
FunctionVectorizer::visitFPTruncInst(FPTruncInst &I)
{
    I.mutateType(WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor));
    return true;
}

bool
FunctionVectorizer::visitFPExtInst(FPExtInst &I)
{
    I.mutateType(WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor));
    return true;
}

typedef std::vector<Value*> ValueVec;

static
void
extractAllVectorElements(Value * val, std::vector<Value*> & elemVec, IRBuilder<> & builder) {
	Type * valTy = val->getType();
	if (isa<VectorType>(*valTy)) {
		elemVec.push_back(val);
		return;
	}

	if (StructType * aggType = dyn_cast<StructType>(valTy)) {
		uint numElems = aggType->getNumElements();
		for (uint i =  0; i < numElems; ++i) {
			auto * elemVal = builder.CreateExtractValue(val, i);
			extractAllVectorElements(elemVal, elemVec, builder);
		}
	} else {
		assert(false && "not implemented");
	}
}


static
Value *
mergeVectorElements(Type * simdType, ValueVec::iterator & it, IRBuilder<> & builder) {
	if (isa<VectorType>(*simdType)) {
		Value * vecElem = *it++;
		return vecElem;

	} else if (StructType * structTy = dyn_cast<StructType>(simdType)) {
		Value * structAgg = UndefValue::get(structTy);

		uint32_t i = 0;
		for (auto elemIt = structTy->element_begin(); elemIt != structTy->element_end(); ++elemIt, ++i) {
			Value * soaElement = mergeVectorElements(*elemIt, it, builder);
			structAgg = builder.CreateInsertValue(structAgg, soaElement, i);
		}
		return structAgg;
	} else {
		assert(false && "not implemented");
		return nullptr;
	}
}


bool
FunctionVectorizer::visitSelectInst(SelectInst &I)
{
	auto * selectVal = I.getOperand(0);
	auto * firstVal = I.getOperand(1);
	auto * secondVal = I.getOperand(2);

	Type * simdTy = WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor);

	if (isa<VectorType>(*simdTy)) {
		I.mutateType(simdTy);
		return true;
	}

	ValueVec firstElemVecs;
	ValueVec secondElemVecs;
	ValueVec blendedElemVecs;

	IRBuilder<> builder(&I);
	// fetch
	extractAllVectorElements(firstVal, firstElemVecs, builder);
	extractAllVectorElements(secondVal, secondElemVecs, builder);

	// blend pair-wise
	for (uint i = 0; i < firstElemVecs.size(); ++i) {
		auto * firstElem = firstElemVecs[i];
		auto * secondElem = secondElemVecs[i];
		auto * blendedVec = builder.CreateSelect(selectVal, firstElem, secondElem);
		blendedElemVecs.push_back(blendedVec);
	}

	// insert
	ValueVec::iterator itBegin = blendedElemVecs.begin();
	Value * newBlendedValue = mergeVectorElements(simdTy, itBegin, builder);
    if(!newBlendedValue)
        return false;
	assert(itBegin == blendedElemVecs.end());

	I.mutateType(simdTy);
	I.replaceAllUsesWith(newBlendedValue);
	I.eraseFromParent();
	return true;
}

bool
FunctionVectorizer::visitExtractValueInst(ExtractValueInst &I)
{
    I.mutateType(WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor));
    return true;
}

bool
FunctionVectorizer::visitInsertValueInst(InsertValueInst &I)
{
    I.mutateType(WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor));
    return true;
}


bool
FunctionVectorizer::visitCallInst(CallInst &I)
{
    if(mInfo->mVerbose) outs() << "\nvectorizing call: " << I << "\n";

    assert (!WFV::isVectorizedType(*I.getType()) && "call is already vectorized!");

    // Values to be split should be caught before calling this function.
    assert (WFV::hasMetadata(&I, WFV::WFV_METADATA_OP_VARYING));

    Function* f = I.getCalledFunction();
    assert (f);


    // We should have a 'native' vectorized function in 'nativeMethods'
    // which corresponds to 'f', generate a call to this function
    // TODO: This is not conservative enough: We can only employ the
    //       native method if the parameters match. It is possible
    //       that a native method with a uniform parameter is called
    //       with a varying value. In this case, the call can not
    //       be replaced by the native method but has to be split
    //       into W calls to the scalar function.
    assert (mInfo->mFunctionInfoMap.hasMapping(*f) &&
            "must have native function for call that was marked OP_VARYING!");
    const Function& simdFn = mInfo->mFunctionInfoMap.getSimdFunction(*f);

    if(mInfo->mVerbose)
        outs() << "  found vectorized function: '"
            << simdFn.getName() << "' (replacing call to '"
            << f->getName() << "')\n";

    // If this is a "pack" function that was introduced by ourselves,
    // simply replace it by the added simd counterpart (no mask needed).
    if (isPackWFunctionCall(&I))
    {
        SmallVector<Value*, 4> args;
        for (unsigned i=0, e=I.getNumArgOperands(); i<e; ++i)
        {
            args.push_back(I.getArgOperand(i));
        }

        CallInst* newCall =
                CallInst::Create(const_cast<Function*>(&simdFn),
                                 ArrayRef<Value*>(args),
                                 "",
                                 &I);

        WFV::uncheckedReplaceAllUsesWith(&I, newCall);

        WFV::copyMetadata(newCall, I);

        I.eraseFromParent();
        return true;
    }

    const int maskIndex = mInfo->mFunctionInfoMap.getMaskIndex(*f);
    if(mInfo->mVerbose) outs() << "  mask-argument index: " << maskIndex << "\n";

    Value* mask = nullptr;
    if (maskIndex != -1)
    {
        // Get mask of this block.
        BasicBlock* callBB = I.getParent();

        mask = mMaskAnalysis->getEntryMask(*callBB);
        assert (mask && "the mask associated to block with call is nullptr!");
        if(mInfo->mVerbose) outs() << "    mask: " << *mask << "\n";
    }

    Instruction* newCall = generateNativeVectorFunctionCall(&I, simdFn, mask, maskIndex);
    if(!newCall)
        return false;
    assert (newCall);
    assert (newCall != &I);

    if(mInfo->mVerbose) outs() << "inserted new call: " << *newCall << "\n";

    if (isEntryMaskUseFnCall(&I))
    {
        // We have to update the mask graph with the new call.
        mMaskAnalysis->updateEntryMask(*I.getParent(), newCall, newCall);
    }

#ifndef WFV_SILENT_MODE
    if (!isEntryMaskUseFnCall(&I))
    {
        outs() << "CALL TO FUNCTION '" << f->getName() << "' WAS VECTORIZED!\n";
    }
#endif

    // TODO: is unchecked a good idea? why do we need it?
    //I.replaceAllUsesWith(newCall);
    WFV::uncheckedReplaceAllUsesWith(&I, newCall);
    I.eraseFromParent();

    return true;
}


bool
FunctionVectorizer::visitCastInst(CastInst &I)
{
    I.mutateType(WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor));
    return true;
}

bool
FunctionVectorizer::visitBinaryOperator(BinaryOperator &I)
{
    I.mutateType(WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor));
    return true;
}

bool
FunctionVectorizer::visitCmpInst(CmpInst &I)
{
    I.mutateType(WFV::vectorizeSIMDType(I.getType(), mInfo->mVectorizationFactor));
    return true;
}


Value*
FunctionVectorizer::getMaskArgument(Value*        mask,
                                    Type*         paramType,
                                    DummyVecType& dummies,
                                    Instruction*  insertBefore)
{
    assert (mask && paramType && insertBefore);

    if(mInfo->mVerbose) outs() << "    adding mask-argument: " << *mask << "\n";

    if (Constant* c = dyn_cast<Constant>(mask))
    {
        assert (mask->getType()->isIntegerTy(1) &&
                "mask constant must be of type i1!");
        assert (paramType == mInfo->mVectorTyBoolSIMD &&
                "mask parameter must be of type <W x i1>!");
        return broadcastValue(c, insertBefore, *mInfo);
    }

    // If the mask is UNIFORM, broadcast it, because native functions
    // always expect a vector (a scalar mask does not make sense anyway).
    if (!WFV::hasMetadata(mask, WFV::WFV_METADATA_RES_VECTOR) &&
        !WFV::hasMetadata(mask, WFV::WFV_METADATA_RES_SCALARS))
    {
        mask = broadcastValue(mask, insertBefore, *mInfo);
        if(!mask)
            return nullptr;
    }

    Value* maskRef     = mask;
    Type*  maskRefType = maskRef->getType();

    // Types match -> Return mask argument.
    if (maskRefType == paramType) return maskRef;

    // Type of mask differs from mask parameter of function.
    // This is likely to be unwanted, so we output a warning if
    // if the type sizes do not match (e.g. we allow
    // <2 x i64> == <4 x i32>).
    if (!WFV::typesMatch(paramType, maskRefType))
    {
        errs() << "WARNING: native function requires a mask of type "
                << *paramType << ", but available mask has type: "
                << *maskRefType << "\n";
    }

    if (paramType->isPointerTy() && !maskRefType->isPointerTy())
    {
        // The mask is supplied via pointer but is currently a
        // scalar value: allocate memory for the reference.
        maskRef = WFV::generateAlignedAlloc(maskRefType, *mInfo, insertBefore);

        if(mInfo->mVerbose) outs() << "    generated alloc: " << *maskRef << "\n";

        // Set pointer to point to correct value.
        new StoreInst(mask,
                      maskRef,
                      false,
                      mInfo->mAlignmentSIMD,
                      insertBefore);
    }

    // If the mask is not vectorized yet, introduce a dummy.
    if (!maskRefType->isVectorTy() && paramType->isVectorTy() &&
        WFV::hasMetadata(mask, WFV::WFV_METADATA_RES_VECTOR) &&
        paramType->getVectorElementType() == maskRefType)
    {
        Instruction* dummy = WFV::createDummy(paramType, insertBefore);
        dummies.push_back(std::make_pair(dummy, maskRef));
        return dummy;
    }

    assert (!(maskRefType->isVectorTy() ^ paramType->isVectorTy()) &&
            "mask and parameter type must both be vectors or both scalars!");

    // Introduce special handling of <W x i1> -> <W x i32> / <W x float>.
    if (maskRefType == mInfo->mVectorTyBoolSIMD)
    {
        assert (maskRefType->getVectorNumElements() == mInfo->mVectorizationFactor);
        assert (paramType->getVectorNumElements() == mInfo->mVectorizationFactor);

        Type* paramElemType = paramType->getVectorElementType();
        // For masks, we only allow f32 and integers <= 32bit.
        assert (paramElemType->isIntegerTy() || paramElemType->isFloatTy());
        assert (paramElemType->isFloatTy() ||
                paramElemType->getIntegerBitWidth() <= 32);

        Type* targetType = paramElemType->isIntegerTy() ?
            VectorType::get(paramElemType, mInfo->mVectorizationFactor) :
            mInfo->mVectorTyIntSIMD;

        SExtInst* sext = new SExtInst(maskRef,
                                      targetType,
                                      "",
                                      insertBefore);
        WFV::markMaskOperation(sext);

        if (paramElemType->isIntegerTy())
        {
            return sext;
        }

        BitCastInst* bc = new BitCastInst(sext, paramType, "", insertBefore);
        WFV::markMaskOperation(bc);
        return bc;
    }

    if(mInfo->mVerbose) outs() << "    bitcast from " << *maskRefType
            << " to " << *paramType << " requested!\n";

    BitCastInst* bc = new BitCastInst(maskRef, paramType, "", insertBefore);
    WFV::markMaskOperation(bc);
    return bc;
}

// From a given vector of arguments, create arguments that match
// the given vectorization factor.
// As a precondition, all arguments have to be either uniform or
// match the general vectorization factor. They are then broken down into
// smaller vectors that match the call vectorization factor.
template<unsigned T1, unsigned T2>
void
FunctionVectorizer::createArgumentsForCall(const SmallVector<Value*, T1>& args,
                                           const SmallVector<bool, T1>&   uniformArgs,
                                           const unsigned                 callIndex,
                                           const unsigned                 vectorizationFactor,
                                           const unsigned                 callVecFactor,
                                           SmallVector<Value*, T2>&       callArgs,
                                           Instruction*                   insertBefore,
                                           const WFVInfo&                 info)
{
    assert (args.size() == uniformArgs.size());
    assert (callArgs.empty());
    assert (vectorizationFactor > 1);
    assert (callVecFactor > 1);

    // If we are not splitting, simply return all arguments unchanged.
    // This is not very efficient, but cleaner than introducing special
    // cases outside of this function.
    if (callVecFactor == vectorizationFactor)
    {
        for (unsigned argIndex=0, argEnd=args.size(); argIndex<argEnd; ++argIndex)
        {
            Value* arg = args[argIndex];
            callArgs.push_back(arg);
        }
        return;
    }

    assert (callVecFactor < vectorizationFactor);
    assert (vectorizationFactor % callVecFactor == 0);
    assert (callIndex < (vectorizationFactor / callVecFactor));

    for (unsigned argIndex=0, argEnd=args.size(); argIndex<argEnd; ++argIndex)
    {
        Value* arg = args[argIndex];
        if (uniformArgs[argIndex])
        {
            callArgs.push_back(arg);
            continue;
        }

        Type* argType = arg->getType();
        assert (WFV::isVectorizedType(*argType));
        assert (WFV::getVectorizationFactor(*argType) == vectorizationFactor);
        Type* scalarType = WFV::getScalarFromVectorizedType(argType);
        Type* targetSIMDType = WFV::vectorizeSIMDType(scalarType, callVecFactor);

        SmallVector<Value*, 8> elems;
        for (unsigned i=0; i<callVecFactor; ++i)
        {
            const unsigned elemIdx = i + (callIndex * callVecFactor);
            Value* idxVal = ConstantInt::get(argType->getContext(), APInt(32, elemIdx));
            Value* elem = generateHorizontalExtract(arg,
                                                    idxVal,
                                                    "",
                                                    insertBefore,
                                                    insertBefore,
                                                    info);
            if(!elem)
                return;
            elems.push_back(elem);
        }

        Value* newArg = UndefValue::get(targetSIMDType);
        for (unsigned i=0; i<callVecFactor; ++i)
        {
            Value* idxVal2 = ConstantInt::get(argType->getContext(), APInt(32, i));
            newArg = generateHorizontalInsert(elems[i], newArg, idxVal2, insertBefore, info);
        }

        callArgs.push_back(newArg);
    }

    assert (callArgs.size() == args.size());
}

// This function combines the results of multiple operations that produce
// vectors smaller than the chosen vectorization factor back together to
// one vector.
// The calls have to be exactly in the order in which their elements should be
// combined.
template<unsigned T>
Instruction*
FunctionVectorizer::mergeCallResults(SmallVector<CallInst*, T>& calls,
                                     const unsigned             vectorizationFactor,
                                     const unsigned             callVecFactor,
                                     Instruction*               insertBefore,
                                     const WFVInfo&             info)
{
    assert (!calls.empty());
    assert (vectorizationFactor > 1);
    assert (callVecFactor > 1);

    // If we are not splitting, simply return the call result unchanged.
    // This is not very efficient, but cleaner than introducing special
    // cases outside of this function.
    if (calls.size() == 1)
    {
        assert (callVecFactor == vectorizationFactor);
        return calls[0];
    }

    assert (callVecFactor < vectorizationFactor);
    assert (vectorizationFactor % callVecFactor == 0);

    Type* callType = calls[0]->getType();
    assert (WFV::isVectorizedType(*callType));
    assert (callVecFactor == WFV::getVectorizationFactor(*callType));

    const unsigned numCalls = calls.size();

    SmallVector<Value*, 8> extracts;
    for (unsigned i=0; i<numCalls; ++i)
    {
        CallInst* call = calls[i];

        assert (WFV::isVectorizedType(*call->getType()));
        assert (call->getType() == callType);

        for (unsigned w=0; w<callVecFactor; ++w)
        {
            ConstantInt* idxVal = ConstantInt::get(callType->getContext(), APInt(32, w));
            Value* elem = generateHorizontalExtract(call,
                                                    idxVal,
                                                    "",
                                                    insertBefore,
                                                    insertBefore,
                                                    info);
            if(!elem)
                return nullptr;
            extracts.push_back(elem);
        }
    }

    assert (extracts.size() == vectorizationFactor);

    Type* scalarType = WFV::getScalarFromVectorizedType(callType);
    Type* targetSIMDType = WFV::vectorizeSIMDType(scalarType, vectorizationFactor);

    // TODO: use generateHorizontalMerge() here instead?!
    Value* result = UndefValue::get(targetSIMDType);
    for (unsigned i=0; i<vectorizationFactor; ++i)
    {
        ConstantInt* idxVal = ConstantInt::get(scalarType->getContext(), APInt(32, i));
        result = generateHorizontalInsert(extracts[i], result, idxVal, insertBefore, info);
    }

    return cast<Instruction>(result);
}


Instruction*
FunctionVectorizer::generateNativeVectorFunctionCall(CallInst*       oldCall,
                                                     const Function& simdFn,
                                                     Value*          mask,
                                                     const int       maskIndex)
{
    assert (oldCall);
    assert (mask || maskIndex == -1);

    Function* scalarFn = oldCall->getCalledFunction();
    const std::string& fname = scalarFn->getName();

    if(mInfo->mVerbose) {
        outs() << "  arguments of scalar function:\n";
        for (Function::arg_iterator A = scalarFn->arg_begin(),
                     AE = scalarFn->arg_end(); A != AE; ++A) {
            outs() << "  - " << *A << "\n";
        }

        outs() << "  arguments of vectorized function:\n";
        for (Function::const_arg_iterator A = simdFn.arg_begin();
             A != simdFn.arg_end(); ++A) {
            outs() << "  - " << *A << "\n";
        }
    }

    DummyVecType dummies;

    // Attributes cannot directly be copied due to the additional mask parameter.
    const AttributeList& oldAttrSet = oldCall->getAttributes();

    SmallVector<AttributeSet, 3> arg_attrs;

    for (unsigned int i = 0; i < oldCall->getNumArgOperands(); ++i)
        arg_attrs.push_back(oldAttrSet.getParamAttributes(i));

    AttributeList attrs = AttributeList::get(*mInfo->mContext, oldAttrSet.getFnAttributes(), oldAttrSet.getRetAttributes(), arg_attrs);
    unsigned oldAttrIdx = 1;
    unsigned newAttrIdx = 1;

    // Map arguments to vector function.
    // Generate dummies with correct types for operands on the fly if necessary.
    // NOTE: We must not generate dummies for those values that are uniFform
    //       parameters to the function!
    SmallVector<Value*, 4> args;
    Function::const_arg_iterator vecArg = simdFn.arg_begin();
    CallInst::op_iterator        oldArg = oldCall->op_begin();

    const int numScalarArgs = oldCall->getNumArgOperands();
    const int numVecArgs    = maskIndex == -1 ?
        numScalarArgs :
        numScalarArgs+1;

    unsigned numCallsRequired = 1;

    SmallVector<bool, 4> uniformArgs;
    for (int argIndex=0; argIndex < numVecArgs; ++argIndex, ++vecArg, ++newAttrIdx)
    {
        if(mInfo->mVerbose) {
            outs() << "  arg-index:  " << argIndex << "\n";
            outs() << "  target:  " << *vecArg << "\n";
        }

        Type* paramType = vecArg->getType();

        // If this is the mask parameter index, add the mask to the arguments.
        // Don't increment 'oldArg' in this case, we must not miss an argument.
        if (argIndex == maskIndex)
        {

            Value *mask_arg = getMaskArgument(mask,
                                              paramType,
                                              dummies,
                                              oldCall);
            if(mask_arg)
                args.push_back(mask_arg);
            else
                return nullptr;

            uniformArgs.push_back(WFV::isVectorizedType(*mask->getType()));
            // Don't add any attribute, don't increment oldAttrIdx.
            continue;
        }

        // Otherwise, add the correct argument (broadcast/bitcast if necessary).
        if(mInfo->mVerbose) outs() << "    adding argument: " << **oldArg << "\n";

        assert (isa<Value>(oldArg));
        Value* argVal     = cast<Value>(oldArg);
        Type*  argValType = argVal->getType();

        // If the argument is uniform but the native function
        // expects a vector, broadcast it.
        const bool uniformExpected =
            mInfo->mFunctionInfoMap.isUniformArgument(*scalarFn,
                                                      argIndex);
        uniformArgs.push_back(uniformExpected);

        const bool uniformValue    =
            isa<Constant>(argVal) ||
            (!WFV::hasMetadata(argVal, WFV::WFV_METADATA_RES_VECTOR) &&
             !WFV::hasMetadata(argVal, WFV::WFV_METADATA_RES_SCALARS));

        if (uniformExpected)
        {
            if (uniformValue)
            {
                // UNIFORM/UNIFORM -> Do nothing.
            }
            else
            {
                // UNIFORM/VARYING -> Ouch.
                // The target type is a scalar, but we have a vector.
                // We assume the vector to be uniform and simply extract the first element.
                // NOTE: this is likely to produce wrong results!
                errs() << "WARNING: Native function '" << simdFn.getName()
                        << "' expects UNIFORM argument, but a VARYING value is passed!\n"
                        << "  Assuming implicitly uniform vector, attempting to extract"
                        << " first element of value: " << *argVal << "\n";

                if (WFV::isVectorizableType(*paramType) &&
                    WFV::vectorizeSIMDType(paramType,
                                           mInfo->mVectorizationFactor) == argValType)
                {
                    // TODO: Set alloc position to somewhere else?
                    argVal = generateHorizontalExtract(argVal,
                                                       mInfo->mConstInt32Zero,
                                                       "",
                                                       oldCall,
                                                       oldCall,
                                                       *mInfo);
                    if(!argVal)
                        return nullptr;
                }
                else
                {
                    errs() << "ERROR: argument and parameter type of call to native function"
                            << " '" << simdFn.getName() << "' do not match: "
                            << *paramType << " != " << *argValType << "\n";
                }
            }
        }
        else
        {
            if (uniformValue)
            {
                // VARYING/UNIFORM -> Broadcast.
                argVal = broadcastValue(argVal, oldCall, *mInfo);
                if(!argVal)
                    return nullptr;
            }
            else
            {
                // VARYING/VARYING -> Do nothing unless the value is
                // not yet vectorized. In this case, create a dummy.
                if (!WFV::isVectorizedType(*argValType))
                {
                    Instruction* dummy =
                            WFV::createDummy(WFV::vectorizeSIMDType(argValType,
                                                                    mInfo->mVectorizationFactor),
                                             oldCall);
                    dummies.push_back(std::make_pair(dummy, argVal));
                    argVal = dummy;
                }
            }
        }

        // From here on, use the most recent type (possibly changed by dummy)!
        Type* newArgValType = argVal->getType();

        // In case the function requires a pointer, we have to allocate memory,
        // store the value to this pointer and supply it to the function.
        if (paramType->isPointerTy() && !newArgValType->isPointerTy())
        {
            assert (WFV::typesMatch(paramType, newArgValType->getContainedType(0)) &&
                    "parameter type does not match underlying argument pointer type");

            // Allocate memory for the reference.
            argVal = WFV::generateAlignedAlloc(newArgValType, *mInfo, oldCall);
            if(mInfo->mVerbose) outs() << "    generated alloc: " << *argVal << "\n";

            // Set pointer to point to correct value.
            new StoreInst(cast<Value>(oldArg),
                          argVal,
                          false,
                          mInfo->mAlignmentSIMD,
                          oldCall);
        }

        // If we are vectorizing for a larger vector width than what the
        // native function can use, check if this argument can be split and
        // we can generate correct code by creating multiple calls.
        // The actual extraction is done when creating the calls.
        if (!WFV::typesMatch(newArgValType, paramType) &&
            WFV::typesMatchPumped(*newArgValType, *paramType))
        {
            const unsigned requiredFactor = WFV::getVectorizationFactor(*paramType);
            const unsigned foundFactor = WFV::getVectorizationFactor(*newArgValType);
            assert (requiredFactor != foundFactor &&
                    "there's something wrong with typesMatchPumped()!");

            assert (requiredFactor < foundFactor && "uh oh...");
            assert (foundFactor % requiredFactor  == 0 && "uh oh...");
            const unsigned pumpFactor = foundFactor / requiredFactor;

            if (pumpFactor > 1 && numCallsRequired != pumpFactor)
            {
                if (numCallsRequired > 1)
                {
                    // Different arguments have different pumping factors.
                    // We cannot do something reasonable if e.g. one argument can only
                    // be split into 2 vectors while another argument produces 4.
                    errs() << "ERROR: Arguments to native function '" << simdFn.getName()
                        << "' do not agree on a vectorization factor, can't vectorize!\n";
                    errs() << "       Factors found: " << pumpFactor << ", "
                        << numCallsRequired << "\n";
                    return nullptr;
                }

                numCallsRequired = pumpFactor;
            }
        }

        // Types of parameter and value should match now, unless
        // we just determined that this is a "pumped" vector value.
        if (!WFV::typesMatch(newArgValType, paramType) &&
            numCallsRequired == 1)
        {
            errs() << "ERROR: Native function '" << simdFn.getName()
                << "' expects argument of different type!\n";
            errs() << "       Expected parameter type: " << *paramType << "\n";
            errs() << "       Found argument type    : " << *newArgValType << "\n";
            return nullptr;
        }

        // Store value as argument.
        args.push_back(argVal);
        ++oldArg;

        // Save attribute and increment read index
        // (start reading at 1, 0 is return attribute).
        attrs.addAttributes(*mInfo->mContext, newAttrIdx,
                            oldAttrSet.getParamAttributes(oldAttrIdx++));
    }

    // SSE/AVX round requires additional unsigned to determine rounding mode.
    // TODO: This is too target specific, it should go somewhere else.
    if (fname == "roundf" || fname == "round") args.push_back(mInfo->mConstInt32Zero);
    else if (fname == "floorf" || fname == "floor") args.push_back(mInfo->mConstInt32One);
    else if (fname == "ceilf" || fname == "ceil") args.push_back(mInfo->mConstInt32Two);
    if (fname == "roundf" || fname == "floorf" || fname == "ceilf" ||
        fname == "round" || fname == "floor" || fname == "ceil")
    {
        uniformArgs.push_back(true);
    }

    if(mInfo->mVerbose) {
        outs() << "  arguments:\n";
        for (auto value : args) {
            outs() << " * " << *value << "\n";
        }
    }

    // We have to cast to a non-const function since we add a use :/.
    Function* nonConstSIMDFn = const_cast<Function*>(&simdFn);

    std::string fixedName = simdFn.getReturnType()->isVoidTy() ? "" : fname;

    // First create all argument vectors: In case we have to create
    // more than one call, we want all those extracts and inserts *before*
    // the actual calls to give LLVM all possibilities for optimization.
    assert (mInfo->mVectorizationFactor % numCallsRequired == 0);
    const unsigned callVecFactor = mInfo->mVectorizationFactor / numCallsRequired;

    SmallVector<SmallVector<Value*, 4>*, 2> callArgVecs;
    for (unsigned i=0; i<numCallsRequired; ++i)
    {
        SmallVector<Value*, 4>* callArgs = new SmallVector<Value*, 4>();
        createArgumentsForCall(args,
                               uniformArgs,
                               i,
                               mInfo->mVectorizationFactor,
                               callVecFactor,
                               *callArgs,
                               oldCall,
                               *mInfo);
        callArgVecs.push_back(callArgs);
    }

    // Now create the actual calls.
    SmallVector<CallInst*, 2> calls;
    for (unsigned i=0; i<numCallsRequired; ++i)
    {
        SmallVector<Value*, 4>* callArgs = callArgVecs[i];
        CallInst* newCall = CallInst::Create(nonConstSIMDFn,
                                             ArrayRef<Value*>(*callArgs),
                                             fixedName,
                                             oldCall);

        newCall->setCallingConv(oldCall->getCallingConv());
        newCall->setAttributes(attrs);

        WFV::copyMetadata(newCall, *oldCall);

        delete callArgs;
        calls.push_back(newCall);
    }

    // Create dummy *behind* call.
    Instruction* dummy = WFV::createDummy(Type::getInt32Ty(*mInfo->mContext), oldCall);
    oldCall->moveBefore(dummy);

    // Merge call results back to vector of vectorization factor size.
    Instruction* result = mergeCallResults(calls,
                                           mInfo->mVectorizationFactor,
                                           callVecFactor,
                                           dummy,
                                           *mInfo);

    // Erase dummy again.
    dummy->eraseFromParent();

    // NOTE: We do not have to store back any values of pointer-arguments
    //       because no extraction was performed (only for OP_SEQUENTIAL/GUARDED).

    // If we inserted any dummies, be sure to remove them again.
    for (unsigned i=0; i<dummies.size(); ++i)
    {
        // There can still be a type mismatch due to INDEX_CONSECUTIVE values
        // which are currently handled later. In such a case, the dummy is of
        // vector type while the value itself is still scalar (to be broadcast
        // later).
        // See test RegressionConsecutiveCallArg.
        WFV::uncheckedReplaceAllUsesWith(dummies[i].first, dummies[i].second);
        dummies[i].first->eraseFromParent();
    }

    return result;
}
