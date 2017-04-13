/**
 * @file   broadcastUniformOperands.cpp
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
#include "wfv/utils/wfvTools.h"
#include "wfv/utils/metadata.h"

#include "wfv/wfvConfig.h"

#include "llvm/IR/Instruction.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/IRBuilder.h"

#include <stdexcept>

using namespace llvm;

template<typename ScalarType>
Constant * CreateConstant(ScalarType, Type * llvmType);

template<>
Constant * CreateConstant<float>(float val, Type * floatTy) { return ConstantFP::get(floatTy, val); }
template<>
Constant * CreateConstant<int>(int val, Type * intTy) { return ConstantInt::get(intTy, val, true); }

template<typename DataType>
Value * CreateAdd(Value * A, Value * B, IRBuilder<> & builder);

template<>
Value * CreateAdd<float>(Value * A, Value * B, IRBuilder<> & builder) { return builder.CreateFAdd(A, B); }

template<>
Value * CreateAdd<int>(Value * A, Value * B, IRBuilder<> & builder) { return builder.CreateAdd(A, B); }

template<typename ScalarType>
Value*
CreateScalarBroadcast(Value * scalarValue, Type * simdType, bool isConsecutive, IRBuilder<> & builder, const WFVInfo & info) {
	Value* undefSIMDVal = UndefValue::get(simdType);
	auto * newVal = builder.CreateInsertElement(undefSIMDVal,
			scalarValue,
			info.mConstInt32Zero);
	WFV::setMetadata(cast<Instruction>(newVal), WFV::WFV_METADATA_PACK_UNPACK);

	std::vector<Constant*> maskElems;
	for (unsigned i=0, e=info.mVectorizationFactor; i<e; ++i)
	{
		maskElems.push_back(info.mConstInt32Zero);
	}

	Value* mask = ConstantVector::get(ArrayRef<Constant*>(maskElems));
	newVal = builder.CreateShuffleVector(newVal,
			undefSIMDVal,
			mask);
	WFV::setMetadata(cast<Instruction>(newVal), WFV::WFV_METADATA_PACK_UNPACK);

	if (isConsecutive) {
		auto * scalarType = scalarValue->getType();
		std::vector<Constant*> consOffsetVecElems;
		for(unsigned i = 0; i < info.mVectorizationFactor; ++i) {
		consOffsetVecElems.push_back(CreateConstant<ScalarType>((ScalarType) i, scalarType));
		}
		auto * constOffsetVec = ConstantVector::get(consOffsetVecElems);
		newVal = CreateAdd<ScalarType>(newVal, constOffsetVec, builder);
		WFV::setMetadata(cast<Instruction>(newVal), WFV::WFV_METADATA_PACK_UNPACK);
	}

	return newVal;
}

// NOTE: This function creates W insert-element operations instead of an
//       insert and a shuffle. LLVM is able to optimize both to the same
//       code (pshufd for SSE).
Value*
FunctionVectorizer::broadcastValue(Value*         oldVal,
                                   Instruction*   insertBefore,
                                   const WFVInfo& info) const
{
    assert (oldVal && insertBefore);
    assert (!isa<BasicBlock>(oldVal) && !isa<Function>(oldVal));

    if(mInfo->mVerbose) outs() << "      broadcasting value: " << *oldVal << "\n";

    Type* oldType = oldVal->getType();

    assert (oldType != Type::getVoidTy(*info.mContext) &&
            "must never call broadcastValue() on values of type void!");
    assert (oldType != Type::getLabelTy(*info.mContext) &&
            "must never call broadcastValue() on values of type label!");

    Type* newType = WFV::vectorizeSIMDType(oldType, info.mVectorizationFactor);

    if (Constant* c = dyn_cast<Constant>(oldVal))
    {
        return createVectorConstant(c, info);
    }

    assert (!WFV::hasMetadata(oldVal, WFV::WFV_METADATA_RES_SCALARS) &&
            "must never broadcast values that are RES_SCALARS!");

    // TODO: HERE! Check if we can always use these assertions.
    if (!mInfo->mDisableAllAnalyses)
    {
        assert ((!WFV::hasMetadata(oldVal, WFV::WFV_METADATA_RES_VECTOR) ||
                WFV::hasMetadata(oldVal, WFV::WFV_METADATA_INDEX_CONSECUTIVE)) &&
                "must never broadcast RES_VECTOR value unless INDEX_CONSECUTIVE!");

        assert (!WFV::hasMetadata(oldVal, WFV::WFV_METADATA_INDEX_RANDOM) &&
                !WFV::hasMetadata(oldVal, WFV::WFV_METADATA_INDEX_SHUFFLE) &&
                !WFV::hasMetadata(oldVal, WFV::WFV_METADATA_INDEX_STRIDED) &&
                "must never broadcast values that are not INDEX_SAME or INDEX_CONSECUTIVE!");
    }

    const bool isConsecutive =
        WFV::hasMetadata(oldVal, WFV::WFV_METADATA_INDEX_CONSECUTIVE);

    Type::TypeID oldTypeID = oldType->getTypeID();
    switch (oldTypeID)
    {
        case Type::HalfTyID:
        case Type::FloatTyID:
        case Type::DoubleTyID:
        case Type::X86_FP80TyID:
        case Type::FP128TyID:
        case Type::PPC_FP128TyID:
        {
            Value* newVal;

            IRBuilder<> builder(insertBefore);
            newVal = CreateScalarBroadcast<float>(oldVal, newType, isConsecutive, builder, info);

            if(info.mVerbose) outs() << "      new value: " << *newVal << "\n";
            return newVal;
        }

        case Type::IntegerTyID:
        {
            Value* newVal;

            // If oldVal is a boolean, we have to generate a vector with elements -1 or 0.
            if (oldVal->getType()->isIntegerTy(1))
            {
                assert (!isConsecutive &&
                        "boolean value must not be consecutive!");

                newVal = SelectInst::Create(oldVal,
                                            Constant::getAllOnesValue(newType),
                                            Constant::getNullValue(newType),
                                            "",
                                            insertBefore);
                WFV::setMetadata(cast<Instruction>(newVal), WFV::WFV_METADATA_PACK_UNPACK);
            }
            else
            {

				IRBuilder<> builder(insertBefore);
				newVal = CreateScalarBroadcast<int>(oldVal, newType, isConsecutive, builder, info);

            }
            if(mInfo->mVerbose) outs() << "      new value: " << *newVal << "\n";
            return newVal;
        }

        case Type::ArrayTyID:
        {
            // Create new array of vector-type 'newType' and same size.
            ArrayType* aType = cast<ArrayType>(oldType);

            // Broadcast each element of the array.
            Value* result = UndefValue::get(newType); // The new 'element'-array.
            for (unsigned i=0, ie=aType->getNumElements(); i<ie; ++i)
            {
                Instruction* scalarElem = ExtractValueInst::Create(oldVal,
                                                                   ArrayRef<unsigned>(i),
                                                                   "",
                                                                   insertBefore);
                WFV::setMetadata(cast<Instruction>(scalarElem), WFV::WFV_METADATA_PACK_UNPACK);

                // recurse in order to broadcast sub-element
                Value* pktElem = broadcastValue(scalarElem, insertBefore, info);

                if(!pktElem)
                    return nullptr;

                result = InsertValueInst::Create(result,
                                                 pktElem,
                                                 ArrayRef<unsigned>(i),
                                                 "",
                                                 insertBefore);
                WFV::setMetadata(cast<Instruction>(result), WFV::WFV_METADATA_PACK_UNPACK);
            }

            if(mInfo->mVerbose) outs() << "    new array: " << *result << "\n";
            assert (result->getType() == newType);
            assert (isa<Instruction>(result));

            return cast<Instruction>(result);
        }

        case Type::StructTyID:
        {
            // Create new struct of vector-type 'newType'.
            StructType* sType = cast<StructType>(oldType);

            // Broadcast each element of the struct.
            Value* result = UndefValue::get(newType);
            for (unsigned i=0; i<sType->getNumElements(); ++i)
            {
                Value* scalarElem = ExtractValueInst::Create(oldVal,
                                                             ArrayRef<unsigned>(i),
                                                             "",
                                                             insertBefore);
                WFV::setMetadata(cast<Instruction>(scalarElem), WFV::WFV_METADATA_PACK_UNPACK);

                // Recurse in order to broadcast sub-element.
                Value* pktElem = broadcastValue(scalarElem, insertBefore, info);

                if(!pktElem)
                    return nullptr;

                result = InsertValueInst::Create(result,
                                                 pktElem,
                                                 ArrayRef<unsigned>(i),
                                                 "",
                                                 insertBefore);
                WFV::setMetadata(cast<Instruction>(result), WFV::WFV_METADATA_PACK_UNPACK);
            }

            if(mInfo->mVerbose) outs() << "    new struct: " << *result << "\n";
            assert (result->getType() == newType);
            assert (isa<Instruction>(result));

            return cast<Instruction>(result);
        }

        case Type::PointerTyID:
        {
            // Create new pointer of vector-type 'newType'.
            // This means we have to allocate a new object of type 'newType',
            // load the value from the supplied pointer ('oldVal'),
            // broadcast that value (=recurse),
            // store the broadcasted value into the newly allocated object
            // and finally return the new pointer (the alloca).
            LoadInst* loadedScalarVal = new LoadInst(oldVal,
                                                     "",
                                                     false,
                                                     info.mAlignmentScalar,
                                                     insertBefore);
            WFV::setMetadata(loadedScalarVal, WFV::WFV_METADATA_PACK_UNPACK);

            Value* newVal = broadcastValue(loadedScalarVal, insertBefore, info);

            if(!newVal)
                return nullptr;

            AllocaInst* newPtr = new AllocaInst(newType->getContainedType(0), 0,
                                                info.mConstInt32One,
                                                info.mAlignmentSIMD,
                                                "",
                                                insertBefore);
            WFV::setMetadata(newPtr, WFV::WFV_METADATA_PACK_UNPACK);

            StoreInst* newStore = new StoreInst(newVal,
                                                newPtr,
                                                false,
                                                info.mAlignmentSIMD,
                                                insertBefore);
            WFV::setMetadata(newStore, WFV::WFV_METADATA_PACK_UNPACK);

            if(mInfo->mVerbose) outs() << "    new pointer: " << *newPtr << "\n";
            assert (newPtr->getType() == newType);

            return newPtr;
        }

        default :
        {
            errs() << "\nERROR: only values of type float, int, array, "
                    << "struct, and pointer can be broadcasted, not '"
                    << *oldType << "'!\n";
            assert (false &&
                    "broadcastValue() can only handle float, int, array, struct, and pointer!");
            return nullptr;
        }
    }
}

bool
FunctionVectorizer::broadcastUniformCallOperands(CallInst*      call,
                                                 const WFVInfo& info) const
{
    assert (call);

    if(mInfo->mVerbose) outs() << "    checking arguments of call: " << *call << "\n";

    Function* callee = call->getCalledFunction();
    assert (callee && "call with unknown callee should be OP_SEQUENTIAL!");

    // Map arguments to supplied values and match their types.
    // Set iterator to first supplied argument.
    CallInst::op_iterator OP = call->op_begin();
    for (Function::arg_iterator A=callee->arg_begin(), AE=callee->arg_end(); A!=AE; ++A, ++OP)
    {
        Value* opV = cast<Value>(OP);

        if(mInfo->mVerbose) {
            outs() << "      testing operand: " << *opV << "\n";
            outs() << "      desired argument: " << *A << "\n";
        }
        assert (WFV::isVectorizedType(*A->getType()) ||
                (!WFV::isVectorizedType(*A->getType()) &&
                !WFV::isVectorizedType(*opV->getType())));

        if (WFV::isVectorizedType(*A->getType()) &&
            !WFV::isVectorizedType(*opV->getType()))
        {
            if(mInfo->mVerbose) outs() << "      is scalar, replicating...\n";

            // Broadcast operand and insert before call.
            Value* newOp = broadcastValue(opV, call, info);
            if(!newOp)
                return false;

            // TODO: This can lead to wrong code if the same value is
            //       used for multiple arguments!!
            //       Use call->setOperand() or so...
            call->replaceUsesOfWith(opV, newOp);
        }
    }
    return true;
}

bool
FunctionVectorizer::broadcastUniformStoreOperands(StoreInst*     store,
                                                  const WFVInfo& info) const
{
    assert (store);

    // If pointer type is a vector, the stored data has to be, too.
    Value* data = store->getValueOperand();

    const bool pointerIsVector = WFV::isVectorizedType(*store->getPointerOperand()->getType());
    const bool dataIsVector    = WFV::isVectorizedType(*data->getType());

    if (pointerIsVector && dataIsVector) return true;
    if (!pointerIsVector && !dataIsVector) return true;

    if (dataIsVector)
    {
        errs() << "    store attempts to write vector to scalar pointer: " << *store << "\n";
        assert (false && "store must not attempt to write vector to scalar pointer!");
        return false;
    }

    // Only pointer is vector -> broadcast data.
    if(mInfo->mVerbose) outs() << "    found scalar data in vector store: " << *store << "\n";

    Value* newOp = broadcastValue(data, store, info);
    if(!newOp)
        return false;
    store->replaceUsesOfWith(data, newOp);
    return true;
}


Constant *
FunctionVectorizer::getTrivialConstant(Type * ty, TrivialConstant trivConst) const {
	StructType * sType = dyn_cast<StructType>(ty);
	if (sType) {
		std::vector<Constant*> elemVec;

		for (unsigned i = 0; i < sType->getNumContainedTypes(); ++i) {
			elemVec.push_back(getTrivialConstant(sType->getElementType(i), trivConst));
		}

		return ConstantStruct::get(sType, ArrayRef<Constant*>(elemVec));
	}

	switch (trivConst) {
	case ALL_ZERO: return Constant::getNullValue(ty);
	case ALL_ONE: return Constant::getAllOnesValue(ty);
	case ALL_UNDEF: return UndefValue::get(ty);
	default: break;
	}

	assert(false && "not implemented");
	return nullptr;
}

Constant*
FunctionVectorizer::createVectorConstant(Constant*      oldC,
                                         const WFVInfo& info) const
{
    assert (oldC);
    Type * simdType = WFV::vectorizeSIMDType(oldC->getType(), info.mVectorizationFactor);

   	TrivialConstant trivC;
   	bool isTrivial = false;
   	if (isa<UndefValue>(oldC)) { trivC = ALL_UNDEF; isTrivial = true; }
   	if (oldC->isNullValue()) { trivC = ALL_ZERO; isTrivial = true; }
   	if (oldC->isAllOnesValue()) { trivC = ALL_ONE; isTrivial = true; }

   	if (isTrivial) {
   		return getTrivialConstant(simdType, trivC);
    }

    Constant* c = NULL;
    std::vector<Constant* > vecvec;
    for (unsigned i=0, e=info.mVectorizationFactor; i<e; ++i)
    {
        vecvec.push_back(oldC);
    }

    Type::TypeID oldTypeID = oldC->getType()->getTypeID();
    switch (oldTypeID)
    {
        case Type::HalfTyID :
        case Type::FloatTyID :
        case Type::DoubleTyID :
        case Type::X86_FP80TyID :
        case Type::FP128TyID :
        case Type::PPC_FP128TyID :
        {
            assert (isa<VectorType>(WFV::vectorizeSIMDType(oldC->getType(),
                                                           info.mVectorizationFactor)));
            c = ConstantVector::get(ArrayRef<Constant*>(vecvec));
            break;
        }

        case Type::IntegerTyID :
        {
            assert (isa<VectorType>(WFV::vectorizeSIMDType(oldC->getType(),
                                                           info.mVectorizationFactor)));
            c = ConstantVector::get(ArrayRef<Constant*>(vecvec));
            break;
        }

        case Type::ArrayTyID :
        {
            ConstantArray* oldArrC = cast<ConstantArray>(oldC);
            ArrayType* arrType = oldArrC->getType();

            assert (WFV::vectorizeSIMDType(arrType, info.mVectorizationFactor)->isArrayTy());

            ArrayType* newArrType = cast<ArrayType>(WFV::vectorizeSIMDType(arrType,
                                                                           info.mVectorizationFactor));

            assert (WFV::vectorizeSIMDType(arrType->getElementType(),
                                           info.mVectorizationFactor) ==
                    newArrType->getElementType());

            const unsigned numVals = newArrType->getNumElements();

            // Recursively create vector constants.
            Constant** vals = new Constant*[numVals]();
            for (unsigned i=0; i<numVals; ++i)
            {
                vals[i] = createVectorConstant(oldArrC->getOperand(i), info);
                assert (vals[i]->getType() == newArrType->getElementType() &&
                        "types of array elements have to match");
            }
            c = ConstantArray::get(newArrType, ArrayRef<Constant*>(vals, numVals));

            delete [] vals;
            break;
        }

        case Type::StructTyID :
        {
        	// FIXME widely untested
        	std::vector<Constant*> simdElements;
        	assert(oldC->getNumOperands() == simdType->getNumContainedTypes());
        	for (unsigned i = 0; i < simdType->getNumContainedTypes(); ++i) {
				Constant * elemC = cast<Constant>(oldC->getOperand(i));
				Constant * simdElemC = createVectorConstant(elemC, info);

        		simdElements.push_back(simdElemC);
        	}

        	return ConstantStruct::get(cast<StructType>(simdType), ArrayRef<Constant*>(simdElements));

        }

        default : errs() << "ERROR: can only vectorize constants of types \
            float, int and arrays!\n";
    }

    if (c)
    {
        if(mInfo->mVerbose) outs() << "created new vector constant: " << *c << "\n";
    }
    else
    {
        errs() << "ERROR: could not vectorize constant: " << *oldC << "\n";
        return nullptr;
    }

    return c;
}

bool
FunctionVectorizer::broadcastUniformOperand(Instruction*   inst,
                                            Value*         operand,
                                            const WFVInfo& info) const
{
    assert (inst && operand);

    if (operand->getType()->isLabelTy()) return true;
    if (isa<Function>(operand)) return true;
    if (isa<BasicBlock>(operand)) return true;

    if (WFV::isVectorizedType(*operand->getType())) return true;

    // Extract/Insert element have a scalar index operand.
    if (isa<ExtractElementInst>(inst) &&
        cast<ExtractElementInst>(inst)->getIndexOperand() == operand)
    {
        return true;
    }
    if (isa<InsertElementInst>(inst) &&
        cast<InsertElementInst>(inst)->getOperand(2) == operand)
    {
        return true;
    }

    // Alloca operations have a scalar size operand.
    if (isa<AllocaInst>(inst) &&
        cast<AllocaInst>(inst)->getArraySize() == operand)
    {
        assert (((!isa<Instruction>(operand) && !isa<Argument>(operand)) ||
                !WFV::hasMetadata(operand, WFV::WFV_METADATA_INDEX_CONSECUTIVE)) &&
                "unexpected consecutive alloca array size operand");
        return true;
    }

    // Selects may have scalar conditions.
    if (isa<SelectInst>(inst) &&
        (operand == cast<SelectInst>(inst)->getCondition()))
    {
        assert (((!isa<Instruction>(operand) && !isa<Argument>(operand)) ||
                !WFV::hasMetadata(operand, WFV::WFV_METADATA_INDEX_CONSECUTIVE)) &&
                "unexpected consecutive select condition");
        return true;
    }

    if (Constant* c = dyn_cast<Constant>(operand))
    {
        Constant* newC = createVectorConstant(c, info);
        if(!newC)
            return false;
        inst->replaceUsesOfWith(operand, newC);
        return true;
    }

    assert (isa<Instruction>(operand) || isa<Argument>(operand));

    if (!mInfo->mDisableAllAnalyses)
    {
        // If the operand does not return a RES_UNIFORM value, don't broadcast it.
        // Exception: INDEX_CONSECUTIVE values are scalars with implicit information.
        //            If they are used in a vector operation, they have to be broadcast.
        const bool isConsec = (isa<Instruction>(operand) || isa<Argument>(operand)) &&
            WFV::hasMetadata(operand, WFV::WFV_METADATA_INDEX_CONSECUTIVE);
        if (!isConsec && !WFV::hasMetadata(operand, WFV::WFV_METADATA_RES_UNIFORM))
        {
            return true;
        }
    }

    // Operand is not vectorized -> broadcast.
    if(mInfo->mVerbose) outs() << "    found scalar operand in instruction: " << *inst << "\n";

    // If this is a phi, we must not insert the new instructions
    // before it, but at the end of the incoming block.
    Instruction* insertBefore = inst;
    if (PHINode* phi = dyn_cast<PHINode>(inst))
    {
        for (unsigned i=0, e=phi->getNumIncomingValues(); i!=e; ++i)
        {
            Value* incVal = phi->getIncomingValue(i);
            if (operand != incVal) continue;

            BasicBlock* incBB = phi->getIncomingBlock(i);
            insertBefore = incBB->getTerminator();
            break;
        }
        assert (insertBefore != inst);
    }

    Value* newOp = broadcastValue(operand, insertBefore, info);
    if(!newOp)
        return false;
    inst->replaceUsesOfWith(operand, newOp);
    return true;
}


// Check OP_VARYING instructions for uniform operands that require
// broadcasting.
bool
FunctionVectorizer::broadcastUniformOperands(Function* f)
{
    assert (f);

    for (inst_iterator I=inst_begin(f), E=inst_end(f); I!=E; )
    {
        Instruction* inst = &*I++;

        // Ignore special WFV function calls.
        assert (!isEntryMaskUseFnCall(inst));
        if (isPackWFunctionCall(inst)) continue;
        if (isUnpackWFunctionCall(inst)) continue;
        if (WFV::isMetadataCall(inst)) continue;

        // Ignore all instructions that belong to pack/unpack operations.
        if (WFV::hasMetadata(inst, WFV::WFV_METADATA_PACK_UNPACK)) continue;

        // Ignore functions that are sequential/sequential_guarded.
        if (WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
            WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
        {
            continue;
        }

        // Ignore operations that are OP_UNIFORM except if they are also RES_VECTOR.
        if (WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_UNIFORM) &&
            !WFV::hasMetadata(inst, WFV::WFV_METADATA_RES_VECTOR))
        {
            continue;
        }

        // Ignore INDEX_CONSECUTIVE instructions (only *operands* that are
        // INDEX_CONSECUTIVE are broadcast).
        if (WFV::hasMetadata(inst, WFV::WFV_METADATA_INDEX_CONSECUTIVE))
        {
            continue;
        }

        // Ignore casts introduced during previous phases.
        // TODO: Factor out all these tests for special instructions
        //       into a common function, it is required in multiple places.
        if (WFV::hasMetadata(inst, WFV::WFV_METADATA_ARGUMENT_CAST) ||
            WFV::hasMetadata(inst, WFV::WFV_METADATA_PKT_PTR_CAST))
        {
            continue;
        }

        // Ignore insert element operations in which the type of the second
        // operand matches the return type.
        if (isa<InsertElementInst>(inst))
        {
            Type* vecType  = inst->getType();
            Type* elemType = inst->getOperand(1)->getType();

            if (WFV::typesMatch(vecType,
                                WFV::vectorizeSIMDType(elemType,
                                                       mInfo->mVectorizationFactor)))
            {
                continue;
            }
        }

        // Ignore extract element operations in which the type of the
        // operands matches the return type.
        if (isa<ExtractElementInst>(inst))
        {
            Type* vecType  = inst->getType();
            Type* elemType = inst->getOperand(1)->getType();

            if (WFV::typesMatch(vecType,
                                WFV::vectorizeSIMDType(elemType,
                                                       mInfo->mVectorizationFactor)))
            {
                continue;
            }
        }

        // Operands of GEPs are never broadcast.
        if (isa<GetElementPtrInst>(inst))
        {
            continue;
        }

        if(mInfo->mVerbose) outs() << "broadcastUniformOperands(" << *inst << " )\n";

        // We have to handle calls separately because their operands
        // have to be matched against their signature.
        if (CallInst* call = dyn_cast<CallInst>(inst))
        {
            if(!broadcastUniformCallOperands(call, *mInfo))
                return false;
            continue;
        }

        if (StoreInst* store = dyn_cast<StoreInst>(inst))
        {
            if(!broadcastUniformStoreOperands(store, *mInfo))
                return false;
            continue;
        }

        for (Instruction::op_iterator OP=inst->op_begin(),
                OPE=inst->op_end(); OP!=OPE; ++OP)
        {
            Value* opV = cast<Value>(OP);
            if(!broadcastUniformOperand(inst, opV, *mInfo))
                return false;
        }
    }
    return true;
}
