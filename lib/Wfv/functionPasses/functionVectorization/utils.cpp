/**
 * @file   utils.cpp
 * @date   05.06.2012
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

#include "llvm/IR/Intrinsics.h"

#include <sstream>
#include <stdexcept>

using namespace llvm;

std::string
FunctionVectorizer::getTypeString(Type* type)
{
    switch (type->getTypeID())
    {
        case Type::VoidTyID: return "void";
        case Type::HalfTyID: return "f16";
        case Type::FloatTyID: return "f32";
        case Type::DoubleTyID: return "f64";
        case Type::X86_FP80TyID: return "f80";
        case Type::FP128TyID: return "f128";
        case Type::PPC_FP128TyID: return "f128ppc";
        case Type::X86_MMXTyID: return "v2i";
        case Type::IntegerTyID:
        {
            std::stringstream sstr;
            sstr << "i" << type->getIntegerBitWidth();
            return sstr.str();
        }
        case Type::StructTyID:
        {
            std::stringstream sstr;
            StructType* st = cast<StructType>(type);
            sstr << "s_";
            for (StructType::subtype_iterator ST=st->subtype_begin(),
                    STE=st->subtype_end(); ST!=STE; ++ST)
            {
                sstr << getTypeString(*ST);
                if (ST+1 != STE) sstr << ".";
            }
            sstr << "_";
            return sstr.str();
        }
        case Type::ArrayTyID:
        {
            std::stringstream sstr;
            ArrayType* at = cast<ArrayType>(type);
            sstr << "a" << at->getNumElements() << getTypeString(at->getElementType());
            return sstr.str();
        }
        case Type::PointerTyID:
        {
            std::stringstream sstr;
            sstr << "p" << type->getPointerAddressSpace()
                << "_" << getTypeString(type->getPointerElementType()) << "_";
            return sstr.str();
        }
        case Type::VectorTyID:
        {
            std::stringstream sstr;
            VectorType* vt = cast<VectorType>(type);
            sstr << "v" << vt->getNumElements() << getTypeString(vt->getElementType());
            return sstr.str();
        }
        case Type::FunctionTyID:
        {
            std::stringstream sstr;
            FunctionType* ft = cast<FunctionType>(type);
            sstr << "fn_" << getTypeString(ft->getReturnType()) << "_";
            for (unsigned i=0, e=ft->getNumParams(); i<e; ++i)
            {
                sstr << getTypeString(ft->getParamType(i));
                if (i+1 != e) sstr << ".";
            }
            sstr << "_";
            return sstr.str();
        }
        default:
        {
            errs() << "ERROR: Can't handle type: " << *type << "\n";
            return "";
        }
    }
}

std::string
FunctionVectorizer::getMangledFunctionName(const char*     name,
                                           Type*           returnType,
                                           ArrayRef<Type*> paramTypes)
{
    std::stringstream sstr;
    const std::string& rts = getTypeString(returnType);
    sstr << name << "$$" << rts << "$" << paramTypes.size();
    for (unsigned i=0, e=paramTypes.size(); i<e; ++i)
    {
        const std::string& pts = getTypeString(paramTypes[i]);
        sstr << "$" << pts;
    }
    return sstr.str();
}

Function*
FunctionVectorizer::getSpecialWFVFunction(const char*     name,
                                          Type*           returnType,
                                          ArrayRef<Type*> paramTypes,
                                          Module*         mod)
{
    const std::string& fnName = getMangledFunctionName(name, returnType, paramTypes);

    // FIXME hacky way of overloading special WFV functions to merge structural equivalent types..
    std::stringstream ss; ss << name;
    while (Function* fn = mod->getFunction(ss.str())) {
        bool matches =  (fn->isDeclaration() &&
                fn->getFunctionType()->getReturnType() == returnType &&
                fn->getFunctionType()->getFunctionNumParams() == paramTypes.size() &&
                fn->doesNotAccessMemory() &&
                fn->doesNotThrow() &&
                "incompatible function already exists in module!");
        for (unsigned i=0, e=paramTypes.size(); i<e; ++i)
        {
            matches &= (fn->getFunctionType()->getParamType(i) == paramTypes[i] &&
                    "incompatible function already exists in module!");
        }

        if (matches) {
        	return fn;
        }

        ss << "_"; // padding..
    }

    FunctionType* fTy = FunctionType::get(returnType, paramTypes, false);
    Function* fn = Function::Create(fTy, Function::ExternalLinkage, ss.str(), mod);
    fn->setDoesNotAccessMemory();
    fn->setDoesNotThrow();

    return fn;
}

bool
FunctionVectorizer::isPackFunctionCall(const Instruction* inst)
{
    assert (inst);
    if (!isa<CallInst>(inst)) return false;

    const Function* f = cast<CallInst>(inst)->getCalledFunction();

    return f->getName().startswith(WFV_FUNCTION_NAME_PACK);
}


bool
FunctionVectorizer::isUnpackFunctionCall(const Instruction* inst)
{
    assert (inst);
    if (!isa<CallInst>(inst)) return false;

    const Function* f = cast<CallInst>(inst)->getCalledFunction();

    return f->getName().startswith(WFV_FUNCTION_NAME_UNPACK);
}


bool
FunctionVectorizer::isForwardFunctionCall(const Instruction* inst)
{
    assert (inst);
    if (!isa<CallInst>(inst)) return false;

    const Function* f = cast<CallInst>(inst)->getCalledFunction();

    return f->getName().startswith(WFV_FUNCTION_NAME_FORWARD);
}

bool
FunctionVectorizer::isPackWFunctionCall(const Instruction* inst)
{
    assert (inst);
    if (!isa<CallInst>(inst)) return false;

    const Function* f = cast<CallInst>(inst)->getCalledFunction();

    return f->getName().startswith(WFV_FUNCTION_NAME_PACK_W);
}

bool
FunctionVectorizer::isUnpackWFunctionCall(const Instruction* inst)
{
    assert (inst);
    if (!isa<CallInst>(inst)) return false;

    const Function* f = cast<CallInst>(inst)->getCalledFunction();

    return f->getName().startswith(WFV_FUNCTION_NAME_UNPACK_W);
}

bool
FunctionVectorizer::isEntryMaskUseFnCall(const Instruction* inst)
{
    assert (inst);
    if (!isa<CallInst>(inst)) return false;

    const Function* f = cast<CallInst>(inst)->getCalledFunction();

    return f->getName().startswith("entryMaskUseFn");
}

Instruction*
FunctionVectorizer::generateHorizontalPointerExtract(Value*         V,
                                                     Value*         indexVal,
                                                     Type*          elementType,
                                                     StringRef      name,
                                                     Instruction*   allocPos,
                                                     Instruction*   insertBefore,
                                                     const WFVInfo& info)
{
    assert (V && indexVal && elementType);
    assert (V->getType()->isPointerTy());
    assert (elementType->isPointerTy());

    // In general, we have to allocate space of the elementType on the stack,
    // store appropriate values to its fields, and return that pointer.
    // However, in case of certain "simple" cases (pointer to int/float), we can
    // directly return a pointer to the sub-element via a GEP.
    if (elementType->isPointerTy() &&
        (elementType->getPointerElementType()->isFloatingPointTy() ||
         elementType->getPointerElementType()->isIntegerTy() ||
         elementType->getPointerElementType()->isPointerTy()))
    {
        SmallVector<Value*, 2> indices;
        indices.push_back(info.mConstInt32Zero);
        indices.push_back(indexVal);
        GetElementPtrInst* newPtr = GetElementPtrInst::Create(nullptr,
                                                              V,
                                                              indices,
                                                              "",
                                                              insertBefore);
        WFV::setMetadata(newPtr, WFV::WFV_METADATA_PACK_UNPACK);

        if(info.mVerbose) outs() << "    extracted element (new pointer): " << *newPtr << "\n";
        assert (newPtr->getType() == elementType);

        return newPtr;
    }

    // Create new pointer of "sub"-type 'elementType'.
    // This means we have to allocate a new object of type 'elementType',
    // load the value from the supplied pointer ('V'),
    // extract from each element of that value the ith sub-element (=recurse),
    // store the extracted value into the newly allocated object
    // and finally return the new pointer (the alloca).
    LoadInst* loadedVal = new LoadInst(V,
                                       name,
                                       false,
                                       info.mAlignmentSIMD,
                                       insertBefore);
    WFV::setMetadata(loadedVal, WFV::WFV_METADATA_PACK_UNPACK);

    Value* newVal = generateHorizontalExtract(loadedVal,
                                              indexVal,
                                              name,
                                              allocPos,
                                              insertBefore,
                                              info);
    if(!newVal)
        return nullptr;

    AllocaInst* newPtr = new AllocaInst(elementType->getContainedType(0), 0,
                                        info.mConstInt32One,
                                        info.mAlignmentScalar,
                                        name,
                                        allocPos);
    WFV::setMetadata(newPtr, WFV::WFV_METADATA_PACK_UNPACK);

    StoreInst* store = new StoreInst(newVal,
                                     newPtr,
                                     false,
                                     info.mAlignmentScalar,
                                     insertBefore);
    WFV::setMetadata(store, WFV::WFV_METADATA_PACK_UNPACK);

    if(mInfo->mVerbose) outs() << "    extracted element (new pointer): " << *newPtr << "\n";
    assert (newPtr->getType() == elementType);

    return newPtr;
}

// Generates code that extracts the ith sub-element from a vectorized value.
// This means that the generated value holds only the values of the
// ith scalar instance of that type.
// Thus, the code does not extract e.g. the ith value of a struct
// but generates a new struct where each element is the ith scalar
// element of the original vector value.
// For consecutive non-pointer values (int/float), no extraction is performed
// but the scalar value is simply incremented by indexVal.
Value*
FunctionVectorizer::generateHorizontalExtract(Value*         V,
                                              Value*         indexVal,
                                              StringRef      name,
                                              Instruction*   allocPos,
                                              Instruction*   insertBefore,
                                              const WFVInfo& info)
{
    assert (V && indexVal);
    if(info.mVerbose) {
        outs() << "  extracting horizontal elements at index "
               << *indexVal << " of value: " << *V << "\n";
    }

    Type* sourceType = V->getType();
    assert (sourceType);
    if(mInfo->mVerbose) outs() << "    source type: " << *sourceType << "\n";

    // Handle consecutive non-pointer values.
    if (!WFV::isVectorizedType(*sourceType))
    {
        if (!WFV::hasMetadata(V, WFV::WFV_METADATA_INDEX_CONSECUTIVE))
        {
            return V;
        }

        assert (isa<Instruction>(V) || isa<Argument>(V));
        assert (WFV::hasMetadata(V, WFV::WFV_METADATA_INDEX_CONSECUTIVE));
        assert (!sourceType->isPointerTy());
        assert (sourceType->isIntegerTy() || sourceType->isFloatingPointTy());

        Type* idxType = indexVal->getType();
        if (sourceType != idxType)
        {
            // Cast indexVal to the appropriate type.
            assert (CastInst::isCastable(idxType, sourceType));
            const bool sourceTypeIsSigned = true; // TODO: How can we derive that?
            Instruction::CastOps castOp = CastInst::getCastOpcode(indexVal,
                                                                  false,
                                                                  sourceType,
                                                                  sourceTypeIsSigned);

            Instruction* castInst = CastInst::Create(castOp,
                                                     indexVal,
                                                     sourceType,
                                                     "",
                                                     insertBefore);
            WFV::setMetadata(castInst, WFV::WFV_METADATA_OP_UNIFORM);
            WFV::setMetadata(castInst, WFV::WFV_METADATA_RES_UNIFORM);
            WFV::setMetadata(castInst, WFV::WFV_METADATA_INDEX_SAME);
            WFV::setMetadata(castInst, indexVal == info.mConstInt32Zero ?
                WFV::WFV_METADATA_ALIGNED_TRUE : WFV::WFV_METADATA_ALIGNED_FALSE);

            indexVal = castInst;
        }

        // Determine type of increment operation.
        Instruction::BinaryOps bop = sourceType->isIntegerTy() ?
            Instruction::Add : Instruction::FAdd;

        Instruction* inc = BinaryOperator::Create(bop, V, indexVal, "", insertBefore);

        WFV::setMetadata(inc, WFV::WFV_METADATA_OP_UNIFORM);
        WFV::setMetadata(inc, WFV::WFV_METADATA_RES_UNIFORM);
        WFV::setMetadata(inc, WFV::WFV_METADATA_INDEX_SAME);
        WFV::setMetadata(inc, indexVal == info.mConstInt32Zero ?
            WFV::WFV_METADATA_ALIGNED_TRUE : WFV::WFV_METADATA_ALIGNED_FALSE);

        return inc;
    }

    Type* elementType = WFV::getScalarFromVectorizedType(sourceType);
    assert (elementType);
    if(mInfo->mVerbose) outs() << "    element type: " << *elementType << "\n";

    switch (sourceType->getTypeID())
    {
        case Type::VectorTyID:
        {
            // simply extract the element at index 'index'
            ExtractElementInst* eeInst = ExtractElementInst::Create(V,
                                                                    indexVal,
                                                                    name,
                                                                    insertBefore);
            WFV::setMetadata(eeInst, WFV::WFV_METADATA_PACK_UNPACK);

            if(mInfo->mVerbose) outs() << "    extracted element (new vector): " << *eeInst << "\n";
            assert (!WFV::isVectorizedType(*eeInst->getType()));
            assert (eeInst->getType() == elementType);

            return eeInst;
        }
        case Type::ArrayTyID:
        {
            // create new array of type 'elementType' and same size
            ArrayType* aType = cast<ArrayType>(sourceType);

            //for each element of the array, get the ith subelement
            Value* result = UndefValue::get(elementType); // the new 'element'-array
            for (unsigned j=0, je=aType->getNumElements(); j<je; ++j)
            {
                Instruction* pktElem = ExtractValueInst::Create(V,
                                                                ArrayRef<unsigned>(j),
                                                                name,
                                                                insertBefore);
                WFV::setMetadata(pktElem, WFV::WFV_METADATA_PACK_UNPACK);

                // recurse in order to extract correct sub-element
                Value* scalarElem = generateHorizontalExtract(pktElem,
                                                              indexVal,
                                                              name,
                                                              allocPos,
                                                              insertBefore,
                                                              info);

                result = InsertValueInst::Create(result,
                                                 scalarElem,
                                                 ArrayRef<unsigned>(j),
                                                 name,
                                                 insertBefore);
                WFV::setMetadata(cast<Instruction>(result), WFV::WFV_METADATA_PACK_UNPACK);
            }

            if(mInfo->mVerbose) outs() << "    extracted element (new array): " << *result << "\n";
            assert (result->getType() == elementType);
            assert (isa<Instruction>(result));

            return cast<Instruction>(result);
        }
        case Type::StructTyID:
        {
            // create new struct of "sub"-type 'elementType'
            StructType* sType = cast<StructType>(sourceType);

            //for each element of the struct, get the ith subelement
            Value* result = UndefValue::get(elementType);
            for (unsigned j=0; j<sType->getNumElements(); ++j)
            {
                Value* pktElem = ExtractValueInst::Create(V,
                                                          ArrayRef<unsigned>(j),
                                                          name,
                                                          insertBefore);
                WFV::setMetadata(cast<Instruction>(pktElem), WFV::WFV_METADATA_PACK_UNPACK);

                // recurse in order to extract correct sub-element
                Value* scalarElem = generateHorizontalExtract(pktElem,
                                                              indexVal,
                                                              name,
                                                              allocPos,
                                                              insertBefore,
                                                              info);

                result = InsertValueInst::Create(result,
                                                 scalarElem,
                                                 ArrayRef<unsigned>(j),
                                                 name,
                                                 insertBefore);
                WFV::setMetadata(cast<Instruction>(result), WFV::WFV_METADATA_PACK_UNPACK);
            }

            if(mInfo->mVerbose) outs() << "    extracted element (new struct): " << *result << "\n";
            assert (result->getType() == elementType);
            assert (isa<Instruction>(result));

            return cast<Instruction>(result);
        }
        case Type::PointerTyID:
        {
            return generateHorizontalPointerExtract(V,
                                                    indexVal,
                                                    elementType,
                                                    name,
                                                    allocPos,
                                                    insertBefore,
                                                    info);
        }
        default:
        {
            errs() << "ERROR: bad type for element extraction found: " << *sourceType << "\n";
            assert (false && "bad type for element extraction found!");
            return nullptr;
        }
    }
}

// Recursively merges 'splitVals.size()' values together to form a
// value of type 'targetType'.
// This function merges horizontally, meaning it has to reorder
// values of structs and arrays because each scalarVal represents
// the composite value of one instance (instead of all values of one
// element of the structure).
// TODO: Unify generateHorizontalInsert() and generateHorizontalMerge().
Instruction*
FunctionVectorizer::generateHorizontalMerge(SmallVector<Value*, 8>& splitVals,
                                            Type*                   targetType,
                                            StringRef               name,
                                            Instruction*            insertBefore,
                                            const WFVInfo&          info)
{
    assert (!splitVals.empty());
    assert (splitVals[0]);
    assert (targetType);
    if(mInfo->mVerbose) outs() << "  merging values...\n";

    assert (!(targetType->getTypeID() <= Type::X86_MMXTyID) &&
            !targetType->isIntegerTy() &&
            "merging into scalar value does not make sense!");

    const unsigned targetVecFactor = splitVals.size();

    Type* elementType = splitVals[0]->getType();

    if(info.mVerbose) {
        outs() << "    element type: " << *elementType << "\n";
        outs() << "    target type : " << *targetType << "\n";
        outs() << "    values:\n";
        for (unsigned i=0, e=targetVecFactor; i<e; ++i)
        {
            assert (splitVals[i]);
            assert (splitVals[i]->getType() == elementType);
            outs() << "     *" << *splitVals[i] << "\n";
        }
    }

    if (elementType == targetType)
    {
        // Uniform value, no merging necessary.
        Value* result = splitVals[0];
        if(info.mVerbose) outs() << "    uniform value: " << *result << "\n";
        assert (splitVals.size() == 1);

        if (!isa<Instruction>(result))
        {
            result = WFV::createNoOp(result->getType(), insertBefore);
            // TODO: metadata...
        }
        return cast<Instruction>(result);
    }

    assert (!WFV::isVectorizedType(*elementType) &&
            "must not attempt to merge already vectorized values!");
    assert (targetVecFactor > 1);
    assert (targetVecFactor == WFV::getVectorizationFactor(*targetType) &&
            "number of values to be merged does not match vectorization factor of target type!");
    assert (targetType == WFV::vectorizeSIMDType(elementType, targetVecFactor) &&
            "element target type has to be vectorized element type or uniform!");

    Value* result = UndefValue::get(targetType);

    switch (targetType->getTypeID())
    {
        case Type::VectorTyID:
        {
            // Simply merge targetVecFactor scalars into a vector.
            assert (elementType->isFloatingPointTy() || elementType->isIntegerTy());

            for (unsigned i=0, e=targetVecFactor; i<e; ++i)
            {
                result = InsertElementInst::Create(result,
                                                   splitVals[i],
                                                   ConstantInt::get(*info.mContext, APInt(32, i)),
                                                   "",
                                                   insertBefore);
                WFV::setMetadata(cast<Instruction>(result), WFV::WFV_METADATA_PACK_UNPACK);
            }
            break;
        }
        case Type::ArrayTyID:
        {
            // loop over elements of array and call merge on
            // 'targetVecFactor' values of split input arrays
            ArrayType* aType = cast<ArrayType>(targetType);
            assert (elementType->isArrayTy() && "elements have to be arrays!");

            for (unsigned j=0, je=aType->getNumElements(); j<je; ++j)
            {
                SmallVector<Value*, 8> values(targetVecFactor);

                // extract jth value from each of the input arrays
                for (unsigned i=0, e=targetVecFactor; i<e; ++i)
                {
                    ExtractValueInst* evInst =
                        ExtractValueInst::Create(splitVals[i],
                                                 ArrayRef<unsigned>(j),
                                                 "",
                                                 insertBefore);
                    WFV::setMetadata(evInst, WFV::WFV_METADATA_PACK_UNPACK);
                    values[i] = evInst;
                }

                // merge the extracted values
                Instruction* mergedArrayElem = generateHorizontalMerge(values,
                                                                       aType->getElementType(),
                                                                       "",
                                                                       insertBefore,
                                                                       info);
                if(!mergedArrayElem)
                    return nullptr;

                WFV::setMetadata(mergedArrayElem, WFV::WFV_METADATA_PACK_UNPACK);

                // insert the merged element into the new array
                result = InsertValueInst::Create(result,
                                                 mergedArrayElem,
                                                 ArrayRef<unsigned>(j),
                                                 "",
                                                 insertBefore);
                WFV::setMetadata(cast<Instruction>(result), WFV::WFV_METADATA_PACK_UNPACK);
            }
            break;
        }
        case Type::StructTyID:
        {
            // loop over elements of struct and call merge on
            // 'targetVecFactor' values of split input structs
            StructType* sType = cast<StructType>(targetType);
            assert (elementType->isStructTy() && "elements have to be structs!");

            for (unsigned j=0, je=sType->getNumElements(); j<je; ++j)
            {
                SmallVector<Value*, 8> values(targetVecFactor);

                // extract jth value from each of the input structs
                for (unsigned i=0, e=targetVecFactor; i<e; ++i)
                {
                    ExtractValueInst* evInst =
                        ExtractValueInst::Create(splitVals[i],
                                                 ArrayRef<unsigned>(j),
                                                 "",
                                                 insertBefore);
                    WFV::setMetadata(evInst, WFV::WFV_METADATA_PACK_UNPACK);
                    values[i] = evInst;
                }

                // merge the extracted values
                Instruction* mergedStructElem = generateHorizontalMerge(values,
                                                                        sType->getElementType(j),
                                                                        "",
                                                                        insertBefore,
                                                                        info);
                if(!mergedStructElem)
                    return nullptr;

                WFV::setMetadata(mergedStructElem, WFV::WFV_METADATA_PACK_UNPACK);

                // insert the merged element into the new struct
                result = InsertValueInst::Create(result,
                                                 mergedStructElem,
                                                 ArrayRef<unsigned>(j),
                                                 "",
                                                 insertBefore);
                WFV::setMetadata(cast<Instruction>(result), WFV::WFV_METADATA_PACK_UNPACK);
            }
            break;
        }
        case Type::PointerTyID:
        {

            PointerType* pType = cast<PointerType>(targetType);
            assert (elementType->isPointerTy() && "elements have to be pointers!");

            SmallVector<Value*, 8> values(targetVecFactor);
            // load from each of the input pointers
            for (unsigned i=0, e=targetVecFactor; i<e; ++i)
            {
                LoadInst* ldInst = new LoadInst(splitVals[i],
                                                "",
                                                false,
                                                info.mAlignmentSIMD,
                                                insertBefore);
                WFV::setMetadata(ldInst, WFV::WFV_METADATA_PACK_UNPACK);
                values[i] = ldInst;
            }

            // merge the loaded values
            Instruction* mergedStructElem = generateHorizontalMerge(values,
                                                                    pType->getElementType(),
                                                                    "",
                                                                    insertBefore,
                                                                    info);
            if(!mergedStructElem)
                return nullptr;

            WFV::setMetadata(mergedStructElem, WFV::WFV_METADATA_PACK_UNPACK);

            // allocate space for the new type
            result = new AllocaInst(pType->getElementType(), 0,
                                    info.mConstInt32Zero,
                                    info.mAlignmentSIMD,
                                    "",
                                    insertBefore);
            WFV::setMetadata(cast<Instruction>(result), WFV::WFV_METADATA_PACK_UNPACK);

            // store the merged element into the newly allocated space
            StoreInst* store = new StoreInst(mergedStructElem,
                                             result,
                                             false,
                                             info.mAlignmentSIMD,
                                             insertBefore);
            WFV::setMetadata(store, WFV::WFV_METADATA_PACK_UNPACK);

            break;
        }
        default:
        {
            errs() << "ERROR: cannot merge elements into value of type: " << *targetType << "\n";
            assert (false && "bad type for element merging found!");
            return nullptr;
        }
    }

    if(info.mVerbose) outs() << "    merged value: " << *result << "\n";
    assert (result->getType() == targetType);

    result->setName(name);

    assert (isa<Instruction>(result));
    return cast<Instruction>(result);
}

// Inserts 'scalarVal' at vector element index 'index' into 'targetVal'.
// This means it has to reorder values of structs and arrays because
// each scalarVal represents the composite value of one instance (instead
// of all values of one element of the structure).
// TODO: Unify generateHorizontalInsert() and generateHorizontalMerge().
Instruction*
FunctionVectorizer::generateHorizontalInsert(Value*         scalarVal,
                                             Value*         targetVal,
                                             Value*         indexVal,
                                             Instruction*   insertBefore,
                                             const WFVInfo& info)
{
    assert (scalarVal && targetVal && indexVal && insertBefore);
    if(info.mVerbose) {
        outs() << "  inserting scalar element: " << *scalarVal << "\n";
        outs() << "  into target value       : " << *targetVal << "\n";
        outs() << "  at index                : " << *indexVal << "\n";
    }

    Type* targetType  = targetVal->getType(); WFV_UNUSED(targetType);
    Type* elementType = scalarVal->getType();

    if(info.mVerbose) {
        outs() << "    element type: " << *elementType << "\n";
        outs() << "    target type : " << *targetType << "\n";
    }
    assert (!targetType->getPrimitiveSizeInBits() != 0 &&
            !targetType->isIntegerTy() &&
            "inserting into scalar value does not make sense!");
    assert (targetType == WFV::vectorizeSIMDType(elementType,
                                                 WFV::getVectorizationFactor(*targetType)) &&
            "element target type has to be vectorized element type or uniform!");

    Instruction* insertOp = nullptr;

    switch (elementType->getTypeID())
    {
        case Type::HalfTyID:
        case Type::FloatTyID:
        case Type::DoubleTyID:
        case Type::X86_FP80TyID:
        case Type::FP128TyID:
        case Type::PPC_FP128TyID:
        case Type::IntegerTyID:
        {
            assert (targetType->isVectorTy());
            assert (targetType->getVectorElementType() == elementType);

            insertOp = InsertElementInst::Create(targetVal,
                                                 scalarVal,
                                                 indexVal,
                                                 "",
                                                 insertBefore);
            WFV::setMetadata(insertOp, WFV::WFV_METADATA_PACK_UNPACK);

            break;
        }
        case Type::ArrayTyID:
        {
            // For each element of the array, extract the corresponding value from
            // scalarVal and insert it into targetVal.
            assert (targetType->isArrayTy() && "target has to be an array!");

            Value* currentArray = targetVal;
            for (unsigned j=0, je=elementType->getArrayNumElements(); j<je; ++j)
            {
                // Extract jth value from scalarVal.
                Instruction* arrayElemSrc = ExtractValueInst::Create(scalarVal,
                                                                     ArrayRef<unsigned>(j),
                                                                     "",
                                                                     insertBefore);
                WFV::setMetadata(arrayElemSrc, WFV::WFV_METADATA_PACK_UNPACK);

                // Extract jth value from targetVal.
                Instruction* arrayElemTrg = ExtractValueInst::Create(currentArray,
                                                                     ArrayRef<unsigned>(j),
                                                                     "",
                                                                     insertBefore);
                WFV::setMetadata(arrayElemTrg, WFV::WFV_METADATA_PACK_UNPACK);

                // Recursively insert arrayElemSrc into arrayElemTrg.
                Instruction* newElem = generateHorizontalInsert(arrayElemSrc,
                                                                arrayElemTrg,
                                                                indexVal,
                                                                insertBefore,
                                                                info);

                // Insert the modified element back into the array (if necessary,
                // in case of a pointer we skip this).
                if (!arrayElemSrc->getType()->isPointerTy())
                {
                    assert (!isa<StoreInst>(newElem));
                    currentArray = InsertValueInst::Create(currentArray,
                                                           newElem,
                                                           ArrayRef<unsigned>(j),
                                                           "",
                                                           insertBefore);
                    WFV::setMetadata(cast<Instruction>(currentArray),
                                     WFV::WFV_METADATA_PACK_UNPACK);
                }
            }

            assert (isa<Instruction>(currentArray));
            insertOp = cast<Instruction>(currentArray);
            break;
        }
        case Type::StructTyID:
        {
            // For each element of the struct, extract the corresponding value from
            // scalarVal and insert it into targetVal.
            assert (targetType->isStructTy() && "target has to be a struct!");

            Value* currentStruct = targetVal;
            for (unsigned j=0, je=elementType->getStructNumElements(); j<je; ++j)
            {
                // Extract jth value from input struct.
                Instruction* structElemSrc = ExtractValueInst::Create(scalarVal,
                                                                      ArrayRef<unsigned>(j),
                                                                      "",
                                                                      insertBefore);
                WFV::setMetadata(structElemSrc, WFV::WFV_METADATA_PACK_UNPACK);

                // Extract jth value from targetVal.
                Instruction* structElemTrg = ExtractValueInst::Create(currentStruct,
                                                                      ArrayRef<unsigned>(j),
                                                                      "",
                                                                      insertBefore);
                WFV::setMetadata(structElemTrg, WFV::WFV_METADATA_PACK_UNPACK);

                // Recursively insert structElemSrc into structElemTrg.
                Instruction* newElem = generateHorizontalInsert(structElemSrc,
                                                                structElemTrg,
                                                                indexVal,
                                                                insertBefore,
                                                                info);

                // Insert the modified element back into the struct (if necessary,
                // in case of a pointer we skip this).
                if (!structElemSrc->getType()->isPointerTy())
                {
                    assert (!isa<StoreInst>(newElem));
                    currentStruct = InsertValueInst::Create(currentStruct,
                                                            newElem,
                                                            ArrayRef<unsigned>(j),
                                                            "",
                                                            insertBefore);
                    WFV::setMetadata(cast<Instruction>(currentStruct),
                                     WFV::WFV_METADATA_PACK_UNPACK);
                }
            }

            assert (isa<Instruction>(currentStruct));
            insertOp = cast<Instruction>(currentStruct);
            break;
        }
        case Type::PointerTyID:
        {
            assert (targetType->isPointerTy() && "target has to be a pointer!");

            // Load input pointer.
            Instruction* loadSrc = new LoadInst(scalarVal,
                                                "",
                                                false,
                                                info.mAlignmentScalar,
                                                insertBefore);
            WFV::setMetadata(loadSrc, WFV::WFV_METADATA_PACK_UNPACK);

            // Load target pointer.
            Instruction* loadTrg = new LoadInst(targetVal,
                                                "",
                                                false,
                                                info.mAlignmentSIMD,
                                                insertBefore);
            WFV::setMetadata(loadTrg, WFV::WFV_METADATA_PACK_UNPACK);

            // Recursively insert loadSrc into loadTrg.
            Instruction* newVal = generateHorizontalInsert(loadSrc,
                                                           loadTrg,
                                                           indexVal,
                                                           insertBefore,
                                                           info);

            // Write back the modified value to the pointer (if necessary,
            // in case of a pointer we skip this).
            if (!isa<StoreInst>(newVal))
            {
                insertOp = new StoreInst(newVal,
                                         targetVal,
                                         false,
                                         info.mAlignmentSIMD,
                                         insertBefore);
                WFV::setMetadata(insertOp, WFV::WFV_METADATA_PACK_UNPACK);
            }
            else
            {
                assert (isa<Instruction>(targetVal));
                insertOp = cast<Instruction>(targetVal);
            }

            break;
        }
        default:
        {
            errs() << "ERROR: cannot insert element of type: "
                   << *elementType << " into value of type: " << *targetType << "\n";
            assert (false && "bad type for element insertion found!");
            return nullptr;
        }
    }

    if(info.mVerbose) outs() << "    insert operation: " << *insertOp << "\n";
    return insertOp;
}

bool
FunctionVectorizer::generateWriteBackOperations(Instruction*   currentPos,
                                                Instruction*   extractedVal,
                                                Value*         targetVal,
                                                Value*         indexVal,
                                                const WFVInfo& info)
{
    assert (currentPos && extractedVal && targetVal && indexVal);
    assert (isa<AllocaInst>(currentPos) ||
            isa<BitCastInst>(currentPos) ||
            isa<PHINode>(currentPos) ||
            isa<GetElementPtrInst>(currentPos));
    assert (currentPos->getType()->isPointerTy());
    assert (extractedVal->getType()->isPointerTy());
    assert (targetVal->getType()->isPointerTy());

    // Iterate over all uses of currentPos.
    // Recurse through casts, phis, and GEPs.
    // For every store or call we encounter, create write-back operations to
    // targetVal behind it to make sure that the side-effect is not only
    // locally visible due to the alloca required for the extract operation.

    SmallVector<Instruction*, 2> uses;
    for (Instruction::user_iterator U=currentPos->user_begin(),
         UE=currentPos->user_end(); U!=UE; ++U)
    {
        assert (isa<Instruction>(*U));
        Instruction* useI = cast<Instruction>(*U);

        uses.push_back(useI);
    }

    for (auto &useI : uses)
    {
        // Step through bitcasts and phis.
        if (isa<BitCastInst>(useI) || isa<PHINode>(useI))
        {
            generateWriteBackOperations(useI, extractedVal, targetVal, indexVal, info);
            continue;
        }

        // If the use is a GEP, step through and append the GEPs indices.
        // Also, create a matching GEP for the target structure, such that we
        // only generate write back operations for the relevant parts of the
        // structure that were actually changed instead of blindly writing back
        // the entire structure (which may also result in segfaults if the
        // structure contains uninitialized pointers - see test_struct_extra05).
        if (GetElementPtrInst* gep = dyn_cast<GetElementPtrInst>(useI))
        {
            // Create GEP for the target value.
            SmallVector<Value*, 2> newIndexValues(gep->idx_begin(), gep->idx_end());
            GetElementPtrInst* targetGEP = GetElementPtrInst::Create(targetVal->getType(),
                                                                     targetVal,
                                                                     newIndexValues,
                                                                     "",
                                                                     gep);
            gep->moveBefore(targetGEP);

            // Now append the old indices. Otherwise, we would have to reverse
            // the list and add the GEP indices in reversed order as well,
            // leading to more confusion.
            generateWriteBackOperations(gep, gep, targetGEP, indexVal, info);

            continue;
        }

        // We do not have to write back after a load.
        if (isa<LoadInst>(useI)) continue;

        if (!isa<StoreInst>(useI) && !isa<CallInst>(useI))
        {
            assert (false && "pointer is modified by unsupported operation");
            return false;
        }

        // Store back every element of extractedVal to targetVal *behind* the use.
        Instruction* insertBefore = nullptr;
        BasicBlock* parentBB = useI->getParent();
        for (auto I=parentBB->begin(), IE=parentBB->end(); I!=IE; ++I)
        {
            if (useI != &*I) continue;

            ++I;

            insertBefore = &*I;
            break;
        }
        assert (insertBefore);

        generateHorizontalInsert(extractedVal, targetVal, indexVal, insertBefore, info);
    }
    return true;
}

Value*
FunctionVectorizer::createPointerCast(Value*       pointer,
                                      Instruction* insertBefore)
{
    assert (pointer);

    Type* oldType = pointer->getType();
    if (WFV::isVectorizedType(*oldType)) return pointer;

    Type* newType = WFV::vectorizeSIMDType(oldType, mInfo->mVectorizationFactor);

    BitCastInst* pktPtrCast = new BitCastInst(pointer,
                                              newType,
                                              "pktPtrCast",
                                              insertBefore);

    WFV::setMetadata(pktPtrCast, WFV::WFV_METADATA_PKT_PTR_CAST);
    WFV::setMetadata(pktPtrCast, WFV::WFV_METADATA_RES_VECTOR);
    WFV::setMetadata(pktPtrCast, WFV::WFV_METADATA_OP_UNIFORM);
    WFV::setMetadata(pktPtrCast, WFV::WFV_METADATA_INDEX_CONSECUTIVE);
    if (WFV::hasMetadata(pointer, WFV::WFV_METADATA_ALIGNED_TRUE))
        WFV::setMetadata(pktPtrCast, WFV::WFV_METADATA_ALIGNED_TRUE);
    else
        WFV::setMetadata(pktPtrCast, WFV::WFV_METADATA_ALIGNED_FALSE);

    //outs() << "  inserted new pointer cast: " << *pktPtrCast << "\n";

    return pktPtrCast;
}

Instruction*
FunctionVectorizer::createPTest(Value* cond, Instruction* insertBefore) const
{
    // The sequence is: <W x i1> -> SExt to <W x i32> -> bitcast to iX (X = W * 32).
    // NOTE: LLVM apparently generates slightly less efficient code if the
    //       bitcast size does not match the type of the original comparison
    //       operators (e.g. if on SSE, <4 x double> is used, the mask is
    //       <4 x i1> nevertheless, so we bitcast to i128 because we multiply
    //       with 32 (see WFVInfo constructor). LLVM in that case generates one
    //       additional shift instruction or so in comparison to if we bitcast
    //       to i256.
    assert (cond && insertBefore);
    assert ((cond->getType() == mInfo->mVectorTyBoolSIMD ||
             cond->getType()->isIntegerTy(1)) &&
            "all mask computations should be of type <W x i1> or i1!");

    if (cond->getType()->isIntegerTy(1))
    {
        cond = broadcastValue(cond, insertBefore, *mInfo);
        if(!cond)
            return nullptr;
    }

    // We have to "convert" the mask to <W x float> to match the intrinsic.
    SExtInst*    sext = new SExtInst(cond, mInfo->mVectorTyIntSIMD, "", insertBefore);
    BitCastInst* bc   = new BitCastInst(sext, mInfo->mScalarTyIntSIMD, "", insertBefore);

    WFV::copyMetadata(sext, *cond);
    WFV::setMetadata(bc, WFV::WFV_METADATA_RES_UNIFORM);
    WFV::setMetadata(bc, WFV::WFV_METADATA_OP_UNIFORM);
    WFV::setMetadata(bc, WFV::WFV_METADATA_MASK);

    // Insert 'allfalse'-comparison (jump back to header if any mask element is not 0).
    Instruction* ptest = CmpInst::Create(Instruction::ICmp,
                                         ICmpInst::ICMP_NE,
                                         bc,
                                         mInfo->mConstIntSIMDRegWidthZero,
                                         "",
                                         insertBefore);

    WFV::setMetadata(ptest, WFV::WFV_METADATA_OP_UNIFORM);
    WFV::setMetadata(ptest, WFV::WFV_METADATA_RES_UNIFORM);

    return ptest;
}

namespace {

// Helper for generateIf() / generateIfCascade().
void
updateMaskGraph(BasicBlock*       endBB,
                const BasicBlock& parentBB,
                MaskAnalysis*     maskAnalysis)
{
    // New end block has no masks yet, mask graph still holds information
    // of exits of parent block that are now exits of the end block.

    // Set entry mask of endBB to the one of parentBB (all paths join again).
    // Set exit masks of endBB to those stored for parentBB.
    maskAnalysis->copyMaskInfoToNewBlock(endBB, parentBB);

    // Clear exit masks of parentBB (successors are executed in scalar mode).
    maskAnalysis->clearExitMasks(parentBB);
}

}

// Generate an if-cascade at instruction 'inst':
// 1) Split parent block of 'inst' (inst remains in "upper" block = parentBB).
// 2) Generate 'info.mVectorizationFactor' blocks that extract the ith mask value,
//    and jump to an 'execution'-block if the value is true, or
//    to the next if-block.
// 3) Generate 'info.mVectorizationFactor' empty blocks where scalar operations
//    can be inserted afterwards. These blocks are executed depending on
//    the guarding ifs.
//
// This basically does the following:
// for (unsigned i=0, e=info.mVectorizationFactor; i<e; ++i) {
//     result[i] = mask[i] ? execute[i] : do_nothing()
// }
//
// do_nothing() results in an undef value, which is blended out due to the masks.
//
// 'ifBlocks' is assumed to be an (empty) array of size 'info.mVectorizationFactor'+1.
// 'targetBlocks' is assumed to be an (empty) array of size 'info.mVectorizationFactor'.
//
// State after execution of this function:
// - parent block of inst is split at the position of inst
// - first if-block is former parent block of 'inst' ("upper part")
// - last if-block is new block containing "lower part" of former parent block of 'inst'
// - each if-block holds mask extraction and scalar comparison if mask-instance is true
// - each target-block only holds an unconditional branch to the next if-block
//
// TODO: Add assertions or checks, e.g. to prevent generation for all-true-masks etc.
void
FunctionVectorizer::generateIfCascade(Instruction*  inst,
                                      Value*        mask,
                                      BasicBlock**  ifBlocks,
                                      BasicBlock**  targetBlocks,
                                      MaskAnalysis* maskAnalysis)
{
    assert (inst && mask && ifBlocks && targetBlocks && maskAnalysis && inst->getParent());
    assert (!WFV::hasMetadata(mask, WFV::WFV_METADATA_RES_UNIFORM) &&
            "must not attempt to generate if cascade for UNIFORM mask!");

    // TODO: The mask is not vectorized but we need to extract the
    //       elements. It would be cleaner to introduce unpack-calls
    //       for masks that will be split due to OP_SEQUENTIAL_GUARDED
    //       instructions, but that will be much more effort.
    //       Thus, we simply create a dummy here, extract elements from it,
    //       and perform an unchecked replace. This temporarily creates
    //       wrong IR, but in this case this is easy to handle since masks
    //       always have the same type.
    // NOTE: It *can* happen that the mask is vectorized if it is an argument
    //       (an input mask). Thus, we only create a dummy if that is not the
    //       case.
    Value* oldMask = nullptr;
    if (!WFV::isVectorizedType(*mask->getType()))
    {
        oldMask = mask;
        mask = WFV::createDummy(mInfo->mVectorTyBoolSIMD, inst);
    }

    if(mInfo->mVerbose) outs() << "  generating if-cascade...\n";
    if(mInfo->mVerbose) outs() << "    creating new blocks... ";

    // Split parent block and move all instructions after inst into endBB.
    BasicBlock* parentBB = inst->getParent();
    BasicBlock* endBB    = parentBB->splitBasicBlock(inst, parentBB->getName()+".casc.end");
    Function*   parentF  = parentBB->getParent();

    // Newly generated branch is not needed.
    parentBB->getTerminator()->eraseFromParent();

    // Add info on newly created block.
    WFV::copyMetadata(endBB, *parentBB);

    // Create blocks.
    for (unsigned i=0, e=mInfo->mVectorizationFactor; i<e; ++i)
    {
        if (i>0)
        {
            std::stringstream sstr;
            sstr << "casc.if" << i;
            ifBlocks[i] = BasicBlock::Create(*mInfo->mContext, sstr.str(), parentF, endBB);
            WFV::copyMetadata(ifBlocks[i], *parentBB);
        }
        std::stringstream sstr;
        sstr << "casc.exec" << i;
        targetBlocks[i] = BasicBlock::Create(*mInfo->mContext, sstr.str(), parentF, endBB);
        WFV::copyMetadata(targetBlocks[i], *parentBB);
        WFV::setMetadata(targetBlocks[i], WFV::WFV_METADATA_OPTIONAL);
    }

    // Those are not really if-blocks but this simplifies iteration.
    // - iterate until i<mVectorizationFactor and use i -> first 4 blocks (includes parent)
    // - iterate until i<mVectorizationFactor and use i+1 -> last 4 blocks (includes end)
    ifBlocks[0] = parentBB;
    ifBlocks[mInfo->mVectorizationFactor] = endBB;

    if(mInfo->mVerbose) outs() << "done.\n    generating unconditional branch statements... ";

    // Generate unconditional jump from each exec-block to next if-block.
    for (unsigned i=0, e=mInfo->mVectorizationFactor; i<e; ++i)
    {
        BranchInst* br = BranchInst::Create(ifBlocks[i+1], targetBlocks[i]);
        WFV::setMetadata(br, WFV::WFV_METADATA_OP_UNIFORM);
    }

    if(mInfo->mVerbose) outs() << "done.\n    generating extract statements of mask values... ";

    // Extract scalar values from entry-mask of exec-block.
    Value** masks = new Value*[mInfo->mVectorizationFactor]();
    for (unsigned i=0, e=mInfo->mVectorizationFactor; i<e; ++i)
    {
        masks[i] = ExtractElementInst::Create(mask,
                                              ConstantInt::get(*mInfo->mContext,
                                                               APInt(32, i)),
                                              "",
                                              ifBlocks[i]);
        WFV::setMetadata(cast<Instruction>(masks[i]), WFV::WFV_METADATA_OP_UNIFORM);
        WFV::setMetadata(cast<Instruction>(masks[i]), WFV::WFV_METADATA_RES_UNIFORM);
    }

    if(mInfo->mVerbose) outs() << "done.\n    generating conditional branch statements... ";

    // Generate conditional jump from each if-block to next exec-block/next if-block.
    for (unsigned i=0, e=mInfo->mVectorizationFactor; i<e; ++i)
    {
        BranchInst* br = BranchInst::Create(targetBlocks[i], ifBlocks[i+1], masks[i], ifBlocks[i]);
        WFV::setMetadata(br, WFV::WFV_METADATA_OP_UNIFORM);
    }

    if(mInfo->mVerbose) outs() << "done.\n    updating mask graph... ";

    updateMaskGraph(endBB, *parentBB, maskAnalysis);

    if(mInfo->mVerbose) outs() << "done.\n  successfully generated if-cascade!\n";

    // Remove dummy.
    if (oldMask)
    {
        WFV::uncheckedReplaceAllUsesWith(mask, oldMask);
        assert (isa<Instruction>(mask)); // The dummy is always an instruction.
        cast<Instruction>(mask)->eraseFromParent();
    }

    delete [] masks;
}

// Generate a simple if-statement without else-part at instruction 'inst':
// 1) Split parent block of 'inst' (inst remains in "upper" block = parentBB).
// 2) Generate target block and corresponding branches
//    the target block is executed if the mask is true at runtime.
//
// NOTE: If the mask is a vector mask, an "any-mask-index-true"-comparison
//       is generated. This means that the target block is executed if any
//       instance would execute it.
// NOTE: ifBB and targetBB are returned (have to be supplied uninitialized).
void
FunctionVectorizer::generateIf(Instruction*  inst,
                               Value*        mask,
                               MaskAnalysis* maskAnalysis,
                               BasicBlock**  outIfBB,
                               BasicBlock**  outTargetBB)
{
    assert (inst && mask && maskAnalysis);

    if(mInfo->mVerbose) {
        outs() << "  generating simple if-statement...\n";
        outs() << "    creating new blocks... ";
    }

    // Only generate a target-block (if mask is false, nothing is executed).

    // Split parent block and move all instructions after inst into endBB.
    BasicBlock* parentBB = inst->getParent();
    BasicBlock* endBB    = parentBB->splitBasicBlock(inst, parentBB->getName()+".if.merge");
    Function*   parentF  = parentBB->getParent();

    // Add info on newly created block.
    WFV::copyMetadata(endBB, *parentBB);

    // Create target-block.
    BasicBlock* targetBlock = BasicBlock::Create(*mInfo->mContext,
                                                 parentBB->getName()+".if.exec",
                                                 parentF,
                                                 endBB);
    WFV::copyMetadata(targetBlock, *parentBB);
    WFV::setMetadata(targetBlock, WFV::WFV_METADATA_OPTIONAL);

    if(mInfo->mVerbose) outs() << "done.\n    generating unconditional branch statement... ";
    BranchInst* br = BranchInst::Create(endBB, targetBlock);
    WFV::setMetadata(br, WFV::WFV_METADATA_OP_UNIFORM);

    if (!isa<Constant>(mask) &&
        !WFV::hasMetadata(mask, WFV::WFV_METADATA_RES_UNIFORM))
    {
        if(mInfo->mVerbose)
            outs() << "done.\n    VARYING mask found - generating "
                   << "'any-mask-index-true'-comparison for conditional branch... ";

        // Insert ptest.
        mask = createPTest(mask, parentBB->getTerminator());
        if(!mask)
            return;
    }

    // New branch generated by splitBasicBlock() is not needed anymore.
    parentBB->getTerminator()->eraseFromParent();

    if(mInfo->mVerbose) outs() << "done.\n    generating final conditional branch statement... ";

    // Generate conditional jump from parent-block to exec-block/end-block.
    BranchInst* br2 = BranchInst::Create(targetBlock, endBB, mask, parentBB);
    WFV::setMetadata(br2, WFV::WFV_METADATA_OP_UNIFORM);

    if(mInfo->mVerbose) outs() << "done.\n    updating mask graph... ";

    updateMaskGraph(endBB, *parentBB, maskAnalysis);

    if(mInfo->mVerbose) outs() << "done.\n  successfully generated simple if!\n";

    // Store back blocks.
    *outIfBB     = parentBB;
    *outTargetBB = targetBlock;

    return;
}
