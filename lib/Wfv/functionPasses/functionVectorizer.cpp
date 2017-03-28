/**
 * @file   functionVectorizer.cpp
 * @date   30.05.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 *
 * Vectorization works as follows:
 *
 * 1) For every instruction that is OP_SEQUENTIAL or OP_SEQUENTIAL_GUARDED:
 *    Redirect every operand that is not RES_UNIFORM to an "unpack" intrinsic.
 *    Redirect every use to a "pack" intrinsic.
 * 2) "Pack" and "unpack" operations are optimized using heuristics.
 *    During this, OP_VARYING instructions may be modified to OP_SEQUENTIAL or
 *    OP_SEQUENTIAL_GUARDED.
 * 3) For every instruction that is OP_SEQUENTIAL or OP_SEQUENTIAL_GUARDED:
 *    Duplicate the instruction W times (W = SIMD width).
 * 4) Replace "pack" and "unpack" intrinsics by insert/extract operations and
 *    update the uses accordingly.
 * 5) For every instruction that is OP_SEQUENTIAL_GUARDED:
 *    Move the duplicates into if-statements that test the corresponding mask element.
 *    Move the unpack operations of their operands into the corresponding if-statements.
 * 6) Replace every instruction that is OP_VARYING by its vector counterpart.
 * 7) Broadcast RES_UNIFORM operands of OP_VARYING instructions to vectors,
 *    except for uniform arguments to calls.
 */

#include "wfv/functionPasses/functionVectorizer.h"
#include "wfv/wfvConfig.h"
#include "wfv/utils/wfvTools.h"
#include "wfv/utils/metadata.h"

#include "llvm/Transforms/Utils/Cloning.h" // CloneFunctionInto()
#include "llvm/IR/Verifier.h" // verifyFunction()
#include "llvm/Support/Timer.h"

#include <stdexcept>


using namespace llvm;


char FunctionVectorizer::ID = 0;
INITIALIZE_PASS_BEGIN(FunctionVectorizer, "functionVectorizer", "FunctionVectorizer", false, false)
INITIALIZE_PASS_DEPENDENCY(WFVInfo)
INITIALIZE_PASS_DEPENDENCY(MaskAnalysis)
INITIALIZE_PASS_END(FunctionVectorizer, "functionVectorizer", "FunctionVectorizer", false, false)

// Public interface to the FunctionVectorizer pass
FunctionPass*
llvm::createFunctionVectorizerPass()
{
	return new FunctionVectorizer();
}



FunctionVectorizer::FunctionVectorizer() : FunctionPass(ID), mInfo(nullptr)
{
    initializeFunctionVectorizerPass(*PassRegistry::getPassRegistry());
}

FunctionVectorizer::~FunctionVectorizer()
{
    mInfo = nullptr; // Deleted by PassManager.
}

void
FunctionVectorizer::releaseMemory()
{
}

void
FunctionVectorizer::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<WFVInfo>();
    AU.addPreserved<WFVInfo>();

    AU.addRequired<MaskAnalysis>();
    AU.addPreserved<MaskAnalysis>();
}

bool
FunctionVectorizer::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
FunctionVectorizer::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

/**
 * Vectorization works as follows:
 *
 * 1) For every instruction that is OP_SEQUENTIAL or OP_SEQUENTIAL_GUARDED:
 *    Redirect every operand that is not RES_UNIFORM to an "unpack" intrinsic.
 *    Redirect every use to a "pack" intrinsic.
 * 2) "Pack" and "unpack" operations are optimized using heuristics.
 *    During this, OP_VARYING instructions may be modified to OP_SEQUENTIAL or
 *    OP_SEQUENTIAL_GUARDED.
 * 3) For every instruction that is OP_SEQUENTIAL or OP_SEQUENTIAL_GUARDED:
 *    Duplicate the instruction W times (W = SIMD width).
 * 4) Replace "pack" and "unpack" intrinsics by insert/extract operations and
 *    update the uses accordingly.
 * 5) For every instruction that is OP_SEQUENTIAL_GUARDED:
 *    Move the duplicates into if-statements that test the corresponding mask element.
 *    Move the unpack operations of their operands into the corresponding if-statements.
 * 6) Replace every instruction that is OP_VARYING by its vector counterpart.
 * 7) Broadcast RES_UNIFORM operands of OP_VARYING instructions to vectors,
 *    except for uniform arguments to calls.
 **/
bool
FunctionVectorizer::runOnFunction(Function& F)
{
    mInfo         = &getAnalysis<WFVInfo>();
    mMaskAnalysis = &getAnalysis<MaskAnalysis>();

    if(mInfo->mVerbose) {
        outs() << "\n#########################################################\n";
        outs() << "## INSTRUCTION VECTORIZATION\n";
        outs() << "#########################################################\n\n";
    }

    // If an error occurred in one of the previous phases, abort.
    assert (mInfo->mFailure);
    if (*mInfo->mFailure) return false;

    Timer t("FV", "Function Vectorizer", *mInfo->mTimerGroup);

    if(mInfo->mVerbose) {
        t.startTimer();
    }

    // Clone the function into the SIMD prototype.
    Function* f_SIMD = mInfo->mSimdFunction;
    ValueToValueMapTy valueMap;
    cloneIntoSIMDPrototype(f_SIMD, F, valueMap);

    // Map the mask analysis information to the new function.
    mMaskAnalysis->mapMaskInformation(valueMap);

    // 0)
    createPtrArgCasts(f_SIMD);

    // 1)
    if(mInfo->mVerbose) {
        outs() << "\n---------------------------------------------------------\n";
        outs() << "| INSERT PACK/UNPACK INTRINSICS\n";
        outs() << "---------------------------------------------------------\n";
    }
    insertPackUnpackIntrinsics(f_SIMD);

    if(mInfo->mVerbose) {
        outs() << "\nafter insertPackUnpackIntrinsics:";
        outs() << *f_SIMD;
    }

    // 2)
    if(mInfo->mVerbose) {
        outs() << "\n---------------------------------------------------------\n";
        outs() << "| OPTIMIZE PACK/UNPACK OPERATIONS\n";
        outs() << "---------------------------------------------------------\n";
    }
    optimizePackUnpack(f_SIMD);

    // 3)
    if(mInfo->mVerbose) {
        outs() << "\n---------------------------------------------------------\n";
        outs() << "| DUPLICATE SPLIT INSTRUCTIONS\n";
        outs() << "---------------------------------------------------------\n";
    }
    duplicateSplitInstructions(f_SIMD, mInfo->mVectorizationFactor);

    if(mInfo->mVerbose) {
        outs() << "\nafter duplicateSplitInstructions:";
        outs() << *f_SIMD;
    }

    //outs() << "temporary exit before instruction vectorization.\n";
    //return false;

    // 6)
    if(mInfo->mVerbose) {
        outs() << "\n---------------------------------------------------------\n";
        outs() << "| VECTORIZE INSTRUCTIONS\n";
        outs() << "---------------------------------------------------------\n";
    }

    if(!vectorizeInstructions(f_SIMD)) {
        errs() << "ERROR: error during vectorize instructions\n";
        *mInfo->mFailure = true;
        t.stopTimer();
        return true;
    }

    if(mInfo->mVerbose) {
        outs() << "\nafter vectorizeInstructions:";
        outs() << *f_SIMD;
    }

    // 4)
    // Can currently only be done after vectorization because
    // generateExtract/generateMerge require concrete vector types.
    // This could be rewritten if necessary, though.
    // Also, this relies on vectorization of 'pack' calls (changing
    // of return type from scalar to vector).
    if(mInfo->mVerbose) {
        outs() << "\n---------------------------------------------------------\n";
        outs() << "| GENERATE PACK/UNPACK OPERATIONS\n";
        outs() << "---------------------------------------------------------\n";
    }

    if(!generatePackUnpackCode(f_SIMD, *mInfo)) {
        errs() << "ERROR: error during generate pack/unpack\n";
        *mInfo->mFailure = true;
        t.stopTimer();
        return true;
    }

    if(mInfo->mVerbose) {
        outs() << "\nafter generatePackUnpackCode:";
        outs() << *f_SIMD;
    }

    // 5)
    if(mInfo->mVerbose) {
        outs() << "\n---------------------------------------------------------\n";
        outs() << "| GENERATE SIDE-EFFECT GUARDS\n";
        outs() << "---------------------------------------------------------\n";
    }
    generateSideEffectGuards(f_SIMD);

    if(mInfo->mVerbose) {
        outs() << "\nafter generateSideEffectGuards:";
        outs() << *f_SIMD;
    }

    // 7)
    if(mInfo->mVerbose) {
        outs() << "\n---------------------------------------------------------\n";
        outs() << "| BROADCAST UNIFORM OPERANDS\n";
        outs() << "---------------------------------------------------------\n";
    }

    if(!broadcastUniformOperands(f_SIMD)) {
        errs() << "ERROR: error during broadcast\n";
        *mInfo->mFailure = true;
        t.stopTimer();
        return true;
    }

    if(mInfo->mVerbose) {
        outs() << "\nafter broadcastUniformOperands:";
        f_SIMD->dump();
    }

    cleanup(f_SIMD);

    if(mInfo->mVerbose) {
        outs() << "\nafter cleanup:" << *f_SIMD;
        verifyFunction(*f_SIMD);
        t.stopTimer();
    }

    return true;
}

void
FunctionVectorizer::print(raw_ostream& O, const Module* M) const
{
}

void
FunctionVectorizer::cloneIntoSIMDPrototype(Function*          f_SIMD,
                                           const Function&    f,
                                           ValueToValueMapTy& valueMap) const
{
    assert (f_SIMD);
    assert (f_SIMD->isDeclaration());
    assert (!f.isDeclaration());

    Function::arg_iterator destI = f_SIMD->arg_begin();
    for (Function::const_arg_iterator I = f.arg_begin(),
            E = f.arg_end(); I != E; ++I)
    {
        destI->setName(I->getName()); // Copy the name over...
        valueMap[&*I] = &*destI;        // Add mapping to ValueMap
        destI++;
    }

    SmallVector<ReturnInst*, 10> returns;
    ClonedCodeInfo newFInfo;
    const char* nameSuffix = ".";

    if(mInfo->mVerbose) verifyFunction(f);

    // FIXME: This temporary declaration is only required to retain the attributes of f_SIMD.
    //        During CloneFunctionInto(), attributes are overwritten with those from f, which
    //        may lead to crashes due to wrong alignment of scalar byval struct parameters.
    //        The only thing that currently (3.2svn, 2012-05-16) works is copyAttributesFrom(),
    //        setAttributes() ignores alignment, removeAttr()/addAttr() fire assertions when
    //        attempting to modify alignment.
    Function* tmpF = Function::Create(f_SIMD->getFunctionType(),
                                      GlobalValue::ExternalLinkage,
                                      f_SIMD->getName().str()+".tmp",
                                      f_SIMD->getParent());

    tmpF->setCallingConv(f_SIMD->getCallingConv());
    tmpF->setAttributes(f_SIMD->getAttributes());
    tmpF->setAlignment(f_SIMD->getAlignment());
    tmpF->setLinkage(f_SIMD->getLinkage());

    // FIXME: Now (3.3svn, 2013-05-07), clang automatically generates alignments for the
    //        SIMD declarations of our test suite and CloneFunctionInto() fires assertions
    //        because the alignments of f and f_SIMD do not match.
    //        Thus, we now delete all attributes of the target function before cloning.
    SmallVector<AttributeList, 4> attrSets;
    AttributeList fnAS = AttributeList::get(*mInfo->mContext, attrSets);
    f_SIMD->setAttributes(fnAS);

    //clone without optimizing (need exactly the same function for mask-mapping)
    CloneFunctionInto(f_SIMD, &f, valueMap, false, returns, nameSuffix, &newFInfo);

    // Set all properties of the simd function again.
    f_SIMD->setCallingConv(tmpF->getCallingConv());
    f_SIMD->copyAttributesFrom(tmpF);
    f_SIMD->setAlignment(tmpF->getAlignment());
    f_SIMD->setLinkage(tmpF->getLinkage());
    tmpF->eraseFromParent();
}

void
FunctionVectorizer::createPtrArgCasts(Function* f)
{
    assert (f);

    // Make sure input vector types are of 32bit types.
    BasicBlock* entryBB = &f->getEntryBlock();
    Instruction* insertBefore = entryBB->getFirstNonPHI();

    for (Function::arg_iterator A=f->arg_begin(), AE=f->arg_end(); A!=AE; ++A)
    {
        if (A->use_empty()) continue;

        BitCastInst* bc = createBitCastToEquivalentVectorType(&*A, insertBefore);
        if (!bc) continue;

        WFV::setMetadata(bc, WFV::WFV_METADATA_ARGUMENT_CAST);
        WFV::setMetadata(bc, WFV::WFV_METADATA_RES_VECTOR);

        WFV::uncheckedReplaceAllUsesWith(&*A, bc);
        bc->replaceUsesOfWith(bc, &*A);

        // Update splitting info of argument (does not
        // require any splitting anymore).
        if (WFV::hasMetadata(&*A, WFV::WFV_METADATA_RES_SCALARS))
        {
            WFV::setMetadata(&*A, WFV::WFV_METADATA_RES_VECTOR);
        }
    }
}

// This function hardcodes some knowledge about equivalent types.
// For other architectures and/or frontends, it may be required to update this.
BitCastInst*
FunctionVectorizer::createBitCastToEquivalentVectorType(Argument*    arg,
                                                        Instruction* insertBefore)
{
    assert (arg && insertBefore);

    if(mInfo->mVerbose) outs() << "creating bitcast to equivalent vector type "
        << "for argument: " << *arg << "\n";

    Type* oldType = arg->getType();

    if (!WFV::isVectorizedType(*oldType)) {
        if(mInfo->mVerbose) outs() << "  is no vector type - ignored!\n";
        return nullptr;
    }

    if (oldType == mInfo->mVectorTyBoolSIMD ||
        oldType == mInfo->mVectorTyIntSIMD ||
        oldType == mInfo->mVectorTyFloatSIMD ||
        oldType == mInfo->mVectorTyLongSIMD ||
        oldType == mInfo->mVectorTyDoubleSIMD ||
        (oldType->isPointerTy() &&
         oldType->getPointerElementType() == mInfo->mVectorTyIntSIMD) ||
        (oldType->isPointerTy() &&
         oldType->getPointerElementType() == mInfo->mVectorTyFloatSIMD) ||
        (oldType->isPointerTy() &&
         oldType->getPointerElementType() == mInfo->mVectorTyLongSIMD) ||
        (oldType->isPointerTy() &&
         oldType->getPointerElementType() == mInfo->mVectorTyDoubleSIMD))
    {
        if(mInfo->mVerbose) outs() << "  is of suitable vector type - ignored!\n";
        return nullptr;
    }

    Type* newType = createEquivalentVectorType(oldType);
    if (!newType) {
        errs() << "ERROR: Could not create suitable vector type for bitcast of"
            " value: " << *arg << "!\n";
        return nullptr;
    }

    BitCastInst* bc = new BitCastInst(arg, newType, "", insertBefore);

    WFV::copyMetadata(bc, *arg);
    return bc;
}

Type*
FunctionVectorizer::createEquivalentVectorType(Type* oldType)
{
    assert (WFV::isVectorizedType(*oldType));

    if (oldType == mInfo->mVectorTyIntSIMD ||
        oldType == mInfo->mVectorTyFloatSIMD ||
        oldType == mInfo->mVectorTyLongSIMD ||
        oldType == mInfo->mVectorTyDoubleSIMD)
    {
        return oldType;
    }

    Type::TypeID oldTypeID = oldType->getTypeID();
    switch (oldTypeID) {

        case Type::VectorTyID:
        {
            VectorType* vType = cast<VectorType>(oldType);
            if (vType == VectorType::get(Type::getInt64Ty(*mInfo->mContext),
                                         mInfo->mVectorizationFactor / 2) ||
                vType == VectorType::get(Type::getInt16Ty(*mInfo->mContext),
                                         mInfo->mVectorizationFactor * 2) ||
                vType == VectorType::get(Type::getInt8Ty(*mInfo->mContext),
                                         mInfo->mVectorizationFactor  * 4))
            {
                return mInfo->mVectorTyIntSIMD;
            }
            if (vType == VectorType::get(Type::getDoubleTy(*mInfo->mContext),
                                         mInfo->mVectorizationFactor / 2))
            {
                return mInfo->mVectorTyFloatSIMD;
            }
            errs() << "ERROR: bad vector type found: " << *vType << "\n";
            return nullptr;
        }

        case Type::ArrayTyID:
        {
            ArrayType* aType = cast<ArrayType>(oldType);
            Type* elemType = aType->getElementType();
            if (!elemType) return nullptr;
            Type* newElemType = createEquivalentVectorType(elemType);
            return ArrayType::get(newElemType, aType->getNumElements());
        }

        case Type::StructTyID:
        {
            StructType* sType = cast<StructType>(oldType);

            std::vector<Type*> elems;
            for (unsigned i=0; i<sType->getNumElements(); ++i) {
                Type* elemType = sType->getElementType(i);
                if (!elemType) return nullptr;
                Type* newElemType = createEquivalentVectorType(elemType);
                elems.push_back(newElemType);
            }
            return StructType::get(*mInfo->mContext, elems, sType->isPacked());
        }
        case Type::PointerTyID:
        {
            PointerType* pType = cast<PointerType>(oldType);
            Type* elemType = pType->getElementType();
            if (!elemType) return nullptr;
            Type* newElemType = createEquivalentVectorType(elemType);
            return PointerType::get(newElemType, pType->getAddressSpace());
        }

        default :
        {
            errs() << "ERROR: bad vector type found: " << *oldType << "\n";
            return nullptr;
        }
    }
}

bool
FunctionVectorizer::isArgCast(const Value& value) const
{
    if (!isa<BitCastInst>(value)) return false;
    return WFV::hasMetadata(&value, WFV::WFV_METADATA_ARGUMENT_CAST);
}

void
FunctionVectorizer::cleanup(Function* f) const
{
    assert (f);

    if(mInfo->mVerbose) outs() << "Cleaning up function after WFV... ";

    DenseSet<Instruction*> deleteSet;

    for (auto &BB : *f)
    {
        for (auto &I : BB)
        {
            Instruction* inst = &I;
            if (WFV::isMetadataCall(inst))
            {
                deleteSet.insert(inst);
            }
        }
    }

    for (auto &inst : deleteSet)
    {
        assert (inst->use_empty());
        inst->eraseFromParent();
    }

    if(mInfo->mVerbose) outs() << "done.\n";
}
