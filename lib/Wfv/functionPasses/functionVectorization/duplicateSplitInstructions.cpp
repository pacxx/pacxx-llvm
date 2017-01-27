/**
 * @file   duplicateSplitInstructions.cpp
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
#include "wfv/utils/metadata.h" // copyMetadata()
#include "wfv/utils/wfvTools.h" // vectorizeSIMDType()

#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstIterator.h"

using namespace llvm;

namespace {

CallInst*
createExtractIntrinsic(CallInst*      oldUnpackCall,
                       Function*      unpackWFn,
                       const unsigned index)
{
    assert (oldUnpackCall && unpackWFn);

    // Get original operand.
    Value* operand = oldUnpackCall->getArgOperand(0);

    SmallVector<Value*, 2> params;
    params.push_back(operand);
    params.push_back(ConstantInt::get(Type::getInt32Ty(oldUnpackCall->getType()->getContext()),
                                      index,
                                      false));

    CallInst* newUnpackCall = CallInst::Create(unpackWFn,
                                               ArrayRef<Value*>(params),
                                               "unpackW",
                                               oldUnpackCall);

    newUnpackCall->setTailCall();
    newUnpackCall->setDoesNotAccessMemory();
    newUnpackCall->setDoesNotThrow();

    // Copy properties from oldUnpackCall.
    WFV::copyMetadata(newUnpackCall, *oldUnpackCall);

    return newUnpackCall;
}

CallInst*
createInsertIntrinsic(CallInst*      oldPackCall,
                      Value**        scalarInsts,
                      Function*      packWFn,
                      const unsigned vectorizationFactor)
{
    assert (oldPackCall && scalarInsts && packWFn);

    CallInst* packCall = CallInst::Create(packWFn,
                                          ArrayRef<Value*>(scalarInsts, vectorizationFactor),
                                          "packW",
                                          oldPackCall);

    packCall->setTailCall();
    packCall->setDoesNotAccessMemory();
    packCall->setDoesNotThrow();

    // Copy properties from oldPackCall.
    WFV::copyMetadata(packCall, *oldPackCall);

    return packCall;
}

bool
isSpecialGEP(const GetElementPtrInst& gep)
{
    const Value* ptr = gep.getPointerOperand();

    if (isa<GlobalValue>(ptr) ||
        WFV::hasMetadata(ptr, WFV::WFV_METADATA_RES_UNIFORM) ||
        WFV::hasMetadata(ptr, WFV::WFV_METADATA_RES_SCALARS))
    {
        return false;
    }

    // Generating a "pointer arithmetic" GEP only works if we do not have to
    // perform a load during extraction (i.e. nested pointers).
    return !WFV::hasNestedPointer(*ptr->getType());
}

GetElementPtrInst*
createSpecialGEP(GetElementPtrInst* gep,
                 Value*             indexVal,
                 const unsigned     vectorizationFactor)
{
    assert (gep && indexVal);

    Value* pointer = gep->getPointerOperand();
    Value* oldPointer = pointer;

    // Since execution of createSpecialGEP() depends on isSpecialGEP(),
    // we know that this is only called for RES_VECTOR pointers.
    assert (WFV::hasMetadata(pointer, WFV::WFV_METADATA_RES_VECTOR));

    // We have to make sure that we create the new GEP with the correct type.
    // If the pointer has not yet been vectorized, we have to introduce a dummy.
    const bool needDummyPtr = !WFV::isVectorizedType(*pointer->getType());
    if (needDummyPtr)
    {
        assert (WFV::isVectorizableType(*pointer->getType()));
        Type* vectorPtrType = WFV::vectorizeSIMDType(pointer->getType(), vectorizationFactor);
        pointer = WFV::createDummy(vectorPtrType, gep);
    }

    // Create GEP with additional index for vector element.
    std::vector<Value*> indices;
    for (GetElementPtrInst::op_iterator IDX=gep->idx_begin(),
        IDXE=gep->idx_end(); IDX!=IDXE; ++IDX)
    {
        indices.push_back(*IDX);
    }
    indices.push_back(indexVal);

    GetElementPtrInst* newGEP = GetElementPtrInst::Create(pointer->getType(),
                                                          pointer,
                                                          ArrayRef<Value*>(indices),
                                                          "",
                                                          gep);

    // Set cloned GEP metadata.
    WFV::setMetadata(newGEP, WFV::WFV_METADATA_RES_UNIFORM);
    WFV::setMetadata(newGEP, WFV::WFV_METADATA_OP_UNIFORM);
    WFV::setMetadata(newGEP, WFV::WFV_METADATA_INDEX_RANDOM);
    WFV::setMetadata(newGEP, WFV::WFV_METADATA_ALIGNED_FALSE);

    if (needDummyPtr)
    {
        WFV::uncheckedReplaceAllUsesWith(pointer, oldPointer);
        cast<Instruction>(pointer)->eraseFromParent();
    }

    return newGEP;
}

}

// For every instruction that is OP_SEQUENTIAL or OP_SEQUENTIAL_GUARDED:
// - Duplicate the instruction W times (W = SIMD width).
void
FunctionVectorizer::duplicateSplitInstructions(Function*      f,
                                               const unsigned vectorizationFactor)
{
    assert (f);
    assert (f->getParent());

    Module* mod = f->getParent();

    // We use a vector to store which instructions to delete
    // to make sure we delete in the right order (there will
    // be dependencies left between some of the unneeded
    // instructions).
    SmallVector<Instruction*, 64> deleteVec;
    DenseMap<Instruction*, Value**> duplicateMap;

    for (auto &BB : *f)
    {
        for (auto &I : BB)
        {
            Instruction* inst = &I;

            if (!WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL) &&
                !WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                continue;
            }

            // ignore pacxx instructions to prevent duplication of intrinsic calls
            // Ignore forward calls (marked 'sequential').
            if (isForwardFunctionCall(inst)) continue;

            // Special WFV function calls should not be marked 'sequential'.
            assert (!isPackFunctionCall(inst));
            assert (!isUnpackFunctionCall(inst));
            assert (!WFV::isMetadataCall(inst));

            if(mInfo->mVerbose) {
                outs() << "duplicating instruction: " << *inst << "\n";
            }

            // Duplicate instruction W times and store mapping.
            // Store as Value** to make creation of ArrayRef easier.
            Value** insts = new Value*[vectorizationFactor]();
            duplicateMap[inst] = insts;
            const bool isSpecial = isa<GetElementPtrInst>(inst) &&
                isSpecialGEP(*cast<GetElementPtrInst>(inst));

            for (unsigned i=0; i<vectorizationFactor; ++i)
            {
                // Fix up GEPs with pointers to structs, arrays, or vectors
                // (= insert additional index) without nested pointers.
                // These GEPs perform pointer arithmetic, e.g. for an array
                // access to a VARYING array. Unchanged, the GEP would return
                // a pointer to a vector instead of a scalar element:
                // scalar:
                //   getelementptr i32*       %array, i64 %idx -> i32*
                // simd (wrong):
                //   getelementptr <4 x i32>* %array, i64 %idx -> <4 x i32>*
                //   getelementptr <4 x i32>* %array, i64 %idx -> <4 x i32>*
                //   getelementptr <4 x i32>* %array, i64 %idx -> <4 x i32>*
                //   getelementptr <4 x i32>* %array, i64 %idx -> <4 x i32>*
                // simd (correct):
                //   getelementptr <4 x i32>* %array, i64 %idx, i64 0 -> i32*
                //   getelementptr <4 x i32>* %array, i64 %idx, i64 1 -> i32*
                //   getelementptr <4 x i32>* %array, i64 %idx, i64 2 -> i32*
                //   getelementptr <4 x i32>* %array, i64 %idx, i64 3 -> i32*
                if (isSpecial)
                {
                    insts[i] = createSpecialGEP(cast<GetElementPtrInst>(inst),
                                                ConstantInt::get(*mInfo->mContext,
                                                                 APInt(32, i)),
                                                mInfo->mVectorizationFactor);
                    continue;
                }

                Instruction* clone = inst->clone();
                clone->insertBefore(inst);
                insts[i] = clone;

                // Set cloned instructions to OP_UNIFORM/RES_UNIFORM.
                WFV::setMetadata(clone, WFV::WFV_METADATA_OP_UNIFORM);
                WFV::setMetadata(clone, WFV::WFV_METADATA_RES_UNIFORM);
            }

            // Replace 1-to-1 unpack call by W calls that will later be
            // transformed into extract operations.
            for (Instruction::op_iterator OP=inst->op_begin(), OPE=inst->op_end(); OP!=OPE; ++OP)
            {
                assert (isa<Value>(OP));
                Value* opVal = cast<Value>(*OP);

                // Ignore operands that are no unpack calls.
                if (!isa<Instruction>(opVal)) continue;
                if (!isUnpackFunctionCall(cast<Instruction>(opVal))) continue;

                CallInst* oldUnpackCall = cast<CallInst>(opVal);

                Type* returnType = oldUnpackCall->getType();
                SmallVector<Type*, 2> paramTypes;
                paramTypes.push_back(oldUnpackCall->getArgOperand(0)->getType());
                paramTypes.push_back(Type::getInt32Ty(returnType->getContext()));

                Function* unpackWFn = getSpecialWFVFunction(WFV_FUNCTION_NAME_UNPACK_W,
                                                            returnType,
                                                            ArrayRef<Type*>(paramTypes),
                                                            mod);

                for (unsigned i=0; i<vectorizationFactor; ++i)
                {
                    CallInst* extract = createExtractIntrinsic(oldUnpackCall,
                                                               unpackWFn,
                                                               i);

                    cast<Instruction>(insts[i])->replaceUsesOfWith(oldUnpackCall, extract);
                }

                // There should be exactly one use left, the one in 'inst'
                // which is not removed yet.
                assert (oldUnpackCall->getNumUses() == 1);

                // Now we can remove the old unpack call.
                deleteVec.push_back(oldUnpackCall);
            }

            // If the instruction has no return value, there are no
            // uses we could redirect to 'pack' nodes or rewire in
            // case of 'forward' nodes.
            if (inst->getType()->isVoidTy())
            {
                // Now we can remove the old instruction.
                deleteVec.push_back(inst);
                continue;
            }


            // Replace uses of the call.
            // It can have at most 2 uses: one for a "pack" call, one for a
            // "forward" call.
            // "pack" calls are replaced by "packW" calls.
            // "forward" calls are replaced by "forwardW" calls in a second step.
            assert (inst->getNumUses() <= 2);

            for (Instruction::user_iterator U=inst->user_begin(),
                    UE=inst->user_end(); U!=UE; ++U)
            {
                assert (isa<Instruction>(*U));

                if (!isPackFunctionCall(cast<Instruction>(*U))) continue;
                CallInst* oldCall = cast<CallInst>(*U);

                assert (WFV::isVectorizableType(*inst->getType()) &&
                        "must not create 'packW' intrinsic of type without vector equivalent!");

                // Replace 1-to-1 pack call by W-to-1 call that will later be
                // transformed into a merge sequence.
                SmallVector<Type*, 4> paramTypes;
                for (unsigned i=0; i<vectorizationFactor; ++i)
                {
                    paramTypes.push_back(inst->getType());
                }
                Function* packWFn = getSpecialWFVFunction(WFV_FUNCTION_NAME_PACK_W,
                                                         inst->getType(),
                                                         ArrayRef<Type*>(paramTypes),
                                                         mod);

                // Create new pack call that takes W parameters.
                CallInst* newCall = createInsertIntrinsic(oldCall, insts, packWFn, vectorizationFactor);

                // Create the SIMD function for 'packWFn' (which will replace the
                // currently inserted packWFn during instruction vectorization).
                // It only differs in the return type.
                Function* packWFnSIMD =
                        getSpecialWFVFunction(WFV_FUNCTION_NAME_PACK_W,
                                              WFV::vectorizeSIMDType(inst->getType(),
                                                                     vectorizationFactor),
                                              ArrayRef<Type*>(paramTypes),
                                              mod);

                // Insert function mapping if the function was newly created.
                if (!mInfo->mFunctionInfoMap.hasMapping(*packWFn))
                {
                    mInfo->addSIMDMapping(*packWFn, *packWFnSIMD, -1, false);
                }

                // Replace uses of old pack call with new one.
                oldCall->replaceAllUsesWith(newCall);
                assert (oldCall->user_empty());
            }

            for (Instruction::user_iterator U=inst->user_begin(),
                    UE=inst->user_end(); U!=UE; ++U)
            {
                assert (isa<Instruction>(*U));

                if (!isForwardFunctionCall(cast<Instruction>(*U))) continue;
                deleteVec.push_back(cast<Instruction>(*U));
            }

            // Now we can remove the old instruction.
            deleteVec.push_back(inst);
        }
    }

    // Remove "forward" calls, rewiring the uses.
    for (auto &BB : *f)
    {
        for (auto &I : BB)
        {
            Instruction* inst = &I;

            if (!isForwardFunctionCall(inst)) continue;

            CallInst* forwardCall = cast<CallInst>(inst);
            if(mInfo->mVerbose) outs() << "replacing forward call: " << *forwardCall << "\n";

            // Get the definition and its duplicates.
            assert (forwardCall->getNumArgOperands() == 1);
            assert (isa<Instruction>(forwardCall->getArgOperand(0)));
            Instruction* def = cast<Instruction>(forwardCall->getArgOperand(0));

            if(mInfo->mVerbose) outs() << "  def: " << *def << "\n";

            assert (duplicateMap.count(def));
            Value** defDuplicates = duplicateMap[def];

            // For every use of the forward call, get its duplicates
            // and replace the use of the call by the appropriate duplicate
            // of the definition.
            for (Instruction::user_iterator U=forwardCall->user_begin(),
                    UE=forwardCall->user_end(); U!=UE; ++U)
            {
                assert (isa<Instruction>(*U));
                Instruction* use = cast<Instruction>(*U);

                // Ignore uses in split instructions (no "normal" use can be
                // RES_UNIFORM).
                if (WFV::hasMetadata(use, WFV::WFV_METADATA_RES_UNIFORM)) continue;

                if(mInfo->mVerbose) outs() << "  use: " << *use << "\n";

                assert (duplicateMap.count(use));
                Value** useDuplicates = duplicateMap[use];

                for (unsigned i=0; i<vectorizationFactor; ++i)
                {
                    Instruction* defDupI = cast<Instruction>(defDuplicates[i]);
                    Instruction* useDupI = cast<Instruction>(useDuplicates[i]);
                    useDupI->replaceUsesOfWith(forwardCall, defDupI);
                }
            }

            if (forwardCall->user_empty()) deleteVec.push_back(forwardCall);
        }
    }

    // Create conditional branch or if cascade for instructions that require guards.
    SmallVector<Instruction*, 16> guardVec;
    for (auto &BB : *f)
    {
        for (auto &I : BB)
        {
            Instruction* inst = &I;
            if (!WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED)) continue;
            assert (duplicateMap.count(inst));
            guardVec.push_back(inst);
        }
    }

    for (auto &inst : guardVec) {
        createGuards(inst, duplicateMap);
    }

    // Now we can finally remove the instructions.
    // Unfortunately, forward calls and instructions depend
    // on each other in a way that it is not easily possible
    // to delete them in one pass.
    // Only for pack calls we can safely decide to delete them first.
    SmallVector<Instruction *, 8> packsToRemove;
    for (auto &BB : *f)
    {
        for (BasicBlock::iterator I=BB.begin(), IE=BB.end(); I!=IE;)
        {
            Instruction* inst = &*I;
            if (!isPackFunctionCall(inst)) {I++; continue; }

            if(mInfo->mVerbose) {
                outs() << "removing pack call: " << *inst << "\n";
            }
            packsToRemove.push_back(inst);
            I++;
        }
    }

    for(auto pack : packsToRemove) {
        pack->eraseFromParent();
    }

    unsigned deleted = 1;
    while (deleted)
    {
        deleted = 0;
        for (unsigned i=0, e=deleteVec.size(); i<e; ++i)
        {
            Instruction* inst = deleteVec[i];
            if (!inst || !inst->user_empty()) continue;

            if(mInfo->mVerbose) outs() << "removing instruction: " << *inst << "\n";
            inst->eraseFromParent();
            deleteVec[i] = nullptr;
            ++deleted;
        }
    }

    for (auto &it : duplicateMap)
    {
        delete [] it.second;
    }

    /*
    if(mInfo->mVerbose) {
        unsigned notDeleted = 0;
        for (unsigned i = 0, e = deleteVec.size(); i < e; ++i) {
            if (deleteVec[i]) ++notDeleted;
            if (deleteVec[i]) outs() << "not deleted: " << *deleteVec[i] << "\n";
        }
        assert(notDeleted == 0 && "failed to delete instructions!");

        // Make sure there are no "old" pack/unpack/forward left in the function.
        for (inst_iterator I = inst_begin(f), E = inst_end(f); I != E; ++I) {
            assert(!isPackFunctionCall(&*I));
            assert(!isUnpackFunctionCall(&*I));
            assert(!isForwardFunctionCall(&*I));
        }
    }
    */
}

void
FunctionVectorizer::createGuards(Instruction*                     inst,
                                 DenseMap<Instruction*, Value**>& duplicateMap)
{
    assert (inst);

    if(mInfo->mVerbose) {
        outs() << "\n  instruction requires guards: " << *inst << "\n";
    }

    BasicBlock* parentBB = inst->getParent();
    Value* mask = mMaskAnalysis->getEntryMask(*parentBB);

    // If the mask is OP_SEQUENTIAL we must not use it directly but use
    // its "packW" call.
    const bool isSequentialMask = isa<Instruction>(mask) &&
        (WFV::hasMetadata(mask, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
         WFV::hasMetadata(mask, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED));

    if (isSequentialMask)
    {
        Instruction* maskI = cast<Instruction>(mask);
        assert (duplicateMap.count(maskI));
        Value** maskInsts = duplicateMap[maskI];
        // Get the pack call to make this code a bit easier.
        Value* firstMask = maskInsts[0];

        Instruction* packCall = nullptr;
        for (Value::user_iterator U=firstMask->user_begin(),
             UE=firstMask->user_end(); U!=UE; ++U)
        {
            if(Instruction* useI = dyn_cast<Instruction>(*U)) {
                if (!isPackWFunctionCall(useI)) continue;
                packCall = useI;
            }
        }
        assert (packCall);
        mask = packCall;

        if(mInfo->mVerbose) {
            outs() << "    mask comes from packed sequential operation: "
                   << *mask << "\n";
        }
    }

    // Before block creation, move old value directly behind the split
    // values. This temporarily breaks domination relations, but
    // ultimately puts all instructions to the right places because
    // splitting then ensures that possible merge-operations are in
    // the block *behind* the if.
    assert (duplicateMap.count(inst));
    Value** insts = duplicateMap[inst];

    Value* lastSplitVal = insts[mInfo->mVectorizationFactor-1];
    assert (isa<Instruction>(lastSplitVal));
    Instruction* lastSplitValI = cast<Instruction>(lastSplitVal);

    inst->moveBefore(lastSplitValI); // move split position above merge code
    lastSplitValI->moveBefore(inst); // swap again

    // Special case if the value is a UNIFORM store or call (which can
    // only happen here if it depends on VARYING control-flow):
    // Splitting means doing an "any-mask-index-true"-comparison
    // followed by a branch to a scalar store.
    // NOTE: !UNIFORM is not enough, !FULLY_UNIFORM is required!
    if (WFV::hasMetadata(inst, WFV::WFV_METADATA_RES_UNIFORM) ||
        isa<Constant>(mask) ||
        WFV::hasMetadata(mask, WFV::WFV_METADATA_RES_UNIFORM))
    {
        assert (!isSequentialMask);
        assert (!mInfo->mDisableControlFlowDivAnalysis ||
                (isa<LoadInst>(inst) || isa<StoreInst>(inst) || isa<CallInst>(inst)));
        assert (mInfo->mDisableControlFlowDivAnalysis ||
                ((isa<StoreInst>(inst) || isa<CallInst>(inst)) &&
                 (WFV::hasMetadata(parentBB, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) ||
                  WFV::hasMetadata(parentBB, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE))));

        BasicBlock* ifBB     = NULL;
        BasicBlock* targetBB = NULL;

        generateIf(inst,
                   mask,
                   mMaskAnalysis,
                   &ifBB,
                   &targetBB);

        if(mInfo->mVerbose) mMaskAnalysis->verifyAnalysis();

        // Move split values into the target block.
        for (unsigned i=0; i<mInfo->mVectorizationFactor; ++i)
        {
            Value* splitVal = insts[i];
            assert (isa<Instruction>(splitVal));
            Instruction* splitValI = cast<Instruction>(splitVal);
            splitValI->moveBefore(targetBB->getTerminator());

            // If the the split value has uses, create a phi for it.
            if (!splitValI->use_empty())
            {
                Type* scalarType = splitValI->getType();
                assert (!WFV::isVectorizedType(*scalarType) &&
                        "type of split value must be scalar!");
                assert (!inst->getParent()->empty());

                PHINode* phi = PHINode::Create(scalarType,
                                               2U,
                                               "",
                                               inst->getParent()->getFirstNonPHI());

                // Replace uses before adding the use that is the phi itself.
                splitValI->replaceAllUsesWith(phi);

                phi->addIncoming(splitValI, targetBB);

                // Insert dummy-values where the mask is 0.
                phi->addIncoming(UndefValue::get(scalarType), ifBB);

                WFV::setMetadata(phi, WFV::WFV_METADATA_OP_UNIFORM);
                WFV::setMetadata(phi, WFV::WFV_METADATA_RES_UNIFORM);
            }
        }

        return;
    }

    // Create if-cascade:
    // Each if-block holds mask extraction and scalar comparison if mask-instance is true.
    // Each use-block holds scalar use (either load or store with operand extraction code).
    BasicBlock** ifBlocks     = new BasicBlock*[mInfo->mVectorizationFactor+1]();
    BasicBlock** targetBlocks = new BasicBlock*[mInfo->mVectorizationFactor]();

    generateIfCascade(inst,
                      mask,
                      ifBlocks,
                      targetBlocks,
                      mMaskAnalysis);

    if(mInfo->mVerbose) mMaskAnalysis->verifyAnalysis();

    // Move split values into the correct target blocks
    for (unsigned i=0; i<mInfo->mVectorizationFactor; ++i)
    {
        Value* splitValue = insts[i];
        assert (isa<Instruction>(splitValue));
        Instruction* splitValI = cast<Instruction>(splitValue);
        splitValI->moveBefore(targetBlocks[i]->getTerminator());

        // If the the split value has uses, create a phi for it.
        if (!splitValI->use_empty())
        {
            Type* scalarType = splitValI->getType();
            assert (!WFV::isVectorizedType(*scalarType) &&
                    "type of split value must be scalar!");
            assert (!ifBlocks[i+1]->empty());

            PHINode* phi = PHINode::Create(scalarType,
                                           2U,
                                           "",
                                           ifBlocks[i+1]->getFirstNonPHI());

            // Replace uses before adding the use that is the phi itself.
            splitValI->replaceAllUsesWith(phi);

            phi->addIncoming(splitValI, targetBlocks[i]);

            // Insert dummy-values where the mask is 0.
            phi->addIncoming(UndefValue::get(scalarType), ifBlocks[i]);

            WFV::setMetadata(phi, WFV::WFV_METADATA_OP_UNIFORM);
            WFV::setMetadata(phi, WFV::WFV_METADATA_RES_UNIFORM);
        }
    }

    if(mInfo->mVerbose) outs() << "done.\n    finished generation of "
        << "'extract-execute-insert' scheme for split value!\n";

    delete [] targetBlocks;
    delete [] ifBlocks;
}

