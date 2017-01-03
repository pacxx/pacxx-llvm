/**
 * @file   wfvRegionBased.cpp
 * @date   11.03.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */

#include "wfv/wfvRegionBased.h"
#include "wfv/utils/metadata.h"
#include "wfv/wfvConfig.h"

// WFV utils
#include "wfv/utils/wfvTools.h"

// Variant stuff
#include "wfv/variantRegions/variantRegion.h"
#include "wfv/variantRegions/variantRegionFactory.h"

// LLVM stuff
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/CodeExtractor.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/IR/Dominators.h"
#include "llvm/ADT/SetVector.h"

#include <stdexcept>

using namespace llvm;
using namespace WFV::RegionBased;

////////////////////////////////////////////////////////////////////////////////
// Region-based WFV implementation.
////////////////////////////////////////////////////////////////////////////////

namespace {

bool
isSESERegion(Function*         parentFn,
             const BasicBlock* entryBB,
             const BasicBlock* exitBB)
{
    assert (entryBB && exitBB);
    assert (parentFn == exitBB->getParent());
    assert (parentFn == entryBB->getParent());

    DominatorTreeBase<BasicBlock>* DTB = new DominatorTreeBase<BasicBlock>(false);
    DominatorTreeBase<BasicBlock>* PTB = new DominatorTreeBase<BasicBlock>(true);
    DTB->recalculate(*parentFn);
    PTB->recalculate(*parentFn);

    const bool isSESE = DTB->properlyDominates(entryBB, exitBB) &&
        PTB->properlyDominates(exitBB, entryBB);

    delete DTB;
    delete PTB;

    return isSESE;
}

Instruction*
findVariantRegionEnd(BasicBlock* curBB,
                     const char* metaDataString)
{
    assert (curBB && metaDataString);

    for (auto &I : *curBB)
    {
        if (!WFV::hasMetadata(&I, WFV::WFV_METADATA_VARIANT_END)) continue;
        if (!WFV::hasMetadata(&I, metaDataString)) continue;

        // This is the end block of the variant.
        return &I;
    }

    // This is not our block -> recurse into children.
    TerminatorInst* terminator = curBB->getTerminator();
    for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
    {
        Instruction* endInst = findVariantRegionEnd(terminator->getSuccessor(i),
                                                    metaDataString);
        if (endInst) return endInst;
    }

    // We did not find the end block :(.
    return nullptr;
}

// Collects all blocks between the block "block" and the end block "endBB"
// TODO: There's functionality inside LLVM for this (llvm::Region), but it
//       requires RegionInfo, which has to be run in a PassManager.
void collectBlocks(BasicBlock*             entryBB,
                   BasicBlock*             exitBB,
                   SetVector<BasicBlock*>& region)
{
    if (region.count(entryBB)) return;
    region.insert(entryBB);

    if (entryBB == exitBB) return;

    TerminatorInst* terminator = entryBB->getTerminator();
    for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
    {
        collectBlocks(terminator->getSuccessor(i), exitBB, region);
    }
}

Instruction*
findVariantRegion(Function* f, SetVector<BasicBlock*>& region)
{
    assert (f);
    BasicBlock* entryBB = nullptr;
    BasicBlock* exitBB  = nullptr;
    Instruction* startInst = nullptr;

    for (auto &BB : *f)
    {
        for (auto &I : BB)
        {
            if (!WFV::hasMetadata(&I, WFV::WFV_METADATA_VARIANT_START)) continue;

            // This is the start block of a variant.
            startInst = &I;
            entryBB = &BB;

            // First, if the instruction is not the first one in the block,
            // we have to split it.
            if (&I != &entryBB->front())
            {
                BasicBlock* newBB = entryBB->splitBasicBlock(&I, entryBB->getName()+".split");
                entryBB = newBB;
            }

            // Now find the corresponding end block.
            const char* metaDataString = WFV::getVariantMetadata(&I);
            assert (metaDataString);
            Instruction* endInst = findVariantRegionEnd(entryBB, metaDataString);
            assert (endInst && "could not find end instruction of variant!");

            exitBB = endInst->getParent();

            // Split the block if necessary (i.e. if the instruction is not the
            // first one.
            if (endInst != &exitBB->front())
            {
                exitBB->splitBasicBlock(endInst, exitBB->getName()+".split");
                // The exit block remains the "first" of the two blocks.
            }

            // Make sure that this is a SESE region (after splitting, since we use
            // "properlyDominates").
            assert (isSESERegion(f, entryBB, exitBB) &&
                    "variant region marked is not a SESE region!");

            break;
        }
    }

    assert ((entryBB && exitBB) || (!entryBB && !exitBB));
    if (!entryBB) return nullptr;

    //DEBUG_WFV_NO_VERBOSE( outs() << "  region found: " << entryBB->getName() << " -> " << exitBB->getName() << "\n"; );

    // Collect all blocks of the region.
    collectBlocks(entryBB, exitBB, region);
    assert (!region.empty());

    return startInst;
}

#if 0
bool
isNotAlwaysByAllOrNone(const BasicBlock& block)
{
    return !WFV::hasMetadata(&block, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) &&
        !WFV::hasMetadata(&block, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE);
}
#endif

// Determines whether 'call' has at least one non-uniform argument.
// NOTE: This relies on WFV metadata being available for all arguments
//       of 'call' except the result pointers.
bool
isVaryingRegionCall(const CallInst& call)
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

void
addMaskArgument(Function* f, Function** newFn, CallInst** newCall)
{
    assert (f);
    assert (f->getNumUses() == 1);

    FunctionType* oldType = f->getFunctionType();

    // Get "old" param types.
    SmallVector<Type*, 4> paramTypes;
    for (unsigned i=0, e=oldType->getNumParams(); i<e; ++i)
    {
        paramTypes.push_back(oldType->getParamType(i));
    }
    // Add mask type.
    Type* int1Ty = Type::getInt1Ty(f->getContext());
    paramTypes.push_back(int1Ty);
    Type* oldRetType = f->getReturnType();

    // Create new function type.
    FunctionType* newType = FunctionType::get(oldRetType, paramTypes, false);

    // Create new function declaration.
    Function* cloneF = Function::Create(newType, Function::InternalLinkage, "", f->getParent());
    cloneF->takeName(f);
    cloneF->setCallingConv(f->getCallingConv());
    cloneF->setAlignment(f->getAlignment());
    cloneF->setAttributes(f->getAttributes());

    // Clone old function into new function (mask parameter is unused).
    ValueToValueMapTy valueMap;
    Function::arg_iterator destA = cloneF->arg_begin();
    for (Function::const_arg_iterator A=f->arg_begin(), AE=f->arg_end(); A!=AE; ++A)
    {
        destA->setName(A->getName()); // Copy the name over...
        valueMap[&*A] = &*destA;        // Add mapping to ValueMap
        destA++;
    }

    SmallVector<ReturnInst*, 10> returns;
    CloneFunctionInto(cloneF, f, valueMap, false, returns);

    // Now, create a new call site with a dummy parameter (we don't know the
    // mask value yet).
    assert (isa<CallInst>(*f->use_begin()));
    CallInst* oldCall = cast<CallInst>(*f->use_begin());

    SmallVector<Value*, 4> params;
    for (unsigned i=0, e=oldCall->getNumArgOperands(); i<e; ++i)
    {
        params.push_back(oldCall->getArgOperand(i));
    }
    params.push_back(ConstantInt::getAllOnesValue(int1Ty));

    CallInst* cloneCall = CallInst::Create(cloneF, params, "", oldCall);
    WFV::copyMetadata(cloneCall, *oldCall);

    // Replace the single use.
    oldCall->replaceAllUsesWith(cloneCall);

    // Erase the old function and the old call.
    assert (oldCall->use_empty());
    oldCall->eraseFromParent();
    assert (f->use_empty());
    f->eraseFromParent();

    *newFn = cloneF;
    *newCall = cloneCall;
}

// Add "default" metadata to the instructions and blocks newly introduced to
// the source function during splitting and extraction of the variant.
// These are: blocks introduced during splitting and extraction, corresponding
// branches, the call to the extracted function, and allocas and loads of
// result values.
// NOTE: This relies on metadata in the extracted function still being available.
void
setDefaultMetadata(Function*          sourceFn,
                   CallInst*          call,
                   SetVector<Value*>& outputs)
{
    assert (sourceFn && call);

    // The block of the call is always a new one that should have exactly
    // the properties of its predecessor block.
    BasicBlock* callBB = call->getParent();
    assert (!WFV::hasMetadata(callBB));
    assert (callBB->getUniquePredecessor());
    BasicBlock* lastSrcBB = callBB->getUniquePredecessor();
    assert (WFV::hasMetadata(lastSrcBB));
    WFV::copyMetadata(callBB, *lastSrcBB);

    // The block after the call is always a new one that should have exactly
    // the properties of the call block.
    assert (callBB->getTerminator()->getNumSuccessors() == 1);
    BasicBlock* afterCallBB = callBB->getTerminator()->getSuccessor(0);
    assert (!WFV::hasMetadata(afterCallBB));
    assert (WFV::hasMetadata(callBB));
    WFV::copyMetadata(afterCallBB, *callBB);

    // Every terminator that is inserted during splitting or extraction is OP_UNIFORM.
    for (auto &BB : *sourceFn)
    {
        TerminatorInst* terminator = BB.getTerminator();
        if (WFV::hasMetadata(terminator)) continue;
        WFV::setMetadata(terminator, WFV::WFV_METADATA_OP_UNIFORM);
    }

    // The calls properties by default depend upon the parent function's
    // vectorization analysis.
    const bool isVaryingCall = isVaryingRegionCall(*call);
    WFV::setMetadata(call,
                     isVaryingCall ?
                         WFV::WFV_METADATA_OP_VARYING :
                         WFV::WFV_METADATA_OP_UNIFORM);

    // The result allocas' and loads' properties depend upon the parent
    // function's vectorization analysis.
    // Thus, we have to create a mapping between the original value (the
    // one being stored one in the variant), the alloca, and the load, and
    // give the alloca and the load the same metadata as the value.

    // The outputs of CodeExtractor give us the number of outputs.
    const unsigned numOutputs  = outputs.size();
    const unsigned numArgs     = call->getNumArgOperands();
    const unsigned firstOutIdx = numArgs - numOutputs;

    // Get the parameters of the function for these outputs (= the allocas).
    SmallVector<Value*, 2> allocas;
    for (unsigned i=firstOutIdx; i<numArgs; ++i)
    {
        Value* alloca = call->getArgOperand(i);

        //DEBUG_WFV_NO_VERBOSE( outs() << "  output argument: " << *alloca << "\n"; );
        assert (isa<AllocaInst>(alloca));

        // Get corresponding argument of variant function.
        Function* V = call->getCalledFunction();
        Function::arg_iterator A = V->arg_begin();
        std::advance(A, i);
        Argument* variantArg = &*A;

        // The argument should only have one use that is a store.
        assert (variantArg->getNumUses() == 1);
        assert (isa<StoreInst>(*variantArg->use_begin()));
        StoreInst* storeI = cast<StoreInst>(*variantArg->use_begin());

        // Get the stored value.
        Value* storedVal = storeI->getValueOperand();

        // Set the metadata of the alloca and load according to the
        // properties of the stored value.
        if (isa<Constant>(storedVal))
        {
            WFV::setMetadata(alloca, WFV::WFV_METADATA_OP_UNIFORM);
            WFV::setMetadata(alloca, WFV::WFV_METADATA_RES_UNIFORM);
        }
        else
        {
            assert (WFV::hasMetadata(storedVal));
            assert (!WFV::hasMetadata(storedVal, WFV::WFV_METADATA_RES_SCALARS) &&
                    "not implemented");

            WFV::copyMetadata(alloca, *storedVal);

            // If the alloca is varying, force it to be consecutive/aligned s.t.
            // a vector store is generated later (if the function is not inlined
            // again).
            if (WFV::hasMetadata(alloca, WFV::WFV_METADATA_OP_VARYING))
            {
                WFV::setMetadata(alloca, WFV::WFV_METADATA_INDEX_CONSECUTIVE);
                WFV::setMetadata(alloca, WFV::WFV_METADATA_ALIGNED_TRUE);
            }
        }

        // Find the corresponding reload: All alloca's should have exactly
        // two uses, one for the call, one for the reload.
        assert (alloca->getNumUses() == 2);

        AllocaInst::use_iterator U = alloca->use_begin();
        Value* useVal0 = cast<Value>(*U++);
        Value* useVal1 = cast<Value>(*U);

        // Determine which of the two uses is the call and which the load
        LoadInst* load = nullptr;
        if (isa<CallInst>(useVal0))
        {
            assert (isa<LoadInst>(useVal1));
            load = cast<LoadInst>(useVal1);
        }
        else
        {
            assert (isa<LoadInst>(useVal0));
            load = cast<LoadInst>(useVal0);
        }

        // Give the load the same properties as the stored value.
        WFV::copyMetadata(load, *storedVal);
    }
}

} // unnamed namespace

// Extract variant regions.
// Regions are expected to be marked in the following way:
// - The instruction marked as "START" is the first one of the region.
// - The instruction marked as "END" is the first one *after* the region.
// TODO: What do we do with OP_SEQUENTIAL operands?
bool
WFVInterface::extractTopLevelVariantRegions(Function*                           sourceFn,
                                            SmallVector<Function*, 2>&          regionFns,
                                            SmallVector<SetVector<Value*>*, 2>& inputSets,
                                            SmallVector<SetVector<Value*>*, 2>& outputSets)
{
    assert (sourceFn);
    assert (regionFns.empty());

    //DEBUG_WFV_NO_VERBOSE( outs() << "\nExtracting variant regions of function '" << sourceFn->getName() << "'...\n"; );

    while (true)
    {
        SetVector<BasicBlock*> region;
        Instruction* startInst = findVariantRegion(sourceFn, region);
        if (!startInst) break; // No region found.

        //DEBUG_WFV_NO_VERBOSE(
            //outs() << "  region: ";
            //for (const auto &BB : region) outs() << BB->getName() << " ";
            //outs() << "\n";
        //);

        //
        // Create new function from the marked range.
        //
        SmallVector<BasicBlock*, 4> regionVec(region.begin(), region.end());
        CodeExtractor extractor(regionVec);
        assert (extractor.isEligible());

        //
        // Find the inputs. This has to be done *before* extractCodeRegion(),
        // otherwise they point to the arguments instead of the values in the
        // original function. At the same time, we don't get any outputs here.
        //
        SetVector<Value*>* inputs = new SetVector<Value*>();
        SetVector<Value*>* outputs = new SetVector<Value*>();
        extractor.findInputsOutputs(*inputs, *outputs);
        assert (outputs->empty() && "outputs should only be available after extractCodeRegion()!");
        inputSets.push_back(inputs);

        //DEBUG_WFV_NO_VERBOSE( outs() << "    inputs:\n"; );
        //DEBUG_WFV_NO_VERBOSE( for (const auto &V : *inputs) outs() << "   * " << *V << "\n"; );

        for (unsigned i=0, e=inputs->size(); i<e; ++i)
        {
            Value* input = (*inputs)[i];
            if (isa<Argument>(input)) continue;
            if (WFV::hasMetadata(input, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
                WFV::hasMetadata(input, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                errs() << "ERROR: Extraction of region with OP_SEQUENTIAL "
                    << "input values not implemented yet!\n";
                return false;
            }
        }

        //
        // Extract the code
        //
        Function* R = extractor.extractCodeRegion();
        R->setName(R->getName()+".extracted");

        //
        // Find the outputs (only available after extractCodeRegion()).
        // Unfortunately, this does not give us the allocas, but only the
        // values in the function that are stored back to the respective arguments.
        //
        SetVector<Value*>* inputs2 = new SetVector<Value*>();
        extractor.findInputsOutputs(*inputs2, *outputs);
        outputSets.push_back(outputs);
        delete inputs2;

        //DEBUG_WFV_NO_VERBOSE( outs() << "    outputs:\n"; );
        //DEBUG_WFV_NO_VERBOSE( for (const auto &V : *outputs) outs() << "   * " << *V << "\n"; );

        for (unsigned i=0, e=outputs->size(); i<e; ++i)
        {
            Value* output = (*outputs)[i];
            if (isa<Argument>(output)) continue;
            if (WFV::hasMetadata(output, WFV::WFV_METADATA_OP_SEQUENTIAL) ||
                WFV::hasMetadata(output, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                errs() << "ERROR: Extraction of region with OP_SEQUENTIAL "
                    << "output values not implemented yet!\n";
                return false;
            }
        }
        assert (!outputs->empty() && "There has to be at least one output!");

        assert (R->getNumUses() == 1 &&
                "There should be only one call to an extracted function");
        assert (isa<CallInst>(*R->use_begin()));
        CallInst* call = cast<CallInst>(*R->use_begin());

        // Add "default" metadata to the instructions and blocks newly introduced to
        // the source function during splitting and extraction of the variant.
        // NOTE: This relies on metadata in the extracted function still being available.
        setDefaultMetadata(sourceFn, call, *outputs);

        // Store the instruction that holds the metadata that defines the variant
        // before removing all metadata.
        // This is not very elegant, but does the job.
        Instruction* clonedStartInst = startInst->clone();

        // Remove all metadata introduced by the parent's vectorization analysis from
        // the region function. Every variant is supposed to run the analysis again
        // with possibly changed properties.
        WFV::removeAllMetadata(R);

        // Add metadata to identify the variant again later.
        // However, remove VARIANT_START mark to prevent erroneous recursion.
        // After this, the instruction is the only one in the function with metadata.
        // Note that this may disturb vectorization analysis (i.e. prevent it from
        // running), so the metadata has to be removed later after the variant has
        // been identified (removeMetadataFromStartInst() below).
        // This is not very elegant, but does the job.
        assert (WFV::hasMetadata(clonedStartInst));
        assert (WFV::hasMetadata(clonedStartInst, WFV::WFV_METADATA_VARIANT_START));
        WFV::copyMetadata(startInst, *clonedStartInst);
        WFV::removeMetadata(startInst, WFV::WFV_METADATA_VARIANT_START);
        clonedStartInst->insertBefore(startInst);
        clonedStartInst->eraseFromParent();

        // If required, add another argument to the extracted function for the mask.
        // This is the case for non-uniform calls in an ALWAYS_BY_ALL_FALSE block.
        // NOTE: This erases the old R and call and updates the pointers.
#if 0
        if (isNotAlwaysByAllOrNone(*call->getParent()) &&
            isVaryingRegionCall(*call))
        {
            addMaskArgument(R, &R, &call);
        }
#else
        addMaskArgument(R, &R, &call);
#endif

        //DEBUG_WFV_NO_VERBOSE( outs() << "extracted function: " << *R << "\n"; );

        regionFns.push_back(R);
    }

    //DEBUG_WFV_NO_VERBOSE( outs() << "source function: " << *sourceFn << "\n\n"; );
    return true;
}

namespace {

void
removeMetadataFromStartInst(Function* regionFn)
{
    assert (regionFn);

    BasicBlock* entryBB = &regionFn->getEntryBlock();
    assert (entryBB->getTerminator()->getNumSuccessors() == 1);
    BasicBlock* startBB = entryBB->getTerminator()->getSuccessor(0);

    Instruction* startInst = &*startBB->begin();
    assert (WFV::hasMetadata(startInst));

    WFV::removeAllMetadata(startInst);
}

} // unnamed namespace

// Determine properties of this region.
VariantRegion*
WFVInterface::getVariantRegion(Function*          sourceFn,
                               Function*          regionFn,
                               SetVector<Value*>* inputs,
                               SetVector<Value*>* outputs,
                               WFVInfo*           info)
{
    assert (sourceFn && regionFn && inputs && outputs);
    assert (regionFn->getNumUses() == 1);
    assert (isa<CallInst>(*regionFn->use_begin()));

    // Determine type of region.
    const VariantRegionType& type = getVariantRegionType(*regionFn);

    // After we have determined the type, delete the metadata of the
    // corresponding instruction to prevent disturbance of the vectorization
    // analysis.
    // TODO: This should be done by the variant itself!
    removeMetadataFromStartInst(regionFn);

    VariantRegion* VR = VariantRegionFactory::Get()->CreateVariantRegion(type, regionFn, info);
    assert (VR);
    VR->mSourceFunction = sourceFn;
    VR->mRegionFunction = regionFn;
    VR->mRegionCall = cast<CallInst>(*regionFn->use_begin());
    VR->mInputs = inputs;
    VR->mOutputs = outputs;

    return VR;
}
