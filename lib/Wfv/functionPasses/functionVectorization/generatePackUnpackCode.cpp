/**
 * @file   generatePackUnpackCode.cpp
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

#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstIterator.h"

#include <stdexcept>

using namespace llvm;


// Replace "packW" and "unpackW" intrinsics by insert/extract operations and
// update the uses accordingly.
bool
FunctionVectorizer::generatePackUnpackCode(Function*      f,
                                           const WFVInfo& info)
{
    assert (f);

    SmallVector<CallInst*, 16> eraseVec;

    for (auto &BB : *f)
    {
        Instruction* allocPos = &*BB.getFirstInsertionPt();
        for (auto &I : BB)
        {
            Instruction* inst = &I;

            if (isUnpackWFunctionCall(inst))
            {
                if(info.mVerbose) {
                    outs() << "generateUnpackCode(" << *inst << " )\n";
                }

                CallInst* unpackCall = cast<CallInst>(inst);

                Value* value    = unpackCall->getArgOperand(0);
                Value* indexVal = unpackCall->getArgOperand(1);

                // Extract scalar values.
                Value* extract = generateHorizontalExtract(value,
                                                           indexVal,
                                                           unpackCall->getName(),
                                                           allocPos,
                                                           unpackCall,
                                                           info);
                if(!extract)
                    return false;

                // If the type only matches structurally, create an additional bitcast.
                Type* oldType = unpackCall->getType();
                Type* newType = extract->getType();
                if (oldType != newType)
                {
                    assert (newType->canLosslesslyBitCastTo(oldType) || WFV::typesMatch(oldType, newType));
                    Instruction* bc = new BitCastInst(extract, oldType, "", unpackCall);

                    // Copy properties from unpackCall.
                    WFV::copyMetadata(bc, *unpackCall);
                    extract = bc;
                }

                // Rewire the use.
                assert (unpackCall->getNumUses() == 1);
                Value* use = *unpackCall->user_begin();
                assert (isa<Instruction>(use));
                Instruction* scalarUse = cast<Instruction>(use);

                scalarUse->replaceUsesOfWith(unpackCall, extract);

                // Erase now unused unpack call.
                eraseVec.push_back(unpackCall);

                // If the returned extract operation is an alloca, we have to
                // make sure that all changes to that memory location are
                // correctly written back to the original memory from which
                // the sub-element was extracted.
                // This means we have to insert merge and store operations
                // after every use of this value (including "forwarded" uses
                // via casts, phis, and GEPs).
                // However, we must only merge back those values that were
                // modified. This is not only for efficiency, but also for
                // correctness, since there may be uninitialized pointers in
                // a structure, which we must not load/store from/to (see
                // test_struct_extra05 with all analyses disabled).
                if (isa<AllocaInst>(extract) ||
                    (isa<BitCastInst>(extract) &&
                     isa<AllocaInst>(cast<BitCastInst>(extract)->getOperand(0))))
                {
                    generateWriteBackOperations(cast<Instruction>(extract),
                                                cast<Instruction>(extract),
                                                value,
                                                indexVal,
                                                info);
                }
            }
            else if (isPackWFunctionCall(inst))
            {
                if(info.mVerbose) outs() << "generatePackCode(" << *inst << " )\n";

                CallInst* packCall = cast<CallInst>(inst);

                assert (WFV::isVectorizedType(*packCall->getType()) &&
                        "packCall should have vector return type after inst vectorization!");

                SmallVector<Value*, 8> scalarVals(info.mVectorizationFactor);

                // Get scalar results for merge.
                for (unsigned i=0; i<info.mVectorizationFactor; ++i)
                {
                    scalarVals[i] = packCall->getArgOperand(i);
                }

                // Merge scalar results.
                Instruction* merge = generateHorizontalMerge(scalarVals,
                                                             packCall->getType(),
                                                             "",
                                                             packCall,
                                                             info);
                if(!merge)
                    return false;

                // Rewire the uses.
                packCall->replaceAllUsesWith(merge);

                // Copy properties from packCall.
                WFV::copyMetadata(merge, *packCall);

                // Erase now unused pack call.
                eraseVec.push_back(packCall);
            }
        }
    }

    for (auto &call : eraseVec)
    {
        assert (call->use_empty());
        call->eraseFromParent();
    }
    return true;
}
