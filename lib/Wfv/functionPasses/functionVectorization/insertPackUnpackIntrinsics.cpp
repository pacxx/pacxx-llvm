/**
 * @file   insertPackUnpackIntrinsics.cpp
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

#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstIterator.h"

#include <sstream>
#include <stdexcept>

using namespace llvm;

namespace {

void
redirectOperand(Value*       operand,
                Instruction* inst,
                Function*    unpackFn)
{
    assert (operand && inst && unpackFn);
    assert (isa<Argument>(operand) || isa<Instruction>(operand));

    CallInst* unpackCall = CallInst::Create(unpackFn,
                                            ArrayRef<Value*>(operand),
                                            "unpack",
                                            inst);

    unpackCall->setTailCall();
    unpackCall->setDoesNotAccessMemory();
    unpackCall->setDoesNotThrow();

    inst->replaceUsesOfWith(operand, unpackCall);

    // Copy properties from inst, but overwrite to OP_UNIFORM/RES_UNIFORM.
    WFV::copyMetadata(unpackCall, *inst);
    WFV::setMetadata(unpackCall, WFV::WFV_METADATA_OP_UNIFORM);
    WFV::setMetadata(unpackCall, WFV::WFV_METADATA_RES_UNIFORM);
}

}


// For every instruction that is OP_SEQUENTIAL or OP_SEQUENTIAL_GUARDED:
// - Redirect every operand that is not RES_UNIFORM to an "unpack" intrinsic.
// - Redirect every use to a "pack" intrinsic.
void
FunctionVectorizer::insertPackUnpackIntrinsics(Function* f)
{
    assert (f);
    assert (f->getParent());
    Module* mod = f->getParent();

    for (inst_iterator I=inst_begin(f), E=inst_end(f); I!=E; )
    {
        Instruction* inst = &*I++;

        if (!WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL) &&
            !WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED))
        {
            continue;
        }

        if(WFV::hasPACXXMetadata(inst)) continue;

        //ignore masked loads and stores handled by masked load and stores later
        if(WFV::hasMetadata(inst, WFV::WFV_METADATA_OP_MASKED))
            continue;

        // Check for which operands we have to introduce "unpack" operations.
        for (Instruction::op_iterator OP=inst->op_begin(), OPE=inst->op_end(); OP!=OPE; ++OP)
        {
            assert (isa<Value>(OP));
            Value* opVal = cast<Value>(*OP);

            // Ignore operands that are uniform (only arguments and instructions can
            // be non-uniform).
            if (!isa<Argument>(opVal) && !isa<Instruction>(opVal)) continue;
            if (WFV::hasMetadata(opVal, WFV::WFV_METADATA_RES_UNIFORM)) continue;

            // Ignore operands that are RES_SCALARS (they will be replaced by
            // a 'forward' intrinsic and thus don't require an additional 'unpack').
            if (WFV::hasMetadata(opVal, WFV::WFV_METADATA_RES_SCALARS)) continue;

            // If this is a GEP, ignore the pointer operand if it is non-nested (if
            // it requires splitting, this will be handled by introducing additional
            // GEP indices).
            if (isa<GetElementPtrInst>(inst) &&
                cast<GetElementPtrInst>(inst)->getPointerOperand() == opVal &&
                !WFV::hasNestedPointer(*opVal->getType()))
            {
                continue;
            }

            if(mInfo->mVerbose) {
                outs() << "generating 'unpack':\n";
                outs() << "  operand    : " << *opVal << "\n";
                outs() << "  instruction: " << *inst << "\n";
            }

            // Arguments can be of vector type already, in which case we must not
            // use the same type for return and parameter type since the return
            // type of the unpack operation must be scalar.
            Type* paramType  = opVal->getType();
            Type* returnType = WFV::isVectorizedType(*paramType) ?
                WFV::getScalarFromVectorizedType(paramType) :
                opVal->getType();

            assert (!WFV::isVectorizedType(*paramType) ||
                    isa<Argument>(opVal) ||
                    isArgCast(*opVal));

            Function* unpackFn = getSpecialWFVFunction(WFV_FUNCTION_NAME_UNPACK,
                                                       returnType,
                                                       ArrayRef<Type*>(paramType),
                                                       mod);

            redirectOperand(opVal, inst, unpackFn);
        }

        // If the instruction has no return value, there are no
        // uses we could redirect to 'pack' nodes.
        if (inst->getType()->isVoidTy()) continue;

        // Create a single call to the 'pack' intrinsic if the instruction is RES_VECTOR
        const bool isResVector = WFV::hasMetadata(inst, WFV::WFV_METADATA_RES_VECTOR);
        CallInst* packCall = nullptr;
        if (isResVector)
        {
            Function* packFn = getSpecialWFVFunction(WFV_FUNCTION_NAME_PACK,
                                                    inst->getType(),
                                                    ArrayRef<Type*>(inst->getType()),
                                                    mod);

            packCall = CallInst::Create(packFn,
                                        ArrayRef<Value*>(inst),
                                        "pack",
                                        inst);

            packCall->setTailCall();
            packCall->setDoesNotAccessMemory();
            packCall->setDoesNotThrow();

            // Copy properties from inst, but overwrite to OP_VARYING/RES_VECTOR.
            WFV::copyMetadata(packCall, *inst);
            WFV::setMetadata(packCall, WFV::WFV_METADATA_OP_VARYING);
            WFV::setMetadata(packCall, WFV::WFV_METADATA_RES_VECTOR);

            // Move inst before packCall again.
            inst->moveBefore(packCall);
        }

        // If a use of the instruction is also OP_SEQUENTIAL, we must not
        // create a pack call (creating a vector of this type might be illegal).
        // Thus, we directly "optimize" chains of sequential operations by not
        // generating pack-unpack combinations but instead creating a special
        // "forward" intrinsic. This intrinsic is replaced during duplication
        // of instructions by another intrinsic that takes W arguments but still
        // returns one value (which is still used by all uses).
        // After duplication, it is removed and uses are rewired as required.

        // Create a single call to the 'forward' intrinsic (might not be used if
        // there is no use that is OP_SEQUENTIAL).
        Function* forwardFn = getSpecialWFVFunction(WFV_FUNCTION_NAME_FORWARD,
                                                    inst->getType(),
                                                    ArrayRef<Type*>(inst->getType()),
                                                    mod);

        CallInst* forwardCall = CallInst::Create(forwardFn,
                                                 ArrayRef<Value*>(inst),
                                                 "forward",
                                                 inst);

        forwardCall->setTailCall();
        forwardCall->setDoesNotAccessMemory();
        forwardCall->setDoesNotThrow();

        // Copy properties from inst, but overwrite to OP_SEQUENTIAL/RES_SCALARS.
        WFV::copyMetadata(forwardCall, *inst);
        WFV::setMetadata(forwardCall, WFV::WFV_METADATA_OP_SEQUENTIAL);
        WFV::setMetadata(forwardCall, WFV::WFV_METADATA_RES_SCALARS);

        // Move inst before forwardCall again.
        inst->moveBefore(forwardCall);

        // Now replace uses (if any).
        DenseMap<Instruction*, std::pair<Instruction*, Value*> > replaceMap;
        for (Instruction::user_iterator U=inst->user_begin(), UE=inst->user_end(); U!=UE; )
        {
            assert (isa<Instruction>(*U));
            Instruction* useI = cast<Instruction>(*U++);

            if (isPackFunctionCall(useI)) continue;
            if (isForwardFunctionCall(useI)) continue;

            // If the use instruction is not OP_SEQUENTIAL and the current
            // instruction has a vectorizable return type, simply replace the
            // use of 'inst' by 'packCall'.
            if (!WFV::hasMetadata(useI, WFV::WFV_METADATA_OP_SEQUENTIAL) &&
                !WFV::hasMetadata(useI, WFV::WFV_METADATA_OP_SEQUENTIAL_GUARDED) &&
                isResVector)
            {
                assert (packCall);
                if(mInfo->mVerbose) {
                    outs() << "generating 'pack':\n";
                    outs() << "  instruction: " << *inst << "\n";
                    outs() << "  use        : " << *useI << "\n";
                }
                //useI->replaceUsesOfWith(inst, packCall);
                replaceMap[useI] = std::make_pair(inst, packCall);
                continue;
            }

            // Otherwise, replace use of 'inst' by 'forwardCall'.
            if(mInfo->mVerbose) {
                outs() << "generating 'forward':\n";
                outs() << "  instruction: " << *inst << "\n";
                outs() << "  use        : " << *useI << "\n";
            }
            //useI->replaceUsesOfWith(inst, forwardCall);
            replaceMap[useI] = std::make_pair(inst, forwardCall);
        }

        for (auto pair : replaceMap)
        {
            Instruction* useI = pair.first;
            Instruction* inst = pair.second.first;
            Value* replaceVal = pair.second.second;
            useI->replaceUsesOfWith(inst, replaceVal);
        }

        // If the pack call has no use, remove it, unless it is a mask (which
        // can be required later if guards are inserted).
        if (!WFV::hasMetadata(inst, WFV::WFV_METADATA_MASK) && packCall)
        {
            assert ((packCall->use_empty() || WFV::isVectorizableType(*packCall->getType())) &&
                    "must not create 'pack' intrinsic of type without vector equivalent!");
            if (packCall->user_empty()) packCall->eraseFromParent();
        }
        // If the forward call has no use, remove it.
        if (forwardCall->user_empty()) forwardCall->eraseFromParent();
    }
}
