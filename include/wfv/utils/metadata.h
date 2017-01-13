/*
 * @file:  metadata.h
 * @date   29.03.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 */

#ifndef METADATA_H
#define	METADATA_H

namespace llvm {
class Module;
class Function;
class BasicBlock;
class Instruction;
class MDNode;
class Loop;
class Argument;
class Value;
class SelectInst;
}

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"

using namespace llvm;

namespace WFV {

//PACXX properties
static const char* const PACXX_ID_X = "idx";
static const char* const PACXX_ID_Y = "idy";
static const char* const PACXX_ID_Z = "idz";
static const char* const PACXX_BLOCK_ID_X = "bidx";
static const char* const PACXX_BLOCK_ID_Y = "bidy";
static const char* const PACXX_BLOCK_ID_Z = "bidz";
static const char* const PACXX_BLOCK_DIM_X = "bdimx";
static const char* const PACXX_BLOCK_DIM_Y = "bdimy";
static const char* const PACXX_BLOCK_DIM_Z = "bdimz";
static const char* const PACXX_BARRIER = "barrier";

// Instruction properties
static const char* const WFV_METADATA_ARGUMENT_CAST         = "wfv_arg_cast";
static const char* const WFV_METADATA_PKT_PTR_CAST          = "wfv_pkt_ptr_cast";
static const char* const WFV_METADATA_BLEND_INFO            = "wfv_blend_info";
static const char* const WFV_METADATA_PACK_UNPACK           = "wfv_pack_unpack";

static const char* const WFV_METADATA_OP_UNIFORM            = "op_uniform";
static const char* const WFV_METADATA_OP_VARYING            = "op_varying";
static const char* const WFV_METADATA_OP_SEQUENTIAL         = "op_sequential";
static const char* const WFV_METADATA_OP_SEQUENTIAL_GUARDED = "op_sequential_guarded";
static const char* const WFV_METADATA_OP_MASKED             = "op_masked";

static const char* const WFV_METADATA_RES_UNIFORM           = "res_uniform"; // unused
static const char* const WFV_METADATA_RES_VECTOR            = "res_vector";
static const char* const WFV_METADATA_RES_SCALARS           = "res_scalars";

static const char* const WFV_METADATA_ALIGNED_TRUE          = "aligned";
static const char* const WFV_METADATA_ALIGNED_FALSE         = "unaligned";

static const char* const WFV_METADATA_INDEX_SAME            = "same";
static const char* const WFV_METADATA_INDEX_CONSECUTIVE     = "consecutive";
static const char* const WFV_METADATA_INDEX_SHUFFLE         = "shuffle";
static const char* const WFV_METADATA_INDEX_STRIDED         = "strided";
static const char* const WFV_METADATA_INDEX_RANDOM          = "random";

static const char* const WFV_METADATA_MASK                  = "mask";

// Block properties

static const char* const WFV_METADATA_MANDATORY             = "mandatory";
static const char* const WFV_METADATA_OPTIONAL              = "optional";

static const char* const WFV_METADATA_DIVERGENT_TRUE        = "divergent";
static const char* const WFV_METADATA_DIVERGENT_FALSE       = "non_divergent";
static const char* const WFV_METADATA_DIVERGENCE_INFO       = "divergence_info";
static const char* const WFV_METADATA_REWIRE_INFO           = "rewire_info";

static const char* const WFV_METADATA_ALWAYS_BY_ALL_TRUE    = "always_by_all";
static const char* const WFV_METADATA_ALWAYS_BY_ALL_OR_NONE = "always_by_all_or_none";
static const char* const WFV_METADATA_ALWAYS_BY_ALL_FALSE   = "not_always_by_all_or_none";

// Loop properties

static const char* const WFV_METADATA_LOOP_DIVERGENT_TRUE   = "loop_divergent";
static const char* const WFV_METADATA_LOOP_DIVERGENT_FALSE  = "loop_non_divergent";

static const char* const WFV_METADATA_LOOP_TOP_LEVEL_DIVERGENT = "loop_top_level_divergent";
static const char* const WFV_METADATA_LOOP_INNERMOST_DIVERGENT = "loop_innermost_divergent";

// Argument properties

static const char* const WFV_METADATA_ARGUMENT_INFO         = "wfv_arg_info";

// Variant properties

static const char* const WFV_METADATA_VARIANT_START          = "variant_start";
static const char* const WFV_METADATA_VARIANT_END            = "variant_end";
static const char* const WFV_METADATA_VARIANT_DISABLE_VECT   = "variant_disable_vect";
static const char* const WFV_METADATA_VARIANT_SEQUENTIALIZE  = "variant_sequentialize";
static const char* const WFV_METADATA_VARIANT_BOSCC          = "variant_boscc";

// Misc

static const char* const WFV_METADATA_ARGUMENT_INFO_FUNCTION_NAME = "wfvMetadataFn";



static Function* argMetadataFn;
static MDNode*   nullMDN;
static DenseMap<Metadata *, Value*> MDValueMap;

Metadata *mapValueToMD(Value *value);
Value *getValueForMD(Metadata *MD);

void setUpMetadata(Module* mod);
bool isMetadataSetUp();

bool isMetadataCall(const Instruction* inst);

bool hasWFVMetadata(const Function& f);

void setMetadata(Loop* loop, const char* const metaDataString);
bool hasMetadata(const Loop* loop);
bool hasMetadata(const Loop* loop, const char* const metaDataString);
void removeMetadata(Loop* loop, const char* const metaDataString);

void setMetadata(Value* value, const char* const metaDataString);
bool hasMetadata(const Value* value);
bool hasWFVMetadata(const Value* value);
bool hasPACXXMetadata(const Value* value);
bool isThreadId(const Value *value);
bool isBlockId(const Value *value);
bool isBlockDim(const Value *value);

bool hasMetadata(const Value* value, const char* const metaDataString);
void removeMetadata(Value* value, const char* const metaDataString);
void copyMetadata(Value* target, const Value& source);

void setMetadataForBlend(SelectInst* select,
                         BasicBlock* incomingBlockTrue,
                         BasicBlock* incomingBlockFalse,
                         const bool  isLastOfCascade);

bool isLastSelectOfBlend(const SelectInst& select);
void getSelectsOfBlend(SelectInst*                  select,
                       SmallPtrSet<SelectInst*, 4>& selects);
BasicBlock* getIncomingBlockTrue(const SelectInst& select);
BasicBlock* getIncomingBlockFalse(const SelectInst& select);

Value* getIncomingValueForBlock(SelectInst* select,
                                const BasicBlock& block);
void replaceBlendUsesOfWith(SelectInst* select,
                            Value*      oldValue,
                            Value*      newValue);

void setMetadataForDivergentBlock(BasicBlock*             block,
                                  SmallVector<Value*, 2>& divergenceCausingBlocks);
void getDivergenceCausingBlocks(const BasicBlock&       block,
                                SmallVector<Value*, 2>& divergenceCausingBlocks);

void addRewireTargetForDCBlock(BasicBlock* divergenceCausingBlock,
                               BasicBlock* rewireTarget);
void getRewireTargetsOfDCBlock(const BasicBlock&       divergenceCausingBlock,
                               SmallVector<Value*, 2>& rewireTargets);

void markMaskOperation(Instruction* maskOp);

const char* getVariantMetadata(const Instruction* inst);

void removeAllMetadata(Instruction* inst);
void removeAllMetadata(Function* f);


// Dummy uses to silence warnings.
namespace {
void dummy() __attribute__((unused));

void dummy()
{
    WFV::argMetadataFn = nullptr;
    WFV::nullMDN = nullptr;
}
}

}

#endif	/* METADATA_H */

