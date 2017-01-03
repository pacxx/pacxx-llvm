/**
 * @file   wfvRegionBased.h
 * @date   11.03.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */
#ifndef _WFVREGIONBASED_H
#define	_WFVREGIONBASED_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SetVector.h"

#include "wfv/variantRegions/variantRegion.h"

// LLVM forward declarations.
namespace llvm {
class Module;
class Argument;
class LLVMContext;
class Function;
class BasicBlock;
class Instruction;
class Value;
class Type;
class TimerGroup;
class CallInst;
}

// Forward declaration of internal implementation.
namespace WFVInterface {
class WFVInterface;
}

using namespace llvm;
using namespace WFV::RegionBased;

namespace WFVInterface {

// Extract variant regions.
// Regions are expected to be marked in the following way:
// - The instruction marked as "START" is the first one of the region.
// - The instruction marked as "END" is the first one *after* the region.
bool extractTopLevelVariantRegions(Function*                           sourceFn,
                                   SmallVector<Function*, 2>&          regionFns,
                                   SmallVector<SetVector<Value*>*, 2>& inputSets,
                                   SmallVector<SetVector<Value*>*, 2>& outputSets);

// Determine properties of this region.
VariantRegion* getVariantRegion(Function*          sourceFn,
                                Function*          regionFn,
                                SetVector<Value*>* inputs,
                                SetVector<Value*>* outputs,
                                WFVInfo*           info);

} // namespace WFVInterface


#endif	/* _WFVREGIONBASED_H */
