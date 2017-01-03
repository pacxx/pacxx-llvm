/**
 * @file   utils.h
 * @date   19.03.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */
#ifndef _VARIANTREGIONUTILS_H
#define	_VARIANTREGIONUTILS_H

// LLVM forward declarations.
namespace llvm {
class Function;
class BasicBlock;
class CallInst;
}

// WFV forward declarations.
class WFVInfo;

using namespace llvm;

namespace WFV {
namespace RegionBased {

// WFV::RegionBased forward declarations.
struct VariantProperties;

bool isNotAlwaysByAllOrNone(const BasicBlock& block);

// Determines whether 'call' has at least one non-uniform argument.
// NOTE: This relies on WFV metadata being available for all arguments
//       of 'call' except the result pointers.
bool isVaryingRegionCall(const CallInst& call);

bool regionMayHaveSideEffects(const Function& regionFn, const WFVInfo& info);

// Create SIMD target declaration for this variant.
Function* createSIMDTargetDecl(Function*                V,
                               const VariantProperties& VP);

} // namespace RegionBased
} // namespace WFV


#endif	/* _VARIANTREGIONUTILS_H */
