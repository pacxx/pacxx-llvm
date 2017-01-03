/**
 * @file   selectGenerator.h
 * @date   06.06.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */
#ifndef _SELECTGENERATOR_H
#define	_SELECTGENERATOR_H

#include "llvm/Pass.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

class WFVInfo;
class MaskAnalysis;
class LoopLiveValueAnalysis;

namespace llvm {
class Value;
class Instruction;
class Module;
class Function;
class SelectInst;
class PHINode;
class Loop;
class LoopInfo;
}

using namespace llvm;


namespace {

class SelectGenerator : public FunctionPass
{
public:
    static char ID __attribute__((unused)); // Pass identification, replacement for typeid.

    SelectGenerator();
	~SelectGenerator();

    virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    virtual bool doInitialization(Module& M);
    virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
    virtual void print           (raw_ostream& O, const Module* M) const;

private:
    const WFVInfo*         mInfo;
    const LoopInfo*        mLoopInfo;
    const MaskAnalysis*    mMaskAnalysis;
    LoopLiveValueAnalysis* mLoopLiveValueAnalysis;

   	bool generatePhiSelects(Function* f);
    Value* generateSelectFromPhi(PHINode* phi);

    bool generateLoopSelects(Function* f);

    void generateMultipleExitLoopSelects(Loop*                        loop,
                                         SmallPtrSet<SelectInst*, 8>& selectSet);
    bool hasLiveValueResult(const Loop* loop, const Instruction* liveValue) const;

    void replaceDirectParentLoopUsesOfWith(Value* oldValue, Value* newValue, Loop* loop);
    void replaceNonLoopUsesOfWith(Value* oldValue, Value* newValue, Loop* loop);
    bool isContainedInSomeParentLoop(BasicBlock* block, Loop* loop);
};

} // unnamed namespace


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeSelectGeneratorPass(PassRegistry&);
FunctionPass* createSelectGeneratorPass();
}


#endif	/* _SELECTGENERATOR_H */
