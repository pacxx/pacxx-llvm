/**
 * @file   maskGenerator.h
 * @date   06.06.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */
#ifndef _MASKGENERATOR_H
#define	_MASKGENERATOR_H

#include "wfv/wfvInfo.h"
#include "wfv/analysis/maskAnalysis.h"

#include "llvm/Pass.h"

using namespace llvm;


namespace {

class MaskGenerator : public FunctionPass
{
public:
    static char ID __attribute__((unused)); // Pass identification, replacement for typeid.

    MaskGenerator();
	~MaskGenerator();

    virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    virtual bool doInitialization(Module& M);
    virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
    virtual void print           (raw_ostream& O, const Module* M) const;

private:
    WFVInfo*        mInfo;
    MaskAnalysis*   mMaskAnalysis;
    const LoopInfo* mLoopInfo;

    void materializeMasks(Function* f);
    bool entryMaskIsUsed(const BasicBlock& block) const;
    Value* materializeMask(MaskPtr maskPtr);

    Value* createNeg(Value* operand, Instruction* insertBefore);
    Value* createAnd(Value* operand0, Value* operand1, Instruction* insertBefore);
    Value* createOr (Value* operand0, Value* operand1, Instruction* insertBefore);
    Value* createSelect(Value*       operand0,
                        Value*       operand1,
                        Value*       operand2,
                        Instruction* insertPoint);
    Value* createPhi(Mask& mask, const Twine& name);

    bool hasNonUniformPhi(const BasicBlock& block) const;
    bool isHeaderOfDivergentLoop(const BasicBlock& block) const;

    void materializeLoopExitMasks(Loop* loop);
    void materializeCombinedLoopExitMasks(Loop* loop);
};

} // unnamed namespace


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeMaskGeneratorPass(PassRegistry&);
FunctionPass* createMaskGeneratorPass();
}


#endif	/* _MASKGENERATOR_H */
