/**
 * @file   loopExitCanonicalizer.h
 * @date   23.10.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */
#ifndef _LOOPEXITCANONICALIZER_H
#define	_LOOPEXITCANONICALIZER_H

#include "llvm/Pass.h"

namespace llvm {
class LoopInfo;
class Loop;
}

class WFVInfo;

using namespace llvm;


namespace {

class LoopExitCanonicalizer : public FunctionPass
{
public:
    static char ID; // Pass identification, replacement for typeid.

    LoopExitCanonicalizer();
    ~LoopExitCanonicalizer();

    virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    virtual bool doInitialization(Module& M);
    virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
    virtual void print           (raw_ostream& O, const Module* M) const;

private:
    const WFVInfo* mInfo;
    LoopInfo*      mLoopInfo;

    bool canonicalizeLoop(Loop* loop) const;
    BasicBlock* createIntermediateBlock(BasicBlock* source,
                                        BasicBlock* target) const;
    void adjustPhis(BasicBlock* source,
                    BasicBlock* target,
                    BasicBlock* newTarget) const;
    void replaceTarget(BasicBlock* source,
                       BasicBlock* target,
                       BasicBlock* newTarget) const;
};

} // unnamed namespace


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeLoopExitCanonicalizerPass(PassRegistry&);
FunctionPass* createLoopExitCanonicalizerPass();
}


#endif	/* _LOOPEXITCANONICALIZER_H */
