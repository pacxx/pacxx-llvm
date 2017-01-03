/**
 * @file   vectorizationPrereqVerifier.h
 * @date   02.06.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */
#ifndef _VECTORIZATIONPREREQVERIFIER_H
#define	_VECTORIZATIONPREREQVERIFIER_H

#include "wfv/wfvInfo.h"
#include "wfv/analysis/sccAnalyzer.h"

#include "llvm/Pass.h"

namespace llvm {
class Module;
class Function;
class BasicBlock;
class Instruction;
class Loop;
class DominatorTree;
}

using namespace llvm;


namespace {

class VectorizationPrereqVerifier : public FunctionPass
{
public:
    static char ID __attribute__((unused)); // Pass identification, replacement for typeid.

    VectorizationPrereqVerifier();
	~VectorizationPrereqVerifier();

    virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    virtual bool doInitialization(Module& M);
    virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
    virtual void print           (raw_ostream& O, const Module* M) const;

private:
    const WFVInfo*     mInfo;
    const LoopInfo*    mLoopInfo;
    const SCCAnalyzer* mSCCAnalyzer;
    DominatorTree*     mDomTree;

    bool verify(const Function& F) const;
    bool verify(const BasicBlock& block) const;
    bool verify(const Instruction& inst) const;
    bool verify(const Loop& loop) const;

    bool containsIrreducibleLoop(const Function& f) const;
};

} // unnamed namespace


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeVectorizationPrereqVerifierPass(PassRegistry&);
FunctionPass* createVectorizationPrereqVerifierPass();
}


#endif	/* _VECTORIZATIONPREREQVERIFIER_H */
