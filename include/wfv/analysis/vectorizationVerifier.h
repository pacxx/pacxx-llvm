/**
 * @file   vectorizationVerifier.h
 * @date   02.06.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */
#ifndef _VECTORIZATIONVERIFIER_H
#define	_VECTORIZATIONVERIFIER_H

#include "wfv/wfvInfo.h"

#include "llvm/Pass.h"

namespace llvm {
class Module;
class Function;
class BasicBlock;
class Instruction;
class Loop;
}

using namespace llvm;


namespace {

class VectorizationVerifier : public FunctionPass
{
public:
    static char ID __attribute__((unused)); // Pass identification, replacement for typeid.

    VectorizationVerifier();
	~VectorizationVerifier();

    virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    virtual bool doInitialization(Module& M);
    virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
    virtual void print           (raw_ostream& O, const Module* M) const;

private:
    const WFVInfo*  mInfo;
    const LoopInfo* mLoopInfo;

    bool verify(const Function& F) const;
    bool verify(const BasicBlock& block) const;
    bool verify(const Instruction& inst, unsigned& vecInstCounter) const;
    bool verify(const Loop& loop) const;
};

} // unnamed namespace


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeVectorizationVerifierPass(PassRegistry&);
FunctionPass* createVectorizationVerifierPass();
}


#endif	/* _VECTORIZATIONVERIFIER_H */
