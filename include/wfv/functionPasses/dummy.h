/**
 * @file   dummy.h
 * @date   28.03.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */
#ifndef _DUMMY_H
#define	_DUMMY_H

#include "llvm/Pass.h"

class WFVInfo;

using namespace llvm;


namespace {

class Dummy : public FunctionPass
{
public:
    static char ID; // Pass identification, replacement for typeid.

    Dummy();
    ~Dummy();

    virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    virtual bool doInitialization(Module& M);
    virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
    virtual void print           (raw_ostream& O, const Module* M) const;

private:
    const WFVInfo* mInfo;
};

} // unnamed namespace


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeDummyPass(PassRegistry&);
FunctionPass* createDummyPass();
}


#endif	/* _DUMMY_H */
