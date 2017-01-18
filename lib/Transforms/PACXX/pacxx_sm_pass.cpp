//
// Created by lars

#include "pacxx_sm_pass.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace llvm {
    void initializePACXXNativeSMTransformerPass(PassRegistry&);
}

PACXXNativeSMTransformer::PACXXNativeSMTransformer() : FunctionPass(ID) {
    initializePACXXNativeSMTransformerPass(*PassRegistry::getPassRegistry());
}

PACXXNativeSMTransformer::~PACXXNativeSMTransformer() {}

void PACXXNativeSMTransformer::releaseMemory() {}

void PACXXNativeSMTransformer::getAnalysisUsage(AnalysisUsage &AU) const {}


bool PACXXNativeSMTransformer::runOnFunction(Function &F) {

    auto argIt = F.arg_end();

    // the sm_size is always the second last arg
    Value *sm_size = &*(--(--argIt));

    createSharedMemoryBuffer(&F, sm_size);

    return true;
}

void PACXXNativeSMTransformer::createSharedMemoryBuffer(Function *func, Value *sm_size) {

    Module *M = func->getParent();

    auto internal_sm = getSMGlobalsUsedByKernel(M, func, true);
    auto external_sm = getSMGlobalsUsedByKernel(M, func, false);

    BasicBlock *entry = &func->front();
    BasicBlock *sharedMemBB = BasicBlock::Create(func->getContext(), "shared mem", func, entry);

    if(!internal_sm.empty()) {
        __verbose("internal shared memory found\n");
    }

    if(!external_sm.empty()) {
        __verbose("external shared memory found\n");
    }

    BranchInst::Create(entry, sharedMemBB);

    __verbose("created shared memory");
}

set<GlobalVariable *> PACXXNativeSMTransformer::getSMGlobalsUsedByKernel(Module *M, Function *func, bool internal) {
    set<GlobalVariable *> sm;
    for (auto &GV : M->globals()) {
        if (internal ? GV.hasInternalLinkage() : GV.hasExternalLinkage() && GV.getType()->getAddressSpace() == 3) {
            for (User *GVUsers : GV.users()) {
                if (Instruction *Inst = dyn_cast<Instruction>(GVUsers)) {
                    if (Inst->getParent()->getParent() == func) {
                        sm.insert(&GV);
                    }
                }

                if(ConstantExpr *constExpr = dyn_cast<ConstantExpr>(GVUsers)) {

                }
            }
        }
    }
    return sm;
}

void PACXXNativeSMTransformer::transformConstExprToInst(Function *func) {

}


namespace llvm {
    Pass* createPACXXNativeSMPass() { return new PACXXNativeSMTransformer(); }
}

char PACXXNativeSMTransformer::ID = 0;

INITIALIZE_PASS_BEGIN(PACXXNativeSMTransformer, "native-sm", "creation of shared memory", true, true)
INITIALIZE_PASS_END(PACXXNativeSMTransformer, "native-sm", "creation of shared memory", true, true)


