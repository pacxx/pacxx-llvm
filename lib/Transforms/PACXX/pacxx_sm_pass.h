//
// Created by lars
//

#ifndef LLVM_PACXX_SM_PASS_H
#define LLVM_PACXX_SM_PASS_H

#include "Log.h"

#include "llvm/Pass.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/ADT/SCCIterator.h"
#include "../../IR/LLVMContextImpl.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

class PACXXNativeSMTransformer : public FunctionPass {

public:
    static char ID;

    PACXXNativeSMTransformer();

    ~PACXXNativeSMTransformer();

    void releaseMemory() override;

    void getAnalysisUsage(AnalysisUsage &AU) const override;

    bool runOnFunction(Function &F) override;

private:

    void createSharedMemoryBuffer(Function *func, Value *sm_size);

    void transformConstExprToInst(Function *func);

    void recursiveTransformConstExprToInst(Instruction *inst, Instruction *insertBefore);

    void createInternalSharedMemoryBuffer(Module &M, set<GlobalVariable*> &globals, Function *wrapper,
                                          BasicBlock *sharedMemBB);

    void createExternalSharedMemoryBuffer(Module &M, set<GlobalVariable*> &globals, Function *wrapper,
                                          Value *sm_size, BasicBlock *sharedMemBB);

    set<GlobalVariable *> getSMGlobalsUsedByKernel(Module *M, Function *func, bool internal);

};

namespace llvm {
    Pass *createPACXXNativeSMPass();
}
#endif //LLVM_PACXX_SM_PASS_H
