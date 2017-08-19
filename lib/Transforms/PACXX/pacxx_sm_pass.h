//
// Created by lars
//

#ifndef LLVM_PACXX_SM_PASS_H
#define LLVM_PACXX_SM_PASS_H

#include "Log.h"

#include "llvm/Pass.h"
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
#include "ModuleHelper.h"

using namespace llvm;
using namespace pacxx;

class PACXXNativeSMTransformer : public ModulePass {

public:
    static char ID;

    PACXXNativeSMTransformer();

    ~PACXXNativeSMTransformer();

    void releaseMemory() override;

    void getAnalysisUsage(AnalysisUsage &AU) const override;

    bool runOnModule(Module &M) override;

    void runOnKernel(Function *kernel);

private:
    struct ConstantUser {
        Instruction *_inst;
        vector<ConstantExpr *> _constants;

        ConstantUser(Instruction *inst, vector<ConstantExpr *> constants) : _inst(inst), _constants(constants) { }
    };

private:

    void createSharedMemoryBuffer(Function *func, Value *sm_size);

    void createInternalSharedMemoryBuffer(Module &M, Function *kernel,
                                          set<GlobalVariable*> &globals, BasicBlock *sharedMemBB);

    void createExternalSharedMemoryBuffer(Module &M, Function *kernel, set<GlobalVariable*> &globals,
                                          Value *sm_size, BasicBlock *sharedMemBB);

    void replaceAllUsesInKernel(Function *kernel, Value *from, Value *with);

    set<GlobalVariable *> getSMGlobalsUsedByKernel(Module *M, Function *func, bool internal);

    vector<ConstantUser> findInstruction(Function *func, ConstantExpr * constExpr);

    void lookAtConstantOps(ConstantExpr *constExp, ConstantExpr *smUser,
                           vector<ConstantExpr *> &tmp,
                           vector<ConstantExpr *> &constants,
                           bool *usesSM);
};

namespace llvm {
    Pass *createPACXXNativeSMPass();
}
#endif //LLVM_PACXX_SM_PASS_H
