//
// Created by lars
/// Based on the paper Computing Liveness Sets for SSA-Form Programs by Florian Brandner,
// Benoit Boissinot, Alain Darte, Benoit Dupont de Dinechin, Fabrice Rastelloss

#ifndef LLVM_PACXX_LIVENESS_ANALYSIS_H
#define LLVM_PACXX_LIVENESS_ANALYSIS_H

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
#include "llvm/IR/Dominators.h"
#include "../../IR/LLVMContextImpl.h"
#include "ModuleHelper.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

class PACXXNativeLivenessAnalyzer : public FunctionPass {

public:
    static char ID;

    PACXXNativeLivenessAnalyzer();

    ~PACXXNativeLivenessAnalyzer();

    void releaseMemory() override;

    void getAnalysisUsage(AnalysisUsage &AU) const override;

    bool runOnFunction(Function &F) override;

    set<Value *> getLivingInValuesForBlock(const BasicBlock* block);

private:

    void computeLiveSets(Function &F);

    void getPhiUses(BasicBlock *current, set<BasicBlock *> &visited, set<Use *> &uses, BasicBlock *orig);

    set<Value *> getPhiDefs(BasicBlock *BB);

    void upAndMark(BasicBlock *BB, Use *value);

    string toString(map<const BasicBlock *, set<Value *>> &map);
    string toString(set<Value *> &set);

private:

    map<const BasicBlock *, set<Value *>> _in;
    map<const BasicBlock *, set<Value *>> _out;
};

namespace llvm {
    Pass* createPACXXLivenessAnalyzerPass();
}
#endif //LLVM_PACXX_LIVENESS_ANALYSIS_H
