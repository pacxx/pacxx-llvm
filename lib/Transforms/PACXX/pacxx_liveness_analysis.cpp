//
// Created by lars
// Based on the paper Computing Liveness Sets for SSA-Form Programs by Florian Brandner,
// Benoit Boissinot, Alain Darte, Benoit Dupont de Dinechin, Fabrice Rastelloss

#include "pacxx_liveness_analysis.h"

using namespace llvm;
using namespace pacxx;

namespace llvm {
void initializePACXXNativeLivenessAnalyzerPass(PassRegistry &);
}

PACXXNativeLivenessAnalyzer::PACXXNativeLivenessAnalyzer() : FunctionPass(ID) {
  initializePACXXNativeLivenessAnalyzerPass(*PassRegistry::getPassRegistry());
}

PACXXNativeLivenessAnalyzer::~PACXXNativeLivenessAnalyzer() {}

void PACXXNativeLivenessAnalyzer::releaseMemory() {}

void PACXXNativeLivenessAnalyzer::getAnalysisUsage(AnalysisUsage &AU) const {}

bool PACXXNativeLivenessAnalyzer::runOnFunction(Function &F) {

  computeLiveSets(F);

  return false;
}

void PACXXNativeLivenessAnalyzer::computeLiveSets(Function &F) {

  for (auto &BB : F) {
    set<Use *> phiUses;
    set<BasicBlock *> visitedBlocks;
    getPhiUses(&BB, visitedBlocks, phiUses, &BB);

    for (auto use : phiUses) {
      _out[&BB].insert(use->get());
      upAndMark(&BB, use);
    }

    for (auto &I : BB) {
      if (!isa<PHINode>(I))
        for (auto &op : I.operands()) {
          if (isa<Instruction>(op))
            upAndMark(&BB, &op);
        }
    }
  }
}

void PACXXNativeLivenessAnalyzer::upAndMark(BasicBlock *BB, Use *use) {

  Value *useValue = use->get();
  if (Instruction *inst = dyn_cast<Instruction>(useValue)) {
    if (!isa<PHINode>(inst))
      if (inst->getParent() == BB)
        return;
  }

  if (_in[BB].count(useValue) > 0)
    return;
  _in[BB].insert(useValue);

  if (getPhiDefs(BB).count(useValue) > 0)
    return;

  for (auto pred : predecessors(BB)) {
    _out[pred].insert(useValue);
    upAndMark(pred, use);
  }
}

void PACXXNativeLivenessAnalyzer::getPhiUses(BasicBlock *current,
                                             set<BasicBlock *> &visited,
                                             set<Use *> &uses,
                                             BasicBlock *orig) {

  if (visited.find(current) != visited.end())
    return;
  visited.insert(current);

  for (auto succ : successors(current)) { // find PHINodes of successors
    for (auto &phi : succ->phis())
      for (auto &use : phi.incoming_values())
        if (phi.getIncomingBlock(use) == orig && isa<Instruction>(use.get()))
          uses.insert(&use);
    //recurse
    getPhiUses(succ, visited, uses, orig);
  }
}

set<Value *> PACXXNativeLivenessAnalyzer::getPhiDefs(BasicBlock *BB) {
  set<Value *> uses;
  for (auto &phi : BB->phis())
    uses.insert(&phi);
  return uses;
}

set<Value *> PACXXNativeLivenessAnalyzer::getLivingInValuesForBlock(const BasicBlock *block) {
  return _in[block];
}

namespace llvm {
Pass *createPACXXLivenessAnalyzerPass() { return new PACXXNativeLivenessAnalyzer(); }
}

string PACXXNativeLivenessAnalyzer::toString(map<const BasicBlock *, set<Value *>> &map) {
  string text;
  raw_string_ostream ss(text);

  for (auto elem : map) {
    ss << elem.first->getName() << " : \n";
    ss << toString(elem.second);
    ss << "\n\n";
  }

  return ss.str();
}

string PACXXNativeLivenessAnalyzer::toString(set<Value *> &set) {
  string text;
  raw_string_ostream ss(text);

  for (auto val : set) {
    val->print(ss, true);
    ss << "\n";
  }

  return ss.str();
}

char PACXXNativeLivenessAnalyzer::ID = 0;

INITIALIZE_PASS_BEGIN(PACXXNativeLivenessAnalyzer, "native-liveness", "Liveness Analysis", true, true)
INITIALIZE_PASS_END(PACXXNativeLivenessAnalyzer, "native-liveness", "Liveness Analysis", true, true)


