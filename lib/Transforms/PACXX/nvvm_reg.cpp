/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2014
*/

//#include <iostream>
#include <vector>
#include <cassert>
#include <algorithm>

#define DEBUG_TYPE "nvvm_reg"

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/CFG.h"

#include "ModuleHelper.h"

#define GLOBAL_ID_PATTERN "mad.lo.u32 $0, $1, $2, $3;"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {

struct NVVMRegPass : public ModulePass {
  static char ID;
  NVVMRegPass(bool runtime = false) : ModulePass(ID), runtime(runtime) {}
  virtual ~NVVMRegPass() {}

  virtual bool runOnModule(Module &M) {
    bool modified = true;

    auto kernels = getTagedFunctions(&M, "nvvm.annotations", "kernel");

    auto replaceSubstring = [](string Str, const StringRef &From,
                               const StringRef &To) {
      size_t Pos = 0;
      while ((Pos = Str.find(From, Pos)) != std::string::npos) {
        Str.replace(Pos, From.size(), To.data(), To.size());
        Pos += To.size();
      }
	  return Str;
    };

    for (auto &GV : M.getGlobalList()) {
      if (GV.getType()->isPointerTy() && GV.getType()->getAddressSpace() == 3) {
        auto newName = replaceSubstring(GV.getName(), ".", "_");
        GV.setName(newName);
      }
    }

    BaseOpts opt(&M);
    for (auto &F : M.getFunctionList()) {
      opt.initialize(F);
      opt.visit(F);
      opt.finalize();
    }

    if (!runtime) {
      vector<unsigned> inst_types;
      //  inst_types.push_back(Instruction::Load);
      // inst_types.push_back(Instruction::GetElementPtr);
      // inst_types.push_back(Instruction::BitCast);

      InstScheduler shed;
      for (auto op : inst_types) {
        for (auto F : kernels) {
          shed.initialize(F);
          shed.setValueId(op);
          shed.visit(F);
          shed.finalize(F);
        }
      }
    }

    SharedMemScheduler sms{runtime};

    for (auto F : kernels) {
      sms.visit(F);
      sms.finalize(F);
    }

    //CastStripper cs;
    //for (auto f : kernels) {
    //  cs.visit(f);
    //  cs.finalize();
    //}

    vector<pair<BasicBlock *, BasicBlock *>> merge;

    // for (auto f : kernels) {
    //  vector<pair<BasicBlock *, int>> BBs;
    //  for (auto &BB : f->getBasicBlockList()) {

    //    int pred = 0;
    //    for (auto PI = pred_begin(&BB), E = pred_end(&BB); PI != E; ++PI) {
    //      ++pred;
    //    }
    //    if (pred == 1) {
    //      bool mergeable = true;
    //      bool merge_left = true;
    //      if (auto BRI = dyn_cast<BranchInst>(BB.getTerminator())) {
    //        if (BRI->isConditional()) {
    //          if (auto cond = dyn_cast<ConstantInt>(BRI->getCondition())) {
    //            merge_left = cond->isZero();
    //          } else
    //            mergeable = false;
    //        }
    //        if (mergeable) {

    //          int spred = 0;
    //          BasicBlock *succ = nullptr;
    //          if (merge_left)
    //            succ = BRI->getSuccessor(0);
    //          else
    //            succ = BRI->getSuccessor(1);

    //          for (auto PI = pred_begin(succ), E = pred_end(succ); PI != E;
    //               ++PI) {
    //            ++spred;
    //          }
    //          if (spred == 1)
    //            merge.push_back(make_pair(&BB, succ));
    //        }
    //      }
    //    }
    //    if (&BB == &f->getBasicBlockList().front())
    //      pred = -1;
    //    BBs.push_back(make_pair(&BB, pred));
    //  }
    //  __debug("deletable BasicBlocks: ", BBs.size());
    //  for (auto p : BBs) {
    //    if (p.second == 0) {
    //      for (auto u : p.first->users()) {
    //        if (auto phi = dyn_cast<PHINode>(u)) {
    //          phi->removeIncomingValue(p.first);
    //        }
    //      }
    //      auto c = std::find_if(merge.begin(), merge.end(),
    //                            [&](auto x) { return x.first == p.first; });
    //      if (c != merge.end()) {
    //        c->first = nullptr;
    //        c->second = nullptr;
    //      }

    //      p.first->eraseFromParent();
    //    }
    //  }
    //}
    //__debug("mergeable BasicBlocks: ", merge.size());
    // for (auto p : merge) {
    //  if (p.first) {
    //    auto term = p.first->getTerminator();
    //    vector<Instruction *> tmp;
    //    p.first->dump();
    //    p.second->dump();
    //    for (auto &I : p.second->getInstList()) {
    //      auto clonedI = I.clone();
    //      clonedI->insertBefore(term);
    //      I.replaceAllUsesWith(clonedI);
    //      I.eraseFromParent();
    //    }
    //    p.second->replaceSuccessorsPhiUsesWith(p.first);
    //    // p.second->eraseFromParent();
    //    term->eraseFromParent();
    //  }
    //}

    return modified;
  }

private:
  class InstScheduler : public InstVisitor<InstScheduler> {
  public:
    InstScheduler() : ID(0), direction(0), DT() {}

    void initialize(Function *F) {
      candidates.clear();

      if (F->isDeclaration())
        return;
      DT.recalculate(*F);
    }
    void setDirection(int v) { direction = v; }
    void setValueId(unsigned id) { ID = id; }

    int getDist(Instruction *begin, Instruction *end, Module *M) {

      const auto &FList = M->getFunctionList();
      int dist = 0;

      bool found = false;
      for (const auto &F : FList) {
        const auto &BBList = F.getBasicBlockList();
        for (const auto &BB : BBList) {
          const auto &IList = BB.getInstList();
          for (const auto &I : IList) {
            if (&I == end) {
              return dist;
            }
            if (found)
              ++dist;
            if (&I == begin)
              found = true;
          }
        }
      }
      return -1;
    }

    void checkForReschedule(Instruction *I) {

      if (I->getOpcode() != ID)
        return;

      if (direction == 1) {
        Function *F = I->getParent()->getParent();

        for (const auto &arg : F->args())
          if (I->getOperand(0) == &arg) {
            candidates.push_back(make_pair(I, nullptr));
            break;
          }
        return;
      }

      int dist = INT32_MAX;
      Instruction *target = nullptr;
      for (auto user : I->users()) {
        if (isa<PHINode>(user))
          return; // discard Insts that have a PHINode as user
        auto cand = dyn_cast<Instruction>(user);
        int d = getDist(I, cand, I->getParent()->getParent()->getParent());

        if (d >= 0 && d < dist) {
          dist = d;
          target = cand;
        }
      }

      if (target) {
        candidates.push_back(make_pair(I, target));
      }
    }

    void visitInstruction(Instruction &I) { checkForReschedule(&I); }

    void finalize(Function *F) {
      for (auto I : candidates) {
        if (direction == 0) {
          auto oldSuccessor = I.first->getNextNode();
          I.first->moveBefore(I.second);
          DT.recalculate(*F);
          bool isSave = true;
          for (auto u : I.first->users()) {

            isSave &= DT.dominates(I.first, cast<Instruction>(u));
          }

          if (!isSave) {
            I.first->moveBefore(oldSuccessor);
          }

          moved.push_back(I.first);
        } else {
          BasicBlock *BB = I.first->getParent();
          I.first->moveBefore(&BB->getInstList().front());
        }
      }

      for (auto I : bit) {
        I.first->moveBefore(&I.second->getInstList().front());
      }
      for (auto I : idx) {
        I.first->removeFromParent();
        auto term = &I.second->getInstList().back();
        term->removeFromParent();
        I.second->getInstList().push_back(I.first);
        I.second->getInstList().push_back(term);
      }
      for (auto I : gep) {
        I.first->removeFromParent();
        auto term = &I.second->getInstList().back();
        term->removeFromParent();
        I.second->getInstList().push_back(I.first);
        I.second->getInstList().push_back(term);
      }
    }

    bool hasCandidates() { return candidates.size() > 0; }

  private:
    vector<pair<Instruction *, Instruction *>> candidates;
    vector<pair<Instruction *, BasicBlock *>> idx;
    vector<pair<Instruction *, BasicBlock *>> gep;
    vector<pair<Instruction *, BasicBlock *>> bit;
    vector<Instruction *> moved;
    unsigned ID;
    int direction;

    DominatorTree DT;
  };

  class BaseOpts : public InstVisitor<BaseOpts> {
  public:
    BaseOpts(Module *module) : M(module) {}

    void visitCallInst(CallInst &CI) {

      if (!CI.getCalledFunction())
        return; // discard inline ASM

      if (!CI.getCalledFunction()->isIntrinsic()) {
        Function *reflect = M->getFunction("__nvvm_reflect");
        if (CI.getCalledFunction() == reflect) {
          CI.replaceAllUsesWith(ConstantInt::get(CI.getType(), 0));
          dead.push_back(&CI);
        }
      }
    }

    void visitExtractElementInst(ExtractElementInst &I) {
      for (auto u : I.users()) {
        if (auto uI = dyn_cast<Instruction>(u)) {
          if (uI->getParent() == I.getParent()) {
            return;
          }
        }
      }
      extracts.push_back(&I);
    }

    void initialize(Function &F) {
      dead.clear();
      extracts.clear();
      if (F.isDeclaration())
        return;
    }

    void finalize() {
      for (auto I : extracts) {
        vector<pair<BasicBlock *, ExtractElementInst *>> cloned_into;
        for (auto u : I->users()) {

          if (auto uI = dyn_cast<Instruction>(u)) {
            auto found = std::find_if(
                cloned_into.begin(), cloned_into.end(),
                [&](auto p) { return p.first == uI->getParent(); });
            ExtractElementInst *clone = nullptr;
            if (found == cloned_into.end()) {
              clone = cast<ExtractElementInst>(I->clone());
              clone->insertBefore(uI->getParent()->getFirstNonPHI());
              cloned_into.push_back(make_pair(uI->getParent(), clone));
            }
            if (clone == nullptr)
              clone = found->second;
            for (unsigned i = 0; i < uI->getNumOperands(); ++i) {
              if (uI->getOperand(i) == I) {
                uI->setOperand(i, clone);
              }
            }
          }
        }
        if (I->hasNUses(0))
          I->eraseFromParent();
      }

      for (auto &d : dead) {
        d->replaceAllUsesWith(UndefValue::get(d->getType()));
        d->eraseFromParent();
      }
    }

  private:
    Module *M;
    vector<ExtractElementInst *> extracts;
    vector<Instruction *> dead;
  };

  class SharedMemScheduler : public InstVisitor<SharedMemScheduler> {
  public:
    SharedMemScheduler(bool runtime) : runtime(runtime) {}

    void initialize() { candidates.clear(); }

    void visitLoadInst(LoadInst &I) {
      if (I.getPointerAddressSpace() != 3)
        return;

      if (GetElementPtrInst *gep =
              dyn_cast<GetElementPtrInst>(I.getPointerOperand())) {
        if (LoadInst *NI = dyn_cast<LoadInst>(I.getNextNode())) {
          if (NI->getPointerAddressSpace() != 3)
            return;

          if (GetElementPtrInst *ngep =
                  dyn_cast<GetElementPtrInst>(NI->getPointerOperand())) {
            bool swap = false;
            auto idx_f = gep->idx_begin();
            auto idx_s = ngep->idx_begin();
            idx_f++;
            idx_s++;

            if (idx_f == gep->idx_end()) {
            }

            for (auto idx = idx_f, E = gep->idx_end(); idx != E; ++idx) {
              for (auto nidx = idx_s, nE = ngep->idx_end(); nidx != nE;
                   ++nidx) {
                if (idx->get() == nidx->get()) {
                  int c1 = idx - idx_f;
                  int c2 = nidx - idx_s;
                  if (c1 == 0 && c2 == 1) { // B[k][j] * A[j][k]  swap it
                    swap = true;
                    // candidates.push_back(make_pair(NI, &I));
                  }
                  if (c1 == 1 && c2 == 0)
                    swap = true;

                  if (BinaryOperator *op =
                          dyn_cast<BinaryOperator>(NI->getNextNode())) {

                    // if (op->getOperand(0) == &I && op->getOperand(1) ==
                    // NI)
                    //{
                    //    __message("swapped mul");
                    //    op->setOperand(0, NI);
                    //    op->setOperand(1, &I);
                    //}else if (op->getOperand(1) == &I && op->getOperand(0)
                    //== NI)
                    //{
                    //    __message("swapped mul2");
                    //    op->setOperand(1, NI);
                    //    op->setOperand(0, &I);
                    //}
                    if (swap)
                      op->setMetadata(
                          "pacxx.opt.swap",
                          MDNode::get(
                              I.getContext(),
                              llvm::ConstantAsMetadata::get(ConstantInt::get(
                                  IntegerType::getInt32Ty(I.getContext()),
                                  1))));
                  }
                }
              }
            }

            // if (swap)
            //{
            //    __warning("swapping");
            //    candidates.push_back(make_pair(NI, &I));
            //}
          }
        }
      }
    }

    void visitBinaryOperator(BinaryOperator &OP) {
      if (!runtime)
        return;
      if (auto md = OP.getMetadata("pacxx.opt.swap")) {
        if (LoadInst *LI = dyn_cast<LoadInst>(OP.getOperand(0))) {
          if (LoadInst *LI2 = dyn_cast<LoadInst>(OP.getOperand(1))) {
            candidates.push_back(make_pair(LI, LI2));
            OP.setOperand(0, LI2);
            OP.setOperand(1, LI);
          }
        }
      }
    }

    void finalize(Function *F) {
      for (auto I : candidates) {
        I.second->moveBefore(I.first);
        moved.push_back(I.first);
      }
    }

    bool hasCandidates() { return candidates.size() > 0; }

  private:
    vector<pair<Instruction *, Instruction *>> candidates;
    vector<Instruction *> moved;
    bool runtime;
  };

/*  class CastStripper : public InstVisitor<CastStripper> {
  public:
    CastStripper() {}

    void visitStoreInst(StoreInst &SI) {
      Replacement r;
      if (auto BC = dyn_cast<BitCastInst>(SI.getPointerOperand())) {
        r.SBC = BC;
        r.SI = &SI;

        r.LI = dyn_cast<LoadInst>(SI.getValueOperand());
        if (r.LI) {
          if (auto BC2 = dyn_cast<BitCastInst>(r.LI->getPointerOperand())) {
            r.LBC = BC2;
            if (BC->getSrcTy()->getPointerElementType() ==
                BC2->getSrcTy()->getPointerElementType()) {
              repls.push_back(r);
            }
          }
        }
      }
    }

    void finalize() {
      for (auto &r : repls) {
        __error("here"); 
        LoadInst *nLI = new LoadInst(r.LBC->getOperand(0), "", r.LI);
        StoreInst *nSI = new StoreInst(nLI, r.SBC->getOperand(0), r.SI);
        r.LBC->replaceAllUsesWith(UndefValue::get(r.LBC->getType()));
        r.SBC->replaceAllUsesWith(UndefValue::get(r.SBC->getType()));
        r.LBC->eraseFromParent();
        r.SBC->eraseFromParent();
        r.SI->eraseFromParent();
        r.LI->eraseFromParent();
      }

      repls.clear();
    }

  private:
    struct Replacement {
      LoadInst *LI;
      BitCastInst *LBC;
      StoreInst *SI;
      BitCastInst *SBC;
    };

    vector<Replacement> repls;
  };*/

  bool runtime;
};

char NVVMRegPass::ID = 0;
static RegisterPass<NVVMRegPass>
    X("nvvm_reg", "PACXX: path to reduce register preasure", false, false);
}

namespace llvm {
Pass *createPACXXNvvmRegPass(bool runtime) { return new NVVMRegPass(runtime); }
}
