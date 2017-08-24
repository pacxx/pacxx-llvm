/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2014
*/

//#include <iostream>
#include <vector>
#include <cassert>
#include <algorithm>
#include <numeric>

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
template<typename T>
void mergeStores(T &vec) {

  // sort on last index of gep
  std::sort(vec.begin(), vec.end(), [](auto a, auto b) {
    GetElementPtrInst *first = a.first;
    GetElementPtrInst *second = b.first;
    int64_t idx1 = cast<ConstantInt>((first->idx_end() - 1)->get())->getValue().getSExtValue();
    int64_t idx2 = cast<ConstantInt>((second->idx_end() - 1)->get())->getValue().getSExtValue();
    return idx1 < idx2;
  });

  IRBuilder<> builder(vec[vec.size() - 1].second);
  Type *elementTy = vec[0].second->getValueOperand()->getType();
  Type *vecTy = VectorType::get(elementTy, vec.size());

  Value *vector = UndefValue::get(vecTy);

  std::for_each(vec.begin(), vec.end(), [&, i = 0](auto &p) mutable {
    vector = builder.CreateInsertElement(vector, p.second->getValueOperand(), i++);
    vector->dump();
  });

  auto addrCast = builder.CreateBitCast(vec[0].first, vector->getType()->getPointerTo(0));
  addrCast->dump();
  auto mergedStore = builder.CreateStore(vector, addrCast);
  mergedStore->dump();

  std::for_each(vec.begin(), vec.end(), [](auto &p) {
    p.second->eraseFromParent();
  });
}

// checks if two GEPs have the same set of indices except for the last
static bool checkGEPIndices(GetElementPtrInst *first, GetElementPtrInst *second) {
  if (first == second)
    return true;

  if (first->getNumIndices() != second->getNumIndices())
    return false;

  bool equalIndices = std::inner_product(first->idx_begin(), first->idx_end() - 1, second->idx_begin(), true,
                                         [](const bool &sum, const bool &val) { return sum & val; },
                                         [](const auto &idx1, const auto &idx2) {
                                           return idx1.get() == idx2.get();
                                         });

  return equalIndices;
}

// checks if the GEP has a constant last index
static bool checkGEPLastIndices(GetElementPtrInst *first) {
  return isa<ConstantInt>((first->idx_end() - 1));
}

template<typename T>
bool checkForMergeableStores(T &vec) {

  bool indexMatch = true;
  GetElementPtrInst *first = vec[0].first;
  // check if all GEPs differ only in the last index
  std::for_each(vec.begin(), vec.end(), [&](auto &p) {
    GetElementPtrInst *gep = p.first;
    indexMatch &= checkGEPIndices(first, gep);
    indexMatch &= checkGEPLastIndices(gep);
  });

  if (!indexMatch)
    return false;

  // collect last indices
  vector<int64_t> idx(vec.size()), diff(vec.size());

  std::transform(vec.begin(), vec.end(), idx.begin(), [&](auto &p) {
    GetElementPtrInst *gep = p.first;
    auto *index = cast<ConstantInt>((gep->idx_end() - 1)->get());
    return index->getValue().getSExtValue();
  });

  // sort indices
  std::sort(idx.begin(), idx.end());
  // check if indices are consecutive
  std::adjacent_difference(idx.begin(), idx.end(), diff.begin());

  auto consecutive = std::all_of(diff.begin() + 1, diff.end(), [](auto v) { return v == 1; });

  if (indexMatch && consecutive)
    llvm::errs() << "matched\n";

  return true;
}

struct NVVMRegPass : public ModulePass {
  static char ID;
  NVVMRegPass(bool runtime = false) : ModulePass(ID), runtime(runtime) {}
  virtual ~NVVMRegPass() {}

  virtual bool runOnModule(Module &M) {
    bool modified = true;

    auto kernels = getTagedFunctions(&M, "nvvm.annotations", "kernel");

    BaseOpts opt(&M);
    for (auto &F : M.getFunctionList()) {
      opt.initialize(F);
      opt.visit(F);
      opt.finalize();
    }

    return modified;
  }

private:

  class BaseOpts : public InstVisitor<BaseOpts> {
  public:
    BaseOpts(Module *module) : M(module) {}


    void visitMemCpyInst(MemCpyInst &MCI) {

      const DataLayout &dl = M->getDataLayout();

      Value *dest = MCI.getRawDest();
      Value *src = MCI.getRawSource();

      ConstantInt *lenVal = cast<ConstantInt>(MCI.getLength());
      uint64_t len = lenVal->getSExtValue();

      Type *srcType = src->getType()->getPointerElementType();
      Type *destType = dest->getType()->getPointerElementType();

      BitCastInst *srcCast = new BitCastInst(src, PointerType::get(VectorType::get(srcType, len), 0), "", &MCI);
      BitCastInst *destCast = new BitCastInst(dest, PointerType::get(VectorType::get(destType, len), 0), "", &MCI);

      unsigned srcAlign = dl.getPrefTypeAlignment(srcCast->getType()->getPointerElementType());
      unsigned destAlign = dl.getPrefTypeAlignment(destCast->getType()->getPointerElementType());

      LoadInst *load = new LoadInst(srcCast, "memcpy.load", false, srcAlign, &MCI);
      new StoreInst(load, destCast, false, destAlign, &MCI);

      dead.push_back(&MCI);
    }

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

    void visitStoreInst(StoreInst &SI) {
      auto addr = SI.getPointerOperand();
      if (auto GEP = dyn_cast<GetElementPtrInst>(addr)) {
        if (GEP->getPointerOperandType()->getPointerElementType()->isAggregateType()) {
          stores[GEP->getPointerOperand()].push_back(make_pair(GEP, &SI));
        }
      }
    }

    void initialize(Function &F) {
      stores.clear();
      dead.clear();
    }

    void finalize() {

      std::for_each(stores.begin(), stores.end(), [](auto &match) {
        auto &vec = match.second;
        auto count = vec.size();
        if (count > 1 && !(count & (count - 1))) { // check if we have a power of 2
          if (checkForMergeableStores(vec))
            mergeStores(vec);
        }
      });

      std::for_each(dead.begin(), dead.end(), [](auto &d) {
        d->replaceAllUsesWith(UndefValue::get(d->getType()));
        d->eraseFromParent();
      });
    }

  private:
    Module *M;

    map<Value *, vector<pair<GetElementPtrInst *, StoreInst *>>>
        stores;

    vector<Instruction *> dead;
  };

  bool runtime;
};

char NVVMRegPass::ID = 0;
static RegisterPass<NVVMRegPass>
    X("nvvm_reg", "PACXX: path to reduce register preasure", false, false);
}

namespace llvm {
Pass *createPACXXNvvmRegPass(bool runtime) { return new NVVMRegPass(runtime); }
}
