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
void mergeStores(T& vec) {

  // sort on last index of gep
  std::sort(vec.begin(), vec.end(), [](auto a, auto b){
    GetElementPtrInst* first = a.first;
    GetElementPtrInst* second = b.first;
    int64_t idx1= cast<ConstantInt>((first->idx_end()-1)->get())->getValue().getSExtValue();
    int64_t idx2= cast<ConstantInt>((second->idx_end()-1)->get())->getValue().getSExtValue();
    return idx1 < idx2;
  });

  IRBuilder<> builder(vec[vec.size()-1].second);
  Type* elementTy = vec[0].second->getValueOperand()->getType();
  Type* vecTy = VectorType::get(elementTy, vec.size());
  Type* scalTy = builder.getIntNTy(elementTy->getIntegerBitWidth() * vec.size());
  //auto alloca = builder.CreateAlloca();
  //auto vector = builder.CreateLoad(alloca);

  //Value* vector = UndefValue::get(vecTy);

  Value* last = nullptr;
  int i = 0;
  for(auto& p : vec){
    //vector = builder.CreateInsertElement(vector, p.second->getValueOperand(), i++);
    auto ext = builder.CreateZExt(p.second->getValueOperand(), scalTy, "mergeStoreExt");
    auto shl = builder.CreateShl(ext, elementTy->getIntegerBitWidth() * (i++));
    if (last)
    {
      auto merged = builder.CreateOr(shl, last);
      last = merged;
    }
    else
      last = shl;
    last->dump();
  }

  auto addrCast = builder.CreateBitCast(vec[0].first, last->getType()->getPointerTo(0));
  addrCast->dump();
  auto mergedStore = builder.CreateStore(last, addrCast);
  mergedStore->dump();

  for (auto& p : vec)
    p.second->eraseFromParent();
}

static bool checkGEPIndices(GetElementPtrInst* first, GetElementPtrInst* second){
  if (first == second)
    return true;

  if (first->getNumIndices() != second->getNumIndices())
    return false;
  bool equalIndices = true;
  for (unsigned i = 0; i < first->getNumIndices()-1; ++i)
  {
    if ((first->idx_begin() + i)->get() != (second->idx_begin() + i)->get())
      equalIndices = false;
  }
  return equalIndices;
}

static bool checkGEPLastIndices(GetElementPtrInst* first){
  bool validIndex = true;
  for (unsigned i = 0; i < first->getNumIndices()-1; ++i)
  {
    if (dyn_cast<ConstantInt>((first->idx_begin() + i)->get()))
      validIndex = false;
  }
  return validIndex;
}

template<typename T>
bool checkForMergeableStores(T& vec){

   bool indexMatch = true;
  GetElementPtrInst* first = vec[0].first;
  // check if all GEPs differ only in the last index
  for (auto& p : vec) {
    GetElementPtrInst* gep = p.first;
    indexMatch &= checkGEPIndices(first, gep);
    indexMatch &= checkGEPLastIndices(gep);
  }

  if (!indexMatch)
    return false;
  // collect last indices
  vector<int64_t> idx, diff;
  for (auto& p : vec) {
    GetElementPtrInst *gep = p.first;
    auto* index = cast<ConstantInt>((gep->idx_end()-1)->get());
    idx.push_back(index->getValue().getSExtValue());
  }
  // sort indices
  std::sort(idx.begin(), idx.end());
  // check if indices are consecutive
  diff.resize(idx.size());
  std::adjacent_difference(idx.begin(), idx.end(), diff.begin());

  auto consecutive = std::all_of(diff.begin() + 1, diff.end(), [](auto v){ return v == 1;});


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

    void visitStoreInst(StoreInst& SI)
    {
      auto addr = SI.getPointerOperand();
      if (auto GEP = dyn_cast<GetElementPtrInst>(addr)){
        if (GEP->getPointerOperandType()->getPointerElementType()->isAggregateType())
        {
          stores[GEP->getPointerOperand()].push_back(make_pair(GEP, &SI));
        }
      }
    }

    void initialize(Function &F) {
      stores.clear();
      dead.clear();
    }

    void finalize() {

      for (auto& matches : stores)
      {
        auto& vec = matches.second;
        if (vec.size() == 4) {
          if (checkForMergeableStores(vec))
          {
            mergeStores(vec);
          }
        }

      }

      for (auto &d : dead) {
        d->replaceAllUsesWith(UndefValue::get(d->getType()));
        d->eraseFromParent();
      }
    }

  private:
    Module *M;

    map<Value*, vector<pair<GetElementPtrInst*, StoreInst*>>> stores;

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
