/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2013-2014
*/

#include "../lib/IR/LLVMContextImpl.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/PACXXTransforms.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include <cassert>
#include <iostream>
#include <set>
#include <sstream>
#include <vector>

#include "llvm/IR/Dominators.h"
#include <string>

#include "CallVisitor.h"
#include "ModuleHelper.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {
template <typename T>
Instruction *getFirstInstructionForConstantExpr(T &kernels, ConstantExpr &CE) {
  for (auto CEU : CE.users()) {
    if (auto I = dyn_cast<Instruction>(CEU)) {
      if (I->getParent()) {
        auto F = I->getParent()->getParent();
        llvm::errs() << "parent: " << F->getName().str() << "\n";
        if (find(begin(kernels), end(kernels), F) != end(kernels)) {
          return I;
        }
      }
    }
    if (auto nextCE = dyn_cast<ConstantExpr>(CEU)) {
      return getFirstInstructionForConstantExpr(kernels, *nextCE);
    }
  }
  return nullptr;
}

template <typename T>
Function *getParentKernel(T &kernels, GlobalVariable &GV) {
  Function *F = nullptr;
  for (auto U : GV.users()) {
    Instruction *I = nullptr;
    if (isa<Instruction>(U)) {
      I = cast<Instruction>(U);
    }
    if (auto CE = dyn_cast<ConstantExpr>(U)) {
      I = getFirstInstructionForConstantExpr(kernels, *CE);
    }

    if (I && I->getParent()) {
      F = I->getParent()->getParent();
    }
    if (find(begin(kernels), end(kernels), F) != end(kernels)) {
      break;
    }
  }
  return F;
}

struct SPIRPass : public ModulePass {
  static char ID;
  SPIRPass() : ModulePass(ID) {}
  virtual ~SPIRPass() {}

  virtual bool runOnModule(Module &M) {
    _M = &M;

    bool modified = true;

    unsigned ptrSize = M.getDataLayout().getPointerSizeInBits();
    if (ptrSize == 64) {
      M.setTargetTriple("spir64-unknown-unknown");

      M.setDataLayout("e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:"
                      "64-f32:32:32-f64:64:64-v16:16:16-v24:32:32-v32:32:32-"
                      "v48:64:64-v64:64:64-v96:128:128-v128:128:128-v192:256:"
                      "256-v256:256:256-v512:512:512-v1024:1024:1024");
    } else {
      M.setTargetTriple("spir-unknown-unknown");

      M.setDataLayout("e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:"
                      "64-f32:32:32-f64:64:64-v16:16:16-v24:32:32-v32:32:32-"
                      "v48:64:64-v64:64:64-v96:128:128-v128:128:128-v192:256:"
                      "256-v256:256:256-v512:512:512-v1024:1024:1024");
    }

    kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");
    auto barrier = M.getFunction("_Z7barrierj");
    if (barrier) {
      auto tb =
          M.getOrInsertFunction("tmp_barrier", barrier->getFunctionType());

      barrier->replaceAllUsesWith(tb);
    }

    auto visitor = make_CallVisitor([&](CallInst *I) {
      if (!I)
        return;

      if (!I->isInlineAsm()) {

        if (!isa<Function>(I->getCalledValue())) {
          I->dump();
        } else {
          I->setCallingConv(CallingConv::SPIR_FUNC);
          I->getCalledFunction()->setCallingConv(CallingConv::SPIR_FUNC);
        }
      }
    });

    for (auto &F : kernels) {
      visitor.visit(F);

      // Mutate pointer types to bring them into AS 1
      auto &BB = F->getBasicBlockList().front();
      auto II = &BB.getInstList().front();
      bool mutate = false;
      for (auto &arg : F->args()) {
        if (arg.getType()->isPointerTy()) {
          if (arg.getType()->getPointerAddressSpace() == 0) {
            auto AL = new AllocaInst(arg.getType(), "", II);
            auto SI = new StoreInst(&arg, AL, II);
            auto LI = new LoadInst(AL, "", II);
            arg.replaceAllUsesWith(LI);
            arg.mutateType(
                arg.getType()->getPointerElementType()->getPointerTo(1));

            auto ASC = new AddrSpaceCastInst(
                &arg, arg.getType()->getPointerElementType()->getPointerTo(0),
                "", II);
            LI->replaceAllUsesWith(ASC);
            LI->eraseFromParent();
            SI->eraseFromParent();
            AL->eraseFromParent();
            mutate = true;
          }
        }
      }

      if (mutate) {
        SmallVector<Type *, 8> Params;
        for (auto &arg : F->args())
          Params.push_back(arg.getType());

        Type *RetTy = F->getReturnType();
        FunctionType *NFTy = FunctionType::get(RetTy, Params, false);
        auto name = F->getName().str();
        F->setName("undead");
        auto NF =
            Function::Create(NFTy, F->getLinkage(), name, &M);
        auto DestI = NF->arg_begin();
        ValueToValueMapTy VMap;
        for (auto I = F->arg_begin(); I != F->arg_end(); ++I) {
          DestI->setName(I->getName());
          VMap[cast<Value>(I)] = cast<Value>(DestI++);
        }
        SmallVector<ReturnInst *, 8> returns;
        CloneFunctionInto(NF, F, VMap, true, returns);
        if (auto MD = M.getNamedMetadata("nvvm.annotations")) {
          for (unsigned i = 0; i != MD->getNumOperands(); ++i) {
            auto Op = MD->getOperand(i);
            if( auto *KF = dyn_cast<Function>( dyn_cast<ValueAsMetadata>(Op->getOperand(0).get())->getValue()))
            {
              if (KF == F)
              {
                Op->replaceOperandWith(0, ValueAsMetadata::get(NF));
              }
            }
          }
        }

        // F->replaceAllUsesWith(NF);
        ReplaceUnsafe(F, NF);
        //  F->eraseFromParent();
      }
    }

    kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

    map<Function *, unsigned> SMMapping;
    for (auto &GV : M.globals()) {
      if (GV.getMetadata("pacxx.as.shared") != nullptr) {
        auto F = getParentKernel(kernels, GV);
        if (F) {
          unsigned i = SMMapping[F];

          string newName = F->getName().str() + ".sm" + to_string(i);
          auto newGV = new GlobalVariable(M, GV.getType(), false, GV.getLinkage(), nullptr, newName, &GV, GV.getThreadLocalMode(), 3, true);
          newGV->dump(); 
          GV.setName(newName);
          SMMapping[F] = i + 1;
          break;
        }
        else 
          llvm::errs() << "no parent found for: " << GV.getName().str() << "\n"; 
      }
    }

    auto called = visitor.get();

    if (auto TB = M.getFunction("tmp_barrier")) {
      TB->replaceAllUsesWith(barrier);
    }

    cleanupDeadCode(_M);
    for (auto &G : _M->getGlobalList()) {
      if (auto ptrT = dyn_cast<PointerType>(G.getType())) {
        if (auto arrTy = dyn_cast<ArrayType>(ptrT->getElementType())) {
          if (arrTy->getArrayNumElements() != 0)
            G.setLinkage(llvm::GlobalValue::LinkageTypes::InternalLinkage);
        }
      }
    }

    for (auto &F : M.getFunctionList()) {

      if (&F == barrier)
        continue;

      AttributeSet attrs = F.getAttributes();
      AttributeSet new_attrs;
      new_attrs.addAttribute(M.getContext(), 0, Attribute::NoUnwind);
      int idx = 1;
      for (unsigned x = 0; x != attrs.getNumSlots(); ++x)
        for (auto i = attrs.begin(x), e = attrs.end(x); i != e; ++i) {
          if (i->isEnumAttribute()) {
            switch (i->getKindAsEnum()) {
            case Attribute::AlwaysInline:
            case Attribute::InlineHint:
            case Attribute::NoInline:
            case Attribute::ReadNone:
            case Attribute::ReadOnly:
              new_attrs.addAttribute(M.getContext(), idx, i->getKindAsEnum());
              idx++;
              break;
            default:
              break;
            }
          }
        }

      F.setAlignment(0);
      F.setAttributes(new_attrs);
      F.setLinkage(GlobalValue::LinkageTypes::ExternalLinkage);
      F.setVisibility(GlobalValue::VisibilityTypes::DefaultVisibility);
      F.setCallingConv(CallingConv::SPIR_FUNC);
    }
    VectorInstFixer fixer;
    for (auto &F : kernels) {
      fixer.visit(F);
      F->setCallingConv(CallingConv::SPIR_KERNEL);
    }

    vector<unsigned> inst_types;
    inst_types.push_back(Instruction::Load);
    inst_types.push_back(Instruction::GetElementPtr);
    inst_types.push_back(Instruction::BitCast);
    InstScheduler shed;
    for (auto op : inst_types) {
      for (auto F : kernels) {
        shed.initialize(F);
        shed.setValueId(op);
        shed.visit(F);
        shed.finalize(F);
      }
    }

 /*   for (auto &F : M.getFunctionList()) {

      CastFixer bcf;
      bcf.visit(F);
      bcf.finalize();

      LoadFixer lf;
      lf.visit(F);
      lf.finalize();

      GEPFixer gepf;
      gepf.visit(F);
      gepf.finalize();

      IntrinsicScheduler isced;
      isced.initialize();
      isced.visit(F);
      isced.finalize();

      bcf.visit(F);
      bcf.finalize();
    }
*/
    return modified;
  }

private:

  class IntrinsicScheduler : public InstVisitor<IntrinsicScheduler> {
  public:
    IntrinsicScheduler() {}

    void initialize() { intrinsicClones.clear(); }

    void finalize() {

      for (auto p : repl) {
        auto M = p.first->getParent()->getParent()->getParent();
        SmallVector<Type *, 1> args;
        args.push_back(Type::getFloatTy(M->getContext()));
        FunctionType *FT =
            FunctionType::get(Type::getFloatTy(M->getContext()), args, false);
        auto F = M->getOrInsertFunction("_Z5rsqrtf", FT);

        SmallVector<Value *, 1> param;
        param.push_back(p.first->getOperand(0));

        CallInst *newCall = CallInst::Create(F, param, "", p.first);
        p.second->replaceAllUsesWith(newCall);
        p.second->eraseFromParent();
        p.first->eraseFromParent();
      }

      for (auto I : dead) {
        I->eraseFromParent();
      }

      std::map<BasicBlock *, vector<CallInst *>> blocks;
      for (auto I : intrinsicClones) {

        auto &clones = blocks[I.first->getParent()];
        CallInst *clone = I.second;

        auto lookup = find_if(clones.begin(), clones.end(), [=](auto cand) {
          if (cand->getCalledFunction() == clone->getCalledFunction() &&
              cand->getOperand(0) == clone->getOperand(0))
            return true;
          else
            return false;
        });

        if (lookup != clones.end())
          clone = *lookup;
        else
          clone = nullptr;

        if (clone == nullptr) {
          Instruction *firstI = nullptr;
          for (auto &II : *I.first->getParent()) {
            if (!isa<PHINode>(II)) {
              firstI = &II;
              break;
            }
          }

          SmallVector<Value *, 1> args;
          args.push_back(I.second->getOperand(0));
          clone =
              CallInst::Create(I.second->getCalledFunction(), args, "", firstI);
          clone->setTailCall(I.second->isTailCall());
          blocks[I.first->getParent()].push_back(clone);
        }

        for (unsigned i = 0; i != I.first->getNumOperands(); ++i) {
          auto op = I.first->getOperand(i);
          if (op == I.second) {
            I.first->setOperand(i, clone);
          }
        }
      }
    }

    void visitCallInst(CallInst &CI) {
      if (CI.isInlineAsm())
        return;
      if (!isa<Function>(CI.getCalledValue())) {
        return;
      }
      if (isaSregIntrinsic(CI.getCalledFunction())) {
        if (CI.hasNUses(0)) {
          dead.push_back(&CI);
          return;
        }
        for (auto u : CI.users()) {
          if (Instruction *I = dyn_cast<Instruction>(u)) {
            // clone them if they are not in the same basic block
            if (!isa<PHINode>(I) && I->getParent() != CI.getParent()) {
              intrinsicClones.push_back(make_pair(I, &CI));
            }
          }
        }
      } else if (CI.getCalledFunction() ||
                 CI.getCalledFunction()->getName().find("sqrt") !=
                     StringRef::npos) {
        bool valid = CI.hasNUses(1);
        // REFACTOR: 1.0f / sqrt -> rsqrt optimiation
        if (valid) {
          for (auto u : CI.users()) {
            if (BinaryOperator *bi = dyn_cast<BinaryOperator>(u)) {
              if (bi->getOpcode() == BinaryOperator::FDiv) {
                if (bi->getOperand(1) == &CI) {
                  if (ConstantFP *div =
                          dyn_cast<ConstantFP>(bi->getOperand(0))) {
                    //__debug(div->getValueAPF().convertToFloat(), " - ",
                    //        1.0f);
                    if (div->getValueAPF().convertToFloat() == 1.0f) {
                      repl.push_back(make_pair(&CI, bi));
                    }
                  }
                }
              }
            }
          }
        }

        CI.getCalledFunction()->addFnAttr(Attribute::NoUnwind);
        CI.getCalledFunction()->addFnAttr(Attribute::ReadNone);
      }
    }

    bool isaSregIntrinsic(Function *F) {
      if (!F)
        return false;
      bool found = false;
      auto name = F->getName();
      if (name.find("get_local_id") != StringRef::npos)
        found = true;
      if (name.find("get_global_id") != StringRef::npos)
        found = true;
      if (name.find("get_local_size") != StringRef::npos)
        found = true;
      if (name.find("get_group_id") != StringRef::npos)
        found = true;
      if (name.find("get_group_size") != StringRef::npos)
        found = true;

      if (found) {
        F->addFnAttr(Attribute::NoUnwind);
        F->addFnAttr(Attribute::ReadNone);
        return true;
      }
      return false;
    }

  private:
    vector<pair<Instruction *, CallInst *>> intrinsicClones;
    vector<CallInst *> dead;
    vector<pair<CallInst *, BinaryOperator *>> repl;
  };

  class FMAOpt : public InstVisitor<FMAOpt> {
  public:
    FMAOpt(Module *module) : M(module) {}

    void visitFMul(BinaryOperator &BI) {
      if (BI.hasNUses(1)) {

        for (auto use : BI.users()) {
          if (BinaryOperator *UBI = dyn_cast<BinaryOperator>(use)) {
            if (UBI->getOpcode() == BinaryOperator::FAdd) {

              if (isa<ExtractElementInst>(UBI->getOperand(0)) ||
                  isa<ExtractElementInst>(UBI->getOperand(1)))
                return;

              vector<Value *> args;
              args.push_back(BI.getOperand(0));
              args.push_back(BI.getOperand(1));

              if (UBI->getOperand(0) == &BI) {
                args.push_back(UBI->getOperand(1));
              } else {
                args.push_back(UBI->getOperand(0));
              }

              SmallVector<Type *, 1> targ;
              targ.push_back(BI.getOperand(0)->getType());

              Function *fma =
                  Intrinsic::getDeclaration(M, Intrinsic::fmuladd, targ);

              CallInst *fc = CallInst::Create(fma, args, "");
              fc->setTailCall(true);
              reps.push_back(
                  make_pair(cast<Instruction>(UBI), cast<Instruction>(fc)));
              dead.push_back(&BI);
            }
          }
        }
      }
    }

    void finalize() {

      for (auto &p : reps) {
        ReplaceInstWithInst(p.first, p.second);
      }

      for (auto &d : dead) {
        d->replaceAllUsesWith(UndefValue::get(d->getType()));
        d->eraseFromParent();
      }
    }

  private:
    vector<pair<Instruction *, Instruction *>> reps;
    vector<Instruction *> dead;
    Module *M;
  };

  class LoadFixer : public InstVisitor<LoadFixer> {
  public:
    LoadFixer() {}

    void visitLoadInst(LoadInst &I) {
      auto AS = I.getPointerAddressSpace();
      if (AS > 0) {
        if (I.getType()->isPointerTy())
          if (I.getType()->getPointerAddressSpace() !=
              I.getPointerOperand()->getType()->getPointerAddressSpace()) {
            auto NI = new LoadInst(
                I.getPointerOperand()->getType()->getPointerElementType(),
                I.getPointerOperand(), "", I.isVolatile(), I.getAlignment());
            reps.push_back(make_pair(&I, NI));
          }
      }
    }

    void finalize() {

      for (auto &p : reps) {
        ReplaceInstWithInst(p.first, p.second);
      }
    }

  private:
    vector<pair<Instruction *, Instruction *>> reps;
  };

  class GEPFixer : public InstVisitor<GEPFixer> {
  public:
    GEPFixer() {}

    void visitGetElementPtrInst(GetElementPtrInst &I) {
      if (I.getType()->isPointerTy()) {
        auto AS = I.getAddressSpace();
        if (AS != 0) {
          SmallVector<Value *, 3> args;
          for (auto it = I.idx_begin(), end = I.idx_end(); it != end; ++it)
            args.push_back(*it);
          reps.push_back(make_pair(
              &I, GetElementPtrInst::Create(
                      I.getPointerOperandType()->getPointerElementType(),
                      I.getPointerOperand(), args, "", &I)));
        }
      }
    }

    void finalize() {

      for (auto &p : reps) {
        ReplaceUnsafe<Instruction>(p.first, p.second);
      }
    }

  private:
    vector<pair<Instruction *, Instruction *>> reps;
  };

  class CastFixer : public InstVisitor<CastFixer> {
  public:
    CastFixer() {}

    void visitBitCastInst(BitCastInst &I) {
      if (I.hasNUses(0)) {
        dead.push_back(&I);
        return;
      }

      if (!CastInst::castIsValid(Instruction::BitCast, I.getOperand(0),
                                 I.getType())) {
        auto nC = CastInst::Create(
            Instruction::BitCast, I.getOperand(0),
            I.getDestTy()->getPointerElementType()->getPointerTo(
                I.getOperand(0)->getType()->getPointerAddressSpace()),
            "", &I);
        reps.push_back(make_pair(&I, nC));
      }
    }

    void visitAddrSpaceCastInst(AddrSpaceCastInst &ASCI) {
      if (!CastInst::castIsValid(Instruction::AddrSpaceCast, ASCI.getOperand(0),
                                 ASCI.getType())) {
        ASCI.replaceAllUsesWith(ASCI.getOperand(0));
        dead.push_back(&ASCI);
      }
    }

    void finalize() {

      for (auto &p : reps) {
        ReplaceUnsafe<Instruction>(p.first, p.second);
      }

      for (auto &I : dead)
        I->eraseFromParent();

      reps.clear();
      dead.clear();
    }

  private:
    vector<pair<Instruction *, Instruction *>> reps;
    vector<Instruction *> dead;
  };

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
        if (isa<PHINode>(user) ||
            (isa<LoadInst>(I) &&
             isa<StoreInst>(
                 user))) // discard if we try to move a load directly to a store
          return;        // discard Insts that have a PHINode as user
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

  class VectorInstFixer : public InstVisitor<VectorInstFixer> {
  public:
    void visitInsertElementInst(InsertElementInst &IEI) {
      auto index = ConstantInt::get(
          Type::getInt32Ty(IEI.getContext()),
          (int)*(
              cast<ConstantInt>(IEI.getOperand(2))->getValue().getRawData()));
      IEI.setOperand(2, index);
    }

    void visitExtractElementInst(ExtractElementInst &EEI) {
      auto index = ConstantInt::get(
          Type::getInt32Ty(EEI.getContext()),
          (int)*(
              cast<ConstantInt>(EEI.getOperand(1))->getValue().getRawData()));

      EEI.setOperand(1, index);
    }
  };


  class MemIntrinsicFixer : public InstVisitor<MemIntrinsicFixer> {
  public:
    void visitCallInst(CallInst &CI) {
      auto F = CI.getCalledFunction();
      if (F && F->isIntrinsic()) {
        auto id = F->getIntrinsicID();
        if (id == Intrinsic::ID::lifetime_start ||
            id == Intrinsic::ID::lifetime_end) {
          dead.push_back(&CI);
        }

        if (id == Intrinsic::ID::memcpy) {
          // CI.dump();
        }
      }
    }

    void finalize() {
      for (auto I : dead) {
        I->eraseFromParent();
      }
    }

  private:
    vector<Instruction *> dead;
  };

  set<Function *> kernels;
  Module *_M;
};

char SPIRPass::ID = 0;
static RegisterPass<SPIRPass> X("spir", "LLVM to SPIR IR pass", false, false);
}

namespace llvm {
Pass *createPACXXSpirPass() { return new SPIRPass(); }
}
