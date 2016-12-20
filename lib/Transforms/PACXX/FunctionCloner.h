/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2013-2014
*/
#ifndef LLVM_TRANSFORM_PACXX_FUNCTIONCLONER_H
#define LLVM_TRANSFORM_PACXX_FUNCTIONCLONER_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include "ModuleHelper.h"
#include <map>
#define USE_STANDALONE 1
#include "Log.h"

using namespace llvm;
using namespace std;

namespace pacxx {

class AddrSpaceMapper : public ValueMapTypeRemapper {
public:
  inline void registerRemap(Type *oldTy, Type *newTy) {
    if (type_map.find(oldTy) == type_map.end())
      type_map.insert(make_pair(oldTy, newTy));
  }

  inline Type *lookup(Type *oldTy) {
    auto I = type_map.find(oldTy);
    if (I == type_map.end())
      return nullptr;
    else
      return I->second;
  }

  inline Type *remapType(Type *oldTy) {
    return oldTy;
    if (oldTy->isStructTy()) {
      auto type = lookup(oldTy);
      if (type == nullptr) {
        type = modifyStruct(cast<StructType>(oldTy));
      }
      return type;
    }

    Type *baseTy = oldTy;
    int indirections = 0;
    do {
      if (baseTy->isPointerTy()) {
        if (cast<PointerType>(baseTy)->getAddressSpace() != 0) {
          // do nothing for pointers already pointing into an address space
          return oldTy;
        }
        baseTy = baseTy->getPointerElementType();
        indirections++;
      } else
        break;
    } while (true);

    if (baseTy->isStructTy()) {

      auto type = lookup(baseTy);
      if (type == nullptr) {
        type = modifyStruct(cast<StructType>(baseTy));
      }
      baseTy = type;
    }

    if (Type *newTy = lookup(baseTy)) {
      for (int i = 0; i != indirections; ++i) {
        newTy = newTy->getPointerTo(i == 0 ? 1 : 0);
      }

      return newTy;
    } else {
      if (oldTy->isPointerTy()) {
        for (int i = 0; i != indirections; ++i) {
          baseTy = baseTy->getPointerTo(i == 0 ? 1 : 0);
        }
        return baseTy;
      }
      return oldTy;
    }
  }

  inline StructType *modifyStruct(StructType *STy) {

    if (!STy->getStructName().find("vector")) {
      registerRemap(STy, STy);
      return STy;
    }

    Type *newType = lookup(STy);
    if (newType)
      return cast<StructType>(newType); // allready modified

    vector<Type *> elementTys;
    bool changed = false;
    for (unsigned int i = 0; i != STy->getNumElements(); ++i) {
      Type *ElemTy = STy->getElementType(i);
      if (ElemTy->isPointerTy()) {
        if (ElemTy->getPointerElementType()->isStructTy())
          ElemTy =
              modifyStruct(cast<StructType>(ElemTy->getPointerElementType()))
                  ->getPointerTo(1);
        else
          ElemTy = ElemTy->getPointerElementType()->getPointerTo(1);

        changed = true;
      }
      if (ElemTy->isStructTy()) {
        StructType *NewTy = modifyStruct(cast<StructType>(ElemTy));
        if (NewTy != ElemTy) {
          ElemTy = NewTy;
          changed = true;
        }
      }

      elementTys.push_back(ElemTy);
    }

    if (changed) {
      StructType *NewTy = StructType::create(elementTys, STy->getName());
      registerRemap(STy, NewTy);

      return NewTy;
    } else {
      registerRemap(STy, STy);
    }
    return STy;
  }

private:
  map<Type *, Type *> type_map;
};

class FunctionCloner : public InstVisitor<FunctionCloner> {
public:
  FunctionCloner(Module *M) : _M(M), remapper() {}
  virtual ~FunctionCloner() {}

  void mapTypes(Function *F) {
    FunctionType *FTy = F->getFunctionType();
    for (unsigned int i = 0; i != FTy->getNumParams(); ++i) {
      Type *PTy = FTy->getParamType(i);

      if (PTy->isPointerTy()) {
        Type *ETy = PTy->getPointerElementType();
        if (ETy->isStructTy()) {
          StructType *StructTy = cast<StructType>(ETy);
          vector<Type *> elementTys;

          if (auto lookup = remapper.lookup(ETy)) {
            ETy = lookup;
          } else {
            ETy = remapper.modifyStruct(StructTy);
          }
        }
      }
    }
  }

  Function *clone(Function *F, bool isKernel, unsigned int AS = 1) {
	  if (F->getName().find("printf") != StringRef::npos)
		  return nullptr;
    FunctionType *FTy = F->getFunctionType();
    vector<Type *> Params;
    bool validToClone = false;
    for (unsigned int i = 0; i != FTy->getNumParams(); ++i) {
      Type *PTy = FTy->getParamType(i);

      if (PTy->isPointerTy()) {
        validToClone = true;
        Type *ETy = PTy->getPointerElementType();
        if (ETy->isStructTy()) {
          StructType *StructTy = cast<StructType>(ETy);
          vector<Type *> elementTys;

          if (auto lookup = remapper.lookup(ETy)) {
            ETy = lookup;
          } else {
            ETy = remapper.modifyStruct(StructTy);
          }
        }

        auto ptrTY = ETy->getPointerTo(AS);
        Params.push_back(ptrTY);

      } else
        Params.push_back(PTy);
    }

    if (!validToClone) {
      __debug("not cloning: ", F->getName().str());
      return F;
    }

    Type *RetTy = F->getReturnType();

    //// Create the new function type based on the recomputed parameters.
    FunctionType *NFTy = FunctionType::get(RetTy, Params, false);
    std::string NFName = F->getName().str();
    if (!isKernel)
      NFName += std::string("_AS") + std::to_string(AS);
    NFName.erase(std::find(NFName.begin(), NFName.end(), '\0'));
    Function *NF = Function::Create(NFTy, F->getLinkage(), NFName, _M);

    VMap.clear();
    IMap.clear();
    auto DestI = NF->arg_begin();
    for (auto I = F->arg_begin(); I != F->arg_end(); ++I) {
      DestI->takeName(cast<Value>(I));
      VMap[cast<Value>(I)] = cast<Value>(DestI++);
    }

    SmallVector<ReturnInst *, 8> returns;
    clone(NF, F, returns, "");

    if (isKernel)
      NF->takeName(F);

    for (auto I = IMap.begin(), E = IMap.end(); I != E; ++I) {
      Instruction *from = I->first;
      Instruction *to = I->second;

      if (from && to)
        ReplaceUnsafe<Instruction>(from, to);
    }

    return NF;
  }

  Function *clone(const Function *F) {
	  if (F->getName().find("printf") != StringRef::npos)
		  return nullptr;

    Function *NF = Function::Create(F->getFunctionType(), F->getLinkage(),
                                    F->getName().str() + "_clone", _M);

    auto DestI = NF->arg_begin();
    for (auto I = F->arg_begin(); I != F->arg_end(); ++I) {
      VMap[cast<Value>(I)] = cast<Value>(DestI++);
    }

    SmallVector<ReturnInst *, 8> returns;
    clone(NF, F, returns, "", false);
    auto NI = NF->arg_begin();
    for (auto I = F->arg_begin(); I != F->arg_end(); ++I) {
      NI->setName(I->getName());
      NI++;
    }
    return NF;
  }

private:
  // Clone OldFunc into NewFunc, transforming the old arguments into
  // references to
  // VMap values.
  //
  void clone(Function *NewFunc, const Function *OldFunc,
             SmallVectorImpl<ReturnInst *> &Returns, const char *NameSuffix,
             bool handleInstr = true) {
    assert(NameSuffix && "NameSuffix cannot be null!");

#ifndef NDEBUG
    for (Function::const_arg_iterator I = OldFunc->arg_begin(),
                                      E = OldFunc->arg_end();
         I != E; ++I)
      assert(VMap.count(I) && "No mapping from source argument specified!");
#endif

    // Copy all attributes other than those stored in the AttributeSet.  We
    // need
    // to remap the parameter indices of the AttributeSet.
    AttributeSet NewAttrs = NewFunc->getAttributes();
    NewFunc->copyAttributesFrom(OldFunc);
    NewFunc->setAttributes(NewAttrs);

    AttributeSet OldAttrs = OldFunc->getAttributes();
    // Clone any argument attributes that are present in the VMap.
    for (const Argument &OldArg : OldFunc->args())
      if (Argument *NewArg = dyn_cast<Argument>(VMap[&OldArg])) {
        AttributeSet attrs = OldAttrs.getParamAttributes(OldArg.getArgNo() + 1);
        if (attrs.getNumSlots() > 0)
          NewArg->addAttr(attrs);
      }

    NewFunc->setAttributes(
        NewFunc->getAttributes()
            .addAttributes(NewFunc->getContext(), AttributeSet::ReturnIndex,
                           OldAttrs.getRetAttributes())
            .addAttributes(NewFunc->getContext(), AttributeSet::FunctionIndex,
                           OldAttrs.getFnAttributes()));

    // Loop over all of the basic blocks in the function, cloning them as
    // appropriate.  Note that we save BE this way in order to handle
    // cloning of
    // recursive functions into themselves.
    //
    for (Function::const_iterator BI = OldFunc->begin(), BE = OldFunc->end();
         BI != BE; ++BI) {
      const BasicBlock &BB = *BI;

      // Create a new basic block and copy instructions into it!
      NewBB = BasicBlock::Create(BB.getContext(), "", NewFunc);
      if (BB.hasName())
        NewBB->setName(BB.getName() + NameSuffix);

      bool hasCalls = false, hasDynamicAllocas = false,
           hasStaticAllocas = false;

      // Loop over all instructions, and copy them over.
      for (BasicBlock::const_iterator II = BB.begin(), IE = BB.end(); II != IE;
           ++II) {
        Instruction *NewInst = II->clone();
        if (II->hasName())
          NewInst->setName(II->getName() + NameSuffix);
        NewBB->getInstList().push_back(NewInst);
        VMap[cast<Value>(II)] =
            cast<Value>(NewInst); // Add instruction map to value.

        hasCalls |= (isa<CallInst>(II) && !isa<DbgInfoIntrinsic>(II));
        if (const AllocaInst *AI = dyn_cast<AllocaInst>(II)) {
          if (isa<ConstantInt>(AI->getArraySize()))
            hasStaticAllocas = true;
          else
            hasDynamicAllocas = true;
        }
      }

      // Add basic block mapping.
      VMap[&BB] = NewBB;

      // It is only legal to clone a function if a block address within that
      // function is never referenced outside of the function.  Given that, we
      // want to map block addresses from the old function to block addresses in
      // the clone. (This is different from the generic ValueMapper
      // implementation, which generates an invalid blockaddress when
      // cloning a function.)
      if (BB.hasAddressTaken()) {
        Constant *OldBBAddr = BlockAddress::get(const_cast<Function *>(OldFunc),
                                                const_cast<BasicBlock *>(&BB));
        VMap[OldBBAddr] = BlockAddress::get(NewFunc, NewBB);
      }

      // Note return instructions for the caller.
      if (ReturnInst *RI = dyn_cast<ReturnInst>(NewBB->getTerminator()))
        Returns.push_back(RI);
    }

    // Loop over all of the instructions in the function, fixing up operand
    // references as we go.  This uses VMap to do all the hard work.

    vector<Instruction *> noASChange;
    for (auto &BB : *NewFunc) {
      // Loop over all instructions, fixing each one as we find it...
      for (auto &II : BB) {
        auto *I = &II;

        for (auto &op : II.operands()) {
          Value *V =
              MapValue(op, VMap, RF_NoModuleLevelChanges, &remapper, nullptr);
          // If we aren't ignoring missing entries, assert that something
          // happened.
          if (V)
            op = V;
        }

        // Remap phi nodes' incoming blocks.
        if (PHINode *PN = dyn_cast<PHINode>(I)) {
          for (unsigned i = 0, e = PN->getNumIncomingValues(); i != e; ++i) {
            Value *V = MapValue(PN->getIncomingBlock(i), VMap,
                                RF_NoModuleLevelChanges);
            // If we aren't ignoring missing entries, assert that something
            // happened.
            if (V)
              PN->setIncomingBlock(i, cast<BasicBlock>(V));
          }
        }

        // Remap attached metadata.
        SmallVector<std::pair<unsigned, MDNode *>, 4> MDs;
        I->getAllMetadata(MDs);
        for (SmallVectorImpl<std::pair<unsigned, MDNode *>>::iterator
                 MI = MDs.begin(),
                 ME = MDs.end();
             MI != ME; ++MI) {
          MDNode *Old = MI->second;
          MDNode *New = MapMetadata(Old, VMap, RF_NoModuleLevelChanges,
                                    &remapper, nullptr);
          if (New != Old)
            I->setMetadata(MI->first, New);
        }
        if (handleInstr)
          handle(I);
      }
    }
  }

  void handle(Function *F) {
    for (auto &BB : *F) {
      // Loop over all instructions, fixing each one as we find it...
      for (auto &II : BB) {
        handle(&II);
      }
    }
  }

  void handle(Instruction *I) {
    _handled = false;
    visit(I);
    if (!_handled)
      I->mutateType(remapper.remapType(I->getType()));
  }

public:
  void visitAllocaInst(AllocaInst &I) {
    auto newType = remapper.remapType(I.getType());
    if (newType->isPointerTy() && newType->getPointerAddressSpace() != 0) {
      newType = newType->getPointerElementType()->getPointerTo(0);
    }

    I.mutateType(newType);
    _handled = true;
  }

  //void visitStoreInst(StoreInst& I)
  //{
	 // PointerType *PTy = dyn_cast<PointerType>(I.getOperand(1)->getType());
	 // Type *ElTy = PTy->getElementType();
	 // if (ElTy != I.getOperand(0)->getType())
	 // {
	 // }
	 // _handled = true;
  //}

  void visitGetElementPtrInst(GetElementPtrInst &I) {
    if (I.getType()->isPointerTy()) {
      auto AS = I.getAddressSpace();
      if (AS != 0) {
        SmallVector<Value *, 3> args;
        for (auto it = I.idx_begin(), end = I.idx_end(); it != end; ++it)
          args.push_back(*it);
        IMap[&I] = GetElementPtrInst::Create(
            I.getPointerOperandType()->getPointerElementType(),
            I.getPointerOperand(), args, "", &I);
      }
    }
  }

  void visitICmpInst(ICmpInst &I) {
    if (I.getOperand(0)->getType() != I.getOperand(1)->getType()) {
      int nv = -1;
      int cv = -1;
      if (isa<ConstantPointerNull>(I.getOperand(0))) {
        nv = 0;
        cv = 1;
      } else if (isa<ConstantPointerNull>(I.getOperand(1))) {
        nv = 1;
        cv = 0;
      }

      if (nv >= 0) {
        auto null_value = ConstantPointerNull::get(
            cast<PointerType>(I.getOperand(cv)->getType()));
        I.setOperand(nv, null_value);
      } else {
        __error("cannot fix this instruction");
        __dump(I);
      }
    }
  }

  void visitExtractValueInst(ExtractValueInst &EVI) { _handled = true; }

  void visitLoadInst(LoadInst &LI) {
    PointerType *PTy = dyn_cast<PointerType>(LI.getOperand(0)->getType());
    Type *ElTy = PTy->getElementType();
    _handled = ElTy == LI.getType();
  }

  void visitCallInst(CallInst &I) {

    Function *F = I.getCalledFunction();
    if (!F) {
      return;
    }
    if (I.isInlineAsm() || F->isDeclaration())
      return;

    // FIXME: unshure if we can discard intrinsics every time
    if (F->isIntrinsic()) {
      auto ID = F->getIntrinsicID();

      if (ID == Intrinsic::lifetime_start || ID == Intrinsic::lifetime_end) {
        IMap.insert(make_pair(
            &I, CastInst::CreateTruncOrBitCast(
                    I.getOperand(1), I.getOperand(1)->getType(), "", &I)));
        _handled = true;
        return;
      }
    }

    vector<unsigned> usedAS;
    unsigned i = 0;
    for (auto AI = F->arg_begin(), AE = F->arg_end(); AI != AE; ++AI) {
      auto type = I.getArgOperand(i)->getType();
      if (AI->getType() != type) {
        if (type->isPointerTy())
          usedAS.push_back(type->getPointerAddressSpace());
      }
      ++i;
    }
    Function *NF = F;
    CallInst *newCall = &I;
    if (!usedAS.empty()) {
      // FIXME: we have to generate more versions of the function regarding
      // every
      // pointer argument
      std::string NFName = F->getName().str() + std::string("_AS") +
                           std::to_string(usedAS.front());

      NF = _M->getFunction(NFName);
      SmallVector<Value *, 8> args;
      for (i = 0; i != I.getNumArgOperands(); ++i)
        args.push_back(I.getArgOperand(i));

      newCall = CallInst::Create(NF, args, "", &I);
      newCall->setTailCall(I.isTailCall());
    }

    IMap[&I] = newCall;

    _handled = true;
  }

private:
  Module *_M;
  bool _handled;
  AddrSpaceMapper remapper;
  ValueToValueMapTy VMap;
  map<Instruction *, Instruction *> IMap;
  BasicBlock *NewBB;
};
}

#endif
