/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2014
*/

#include <vector>
#include <cassert>
#include <algorithm>

#define DEBUG_TYPE "pacxx_classify"

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
#include "llvm/Transforms/PACXXTransforms.h"

#include "ModuleHelper.h"
//#include "../../../kronos/detail/KernelInfo.h"

#define GLOBAL_ID_PATTERN "mad.lo.u32 $0, $1, $2, $3;"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {

enum AccessPattern {
  access_ignored = -1,
  access_unknown = 0,
  access_global_idx_x = 1 << 0,
  access_global_idx_y = access_global_idx_x + 1,
  access_global_idx_z = access_global_idx_x + 2,
  access_thread_idx_x = 1 << 2,
  access_thread_idx_y = access_thread_idx_x + 1,
  access_thread_idx_z = access_thread_idx_x + 2,
  access_block_idx_x = 1 << 3,
  access_block_idx_y = access_block_idx_x + 1,
  access_block_idx_z = access_block_idx_x + 2
};

enum AccessModifier {
  access_mod_read_only = 1,
  access_mod_write_only = 2,
  access_mod_random_access = 0
};

class ArgumentAccess {
public:
  ArgumentAccess(llvm::Value *I, unsigned num, unsigned bytes,
                 AccessPattern pattern = access_unknown,
                 AccessModifier modi = access_mod_random_access)
      : I(I), access(pattern), access_modi(modi), num(num), bytes(bytes) {}

  ArgumentAccess() : ArgumentAccess(nullptr, 0, 0) {}

  void setValue(llvm::Value *argVal) { I = argVal; }
  auto getValue() const { return I; }
  auto getAccess() const { return access; }
  auto setAccess(AccessPattern acc) { access = acc; }
  auto getAccessModifier() const { return access_modi; }
  auto setAccessModifier(AccessModifier mod) { access_modi = mod; }
  auto isReadOnly() const { return access_modi == access_mod_read_only; }
  auto isWriteOnly() const { return access_modi == access_mod_write_only; }
  unsigned getArgNum() const { return num; }
  unsigned getArgBytes() const { return bytes; }
  void setArgBytes(unsigned value) { bytes = value; }

private:
  llvm::Value *I;
  AccessPattern access;
  AccessModifier access_modi;
  unsigned num;
  unsigned bytes;
};


struct PACXXAccessClassifer : public ModulePass {
  static char ID;
  PACXXAccessClassifer() : ModulePass(ID) {}
  virtual ~PACXXAccessClassifer() {}

  virtual bool runOnModule(Module &M) {
    bool modified = true;

    AccessClassifyer opt(&M);
    auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");
    auto &args = opt.getArgVec();
    for (auto &F : M.getFunctionList()) {

      bool isKernel = find(kernels.begin(), kernels.end(), &F) != kernels.end();
      unsigned argc = 0;
      if (isKernel) {
        for (auto &arg : F.args()) {
          if (arg.getType()->isPointerTy()) {
            if (arg.getType()->getPointerAddressSpace() ==
                1) // look only on global memory pointers
            {
              bool onlyGEPs = true;
              for (const auto &u : arg.users()) {
                onlyGEPs &= isa<GetElementPtrInst>(u);
              }
              if (onlyGEPs) {
                args.push_back(ArgumentAccess{&arg, argc, 0});
              }
              else
              {
                  //__debug("Argument: ", arg.getName().str(),
                  //    " discarded not all users are GEPs");
                  args.push_back(ArgumentAccess{ &arg, argc, 0, AccessPattern::access_ignored });
              }
            }
            else
            {
                //__debug("Argument: ", arg.getName().str(),
                //    " discarded not a pointer to AS 1");
                args.push_back(ArgumentAccess{ &arg, argc, 0, AccessPattern::access_ignored });
            }
          }
          else
          {
              //__debug("Argument: ", arg.getName().str(),
              //    " discarded not a pointer type");
              args.push_back(ArgumentAccess{ &arg, argc, 0, AccessPattern::access_ignored });
          }
          ++argc;
        }
        opt.followArguments();
      }

      opt.visit(F);

      if (isKernel) {
        NamedMDNode *pacxxMD = M.getOrInsertNamedMetadata("pacxx.kernel");
        vector<Metadata *> mdArgs;
        mdArgs.push_back(llvm::ConstantAsMetadata::get(&F));
        pacxxMD->addOperand(MDNode::get(M.getContext(), mdArgs));
        NamedMDNode *kernelMD =
            M.getOrInsertNamedMetadata("pacxx.kernel." + F.getName().str());
        argc = 0;
        mdArgs.clear();
        for (auto a : args) {
          mdArgs.push_back(llvm::ConstantAsMetadata::get(ConstantInt::get(
              IntegerType::getInt32Ty(M.getContext()), a.getAccess())));
          // no modifier
          mdArgs.push_back(llvm::ConstantAsMetadata::get(ConstantInt::get(
              IntegerType::getInt32Ty(M.getContext()), a.getAccessModifier())));
          mdArgs.push_back(llvm::ConstantAsMetadata::get(ConstantInt::get(
              IntegerType::getInt32Ty(M.getContext()), a.getArgBytes())));
          //__debug("Argument: ", a.getValue()->getName().str(), " has access ",
          //        a.getAccess(), " and is ", a.getAccessModifier(),
          //        " (load/store size is ", a.getArgBytes(), ")");
        }
        kernelMD->addOperand(MDNode::get(M.getContext(), mdArgs));
      }
      args.clear();
    }

    opt.finalize();

    // FIXME: this is part of the reflection api and should not be in this pass
    // however, there is no better place yet
    std::map<Function *, SmallVector<Metadata *, 3>> reflects;
    Function *PRF = M.getFunction("__pacxx_reflect");
    if (PRF) {
      for (auto U : PRF->users()) {
        SmallVector<Metadata *, 3> MDArgs;
        if (CallInst *I = dyn_cast<CallInst>(U)) {
          Function *K = I->getParent()->getParent();
          if (reflects[K].size() == 0) {
            reflects[K].push_back(
                MDString::get(M.getContext(), "reflected calls"));
          }
          reflects[K].push_back(llvm::ValueAsMetadata::get(I->getOperand(0)));
        }
      }

      for (auto I : reflects) {
        if (NamedMDNode *MD = M.getNamedMetadata("pacxx.kernel." +
                                                 I.first->getName().str())) {
          MD->addOperand(MDNode::get(M.getContext(), I.second));
        }
      }
    }

    // FIXME: find better path to clean SPIR code from reflection
    cleanFromReflections(M);
    return modified;
  }

private:

  class AccessClassifyer : public InstVisitor<AccessClassifyer> {
  public:
    AccessClassifyer(Module *module)
        : M(module), access(AccessPattern::access_unknown) {}

    int switchIntrinsicThreadId(unsigned IID) {
      switch (IID) {
      case Intrinsic::nvvm_read_ptx_sreg_tid_x:
      case Intrinsic::pacxx_read_tid_x:
        return 1;
      case Intrinsic::nvvm_read_ptx_sreg_tid_y:
      case Intrinsic::pacxx_read_tid_y:
        return 2;
      case Intrinsic::nvvm_read_ptx_sreg_tid_z:
      case Intrinsic::pacxx_read_tid_z:
        return 3;
      default:
        return 0;
      }
    }

    int switchIntrinsicBlockDim(unsigned IID) {
      switch (IID) {
      case Intrinsic::nvvm_read_ptx_sreg_ntid_x:
      case Intrinsic::pacxx_read_ntid_x:

        return 1;
      case Intrinsic::nvvm_read_ptx_sreg_ntid_y:
      case Intrinsic::pacxx_read_ntid_y:
        return 2;
      case Intrinsic::nvvm_read_ptx_sreg_ntid_z:
      case Intrinsic::pacxx_read_ntid_z:
        return 3;
      default:
        return 0;
      }
    }

    int switchIntrinsicBlockId(unsigned IID) {
      switch (IID) {
      case Intrinsic::nvvm_read_ptx_sreg_ctaid_x:
      case Intrinsic::pacxx_read_ctaid_x:
        return 1;
      case Intrinsic::nvvm_read_ptx_sreg_ctaid_y:
      case Intrinsic::pacxx_read_ctaid_y:
        return 2;
      case Intrinsic::nvvm_read_ptx_sreg_ctaid_z:
      case Intrinsic::pacxx_read_ctaid_z:
        return 3;
      default:
        return 0;
      }
    }

    void visitCallInst(CallInst &CI) {
      if (CI.isInlineAsm()) {
        access = AccessPattern::access_unknown;
        InlineAsm *Asm = cast<InlineAsm>(CI.getCalledValue());

        if (Asm->getAsmString() == GLOBAL_ID_PATTERN) {
          int found_block_id = 0, found_block_dim = 0, found_thread_id = 0;
          for (const auto &a : CI.arg_operands()) {
            if (CallInst *call = dyn_cast<CallInst>(a)) {
              if (call->getCalledFunction()->isIntrinsic()) {
                auto IID = call->getCalledFunction()->getIntrinsicID();
                if (!found_thread_id)
                  found_thread_id = switchIntrinsicThreadId(IID);
                if (!found_block_dim)
                  found_block_dim = switchIntrinsicBlockDim(IID);
                if (!found_thread_id)
                  found_block_id = switchIntrinsicBlockId(IID);
              }
            }
          }

          if (found_thread_id != 0 && found_block_dim == found_thread_id &&
              found_block_id == found_thread_id) {
            switch (found_thread_id) {
            case 1:
              access = access_global_idx_x;
              break;
            case 2:
              access = access_global_idx_y;
              break;
            case 3:
              access = access_global_idx_z;
              break;
            default:
              llvm_unreachable("unsuspected value provided");
            }

            followUsers(&CI);
          }
        }
      }
      else if (CI.getCalledFunction() && CI.getCalledFunction()->isIntrinsic()) {
        auto IID = CI.getCalledFunction()->getIntrinsicID();
        if (int dir = switchIntrinsicThreadId(IID)) {
          switch (dir) {
          case 1:
            access = access_thread_idx_x;
            break;
          case 2:
            access = access_thread_idx_y;
            break;
          case 3:
            access = access_thread_idx_z;
            break;
          default:
            llvm_unreachable("unsuspected value provided");
          }

          followUsers(&CI);
        } else if (int dir = switchIntrinsicBlockId(IID)) {
          switch (dir) {
      	  case 1:
            access = access_block_idx_x;
            break;
          case 2:
            access = access_block_idx_y;
            break;
          case 3:
            access = access_block_idx_z;
            break;
          default:
            llvm_unreachable("unsuspected value provided");
          }
          followUsers(&CI);
        }
      }
    }
    void finalize() {}

    void followArguments() {
      for (auto &a : args)
        followUsers(a.getValue());
    }

    void followUsers(Value *I) {
      for (const auto &use : I->users()) {
        if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(use)) {
          if (isa<Argument>(GEP->getPointerOperand())) {
            gepMAP.insert(make_pair(GEP, GEP));
            classifyGEP(GEP);
          } else if (BitCastInst *BCI =
                         dyn_cast<BitCastInst>(GEP->getPointerOperand())) {
            if (GetElementPtrInst *IGEP =
                    dyn_cast<GetElementPtrInst>(BCI->getOperand(0))) {
              if (isa<Argument>(IGEP->getPointerOperand())) {
                gepMAP.insert(make_pair(GEP, IGEP));
                classifyGEP(GEP);
              }
            }
          }
          followUsers(use);
        }
        if (isa<CastInst>(use))
          followUsers(use);
      }
    }

    void classifyGEP(GetElementPtrInst *GEP) {
      auto IGEP = gepMAP[GEP];

      unsigned size = 0;
      for (auto &a : args) {
        if (a.getValue() == IGEP->getPointerOperand()) {

          bool allLoads = true, allStores = true, conservative = false;
          for (const auto &u : GEP->users()) {
            bool isLoad = false;
            bool isStore = false;
			
		//	u->dump();
		//	cast<Instruction>(u)->getParent()->getParent()->dump();
            if (LoadInst *LI = dyn_cast<LoadInst>(u)) {
              isLoad = true;
              auto *PTy = LI->getPointerOperand()->getType();
              Type *ETy = dyn_cast<PointerType>(PTy)->getElementType();
              size = M->getDataLayout().getTypeAllocSize(ETy);
            } else if (StoreInst *SI = dyn_cast<StoreInst>(u)) {
              isStore = true;
              auto *PTy = SI->getPointerOperand()->getType();
              Type *ETy = dyn_cast<PointerType>(PTy)->getElementType();
              size = M->getDataLayout().getTypeAllocSize(ETy);

			  //__dump(*SI->getValueOperand());
			  if (auto CI = dyn_cast<CallInst>(SI->getValueOperand()))
			  {
				  // go conservative if we store something from inline PTX
				  conservative = CI->isInlineAsm();
				  //__dump(*CI);
			  }

            }

            if (!isLoad && !isStore) {
              allLoads = false;
              allStores = false;
            }

            allStores &= !isLoad;
            allLoads &= !isStore;
          }
		  if (!conservative)
		  {
			  if (allLoads)
				  a.setAccessModifier(AccessModifier::access_mod_read_only);
			  if (allStores)
				  a.setAccessModifier(AccessModifier::access_mod_write_only);
		  }
		  else
		  {
			  a.setAccessModifier(AccessModifier::access_mod_random_access);
		  }

          if (a.getAccess() == AccessPattern::access_unknown)
            a.setAccess(access);
          a.setArgBytes(size);
        }
      }
    }

    vector<ArgumentAccess> &getArgVec() { return args; }

  private:
    Module *M;

    AccessPattern access;
    vector<ArgumentAccess> args;
    map<GetElementPtrInst *, GetElementPtrInst *> gepMAP;
  };


   void cleanFromReflections(Module &M) {
    auto reflections = pacxx::getTagedFunctions(&M, "pacxx.reflection", "");
  
    for (auto F : reflections) {
      F->replaceAllUsesWith(UndefValue::get(F->getType()));
      F->eraseFromParent();
    }

    vector<GlobalValue *> globals;
    for (auto &G : M.getGlobalList()) {
        if (GlobalVariable* GV = dyn_cast<GlobalVariable>(&G)){
            if (GV->getVisibility() == GlobalValue::ProtectedVisibility){
                G.removeDeadConstantUsers();
                G.replaceAllUsesWith(UndefValue::get(G.getType()));
                globals.push_back(&G);

            }
        }
    }

    for (auto G : globals)
        G->eraseFromParent();

  }

};

char PACXXAccessClassifer::ID = 0;
static RegisterPass<PACXXAccessClassifer> X("pacxx_classify",
                                            "PACXXAccessClassifer: path "
                                            "classify memory access patterns "
                                            "on kernel parameters",
                                            false, false);
}

namespace llvm {
Pass *createPACXXClassifyPass() { return new PACXXAccessClassifer(); }
}
