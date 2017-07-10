#include "ModuleHelper.h"

#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {
struct PACXXDeadCodeElim : public ModulePass {
  static char ID;
  PACXXDeadCodeElim() : ModulePass(ID) {}
  virtual ~PACXXDeadCodeElim() {}

  virtual bool runOnModule(Module &M) {

    vector<pair<Instruction *, Instruction *>> to_insert;
    auto visitor = make_CallVisitor([&](CallInst *I) {
      if (!I)
        return;

      if (!I->isInlineAsm()) {

        if (!isa<Function>(I->getCalledValue())) {
        	return;
	}

        if (I->getCalledFunction()->getName().find("native8syscalls6printf") !=
            StringRef::npos) {
          //__dump(*I);
          //	if (auto ASC = dyn_cast<ConstantExpr>(I->getOperand(0)))
          {
            if (auto GEP = dyn_cast<ConstantExpr>(I->getOperand(0))) {
              if (auto str = dyn_cast<GlobalVariable>(GEP->getOperand(0))) {
                str->mutateType(
                    str->getType()->getPointerElementType()->getPointerTo(4));
                auto c0 =
                    ConstantInt::get(Type::getInt64Ty(M.getContext()), 0);
                vector<Value *> idx;
                idx.push_back(c0);
                idx.push_back(c0);
                auto newGEP = GetElementPtrInst::Create(
                    str->getType()->getElementType(), str, idx);
                auto ASC =
                    AddrSpaceCastInst::CreatePointerBitCastOrAddrSpaceCast(
                        newGEP,
                        newGEP->getType()->getSequentialElementType()->getPointerTo());
                I->setOperand(0, ASC);
                to_insert.push_back(
                    pair<Instruction *, Instruction *>(I, newGEP));
                to_insert.push_back(pair<Instruction *, Instruction *>(I, ASC));
              }
            }
          }
          //__dump(*I);
        }
      }
    });
    auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

    for (auto &F : kernels) {
      visitor.visit(F);

      for (auto &p : to_insert) {
        p.second->insertBefore(p.first);
      }

      to_insert.clear();
    }

    //cleanupDeadCode(&M);


    for (auto &F : M) {
      if (std::find(kernels.begin(), kernels.end(), &F) == kernels.end())
        F.setLinkage(GlobalValue::LinkageTypes::InternalLinkage);
    }


    return true;
  }
};

char PACXXDeadCodeElim::ID = 0;
static RegisterPass<PACXXDeadCodeElim>
    X("pacxx_dce", "Removes dead code from kernels' point of view", false,
      false);
}

namespace llvm {
Pass *createPACXXDeadCodeElimPass() { return new PACXXDeadCodeElim(); }
}
