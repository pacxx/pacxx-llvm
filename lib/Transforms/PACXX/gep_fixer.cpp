// Created by lars

#include <cassert>
#include <iostream>
#include <vector>

#include "llvm/IR/Argument.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/PACXXTransforms.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "CallVisitor.h"
#include "ModuleHelper.h"

using namespace llvm;
using namespace pacxx;

namespace {

    struct GEPPass : public ModulePass {

        static char ID;

        GEPPass() : ModulePass(ID) {}

        virtual ~GEPPass() {}

        bool runOnModule(Module &M) override {

            bool modified = false;

            auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

            SmallVector<GetElementPtrInst *, 8> oldGEPs;

            for (auto &F : kernels) {
                for (auto &B : *F) {
                    for (auto &I : B) {
                        if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(&I)) {

                            bool needToReplace = false;
                            SmallVector<Value *, 8> idx;


                            for (auto II = GEP->idx_begin(), IE = GEP->idx_end(); II != IE; ++II) {
                                Value *val = (*II).get();
                                Type *T = val->getType();

                                if (T->isIntegerTy(64)) {
                                    needToReplace = true;
                                    auto *cast = new TruncInst(val, Type::getInt32Ty(M.getContext()), "cast", GEP);
                                    idx.push_back(cast);
                                } else
                                    idx.push_back(val);
                            }

                            if (needToReplace) {
                                modified = true;
                                auto newGEP =
                                        GetElementPtrInst::Create(GEP->getPointerOperandType()->getPointerElementType(),
                                                                  GEP->getPointerOperand(),
                                                                  idx,
                                                                  GEP->getName(),
                                                                  GEP);
                                GEP->replaceAllUsesWith(newGEP);
                                oldGEPs.push_back(GEP);
                            }
                        }
                    }
                }
            }

            for (auto I : oldGEPs)
                I->eraseFromParent();

            return modified;
        }
    };

    char GEPPass::ID = 0;
    static RegisterPass <GEPPass> X("pacxx-gep", "GEP Pass", false, false);
}

namespace llvm {
    Pass *createPACXXGEPPass() { return new GEPPass(); }
}
