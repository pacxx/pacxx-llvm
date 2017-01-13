// Created by lars

#define PACXX_PASS_NAME "PACXXAddrSpaceTransformer"
#include "Log.h"

#include <llvm/Transforms/Vectorize.h>

#include "ModuleHelper.h"
#include "CallVisitor.h"

#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"


using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {
    struct PACXXAddrSpaceTransform : public ModulePass {
        static char ID;

        PACXXAddrSpaceTransform() : ModulePass(ID) {}

        virtual ~PACXXAddrSpaceTransform() {}

        virtual bool runOnModule(Module &M) {
            auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");
            for (auto &F : kernels) {
                AddrSpaceTransformer transformer;
                transformer.runOn(*F);
            }
            return true;
        }

        class AddrSpaceTransformer {
        public:
            AddrSpaceTransformer() {}

            void runOn(Function& F) {

                BasicBlock* functionEntry = &F.front();

                BasicBlock* BB = BasicBlock::Create(F.getContext(), "addrspacecast", &F, functionEntry);

                for (Function::arg_iterator I = F.arg_begin(),
                             E = F.arg_end(); I != E; ++I) {
                    if ((*I).getType()->isPointerTy()) {
                        PointerType* ptrType = dyn_cast<PointerType>((*I).getType());
                        if (ptrType->getAddressSpace() != 0) {
                            AddrSpaceCastInst* cast = insertAddrSpaceCast(*I, BB);
                            if(gatherAndReplaceAddrSpaceCasts(*I, cast))
                                cast->eraseFromParent();
                        }
                    } else
                        continue;
                }

                for(auto C : _deleteInstructions)
                    C->eraseFromParent();

                if(BB->getInstList().size() > 0)
                    BranchInst::Create(functionEntry, BB);
                else
                    BB->eraseFromParent();
            }

        private:

            bool gatherAndReplaceAddrSpaceCasts(Argument& arg, AddrSpaceCastInst* cast) {
                bool replaced = false;
                for (auto AU : arg.users()) {
                    if (GetElementPtrInst* GEP = dyn_cast<GetElementPtrInst>(AU)) {
                        std::vector<Value *> idx;
                        for(unsigned i = 1; i < GEP->getNumOperands(); ++i)
                            idx.push_back(GEP->getOperand(i));
                        GetElementPtrInst* newGEP = GetElementPtrInst::CreateInBounds(
                                cast, idx, "", GEP);
                        _deleteInstructions.push_back(GEP);
                        for (auto GEPU : GEP->users()) {
                            if (PtrToIntInst* ptrToInt = dyn_cast<PtrToIntInst>(GEPU)) {
                                _deleteInstructions.push_back(ptrToInt);
                                for (auto PTIU : ptrToInt->users()) {
                                    if (IntToPtrInst* intToPtr = dyn_cast<IntToPtrInst>(PTIU)) {
                                        intToPtr->replaceAllUsesWith(GEP);
                                        _deleteInstructions.push_back(intToPtr);
                                    }
                                }
                            }
                        }
                        replaced = true;
                        GEP->replaceAllUsesWith(newGEP);
                    }

                    if(PtrToIntInst *ptrToInt = dyn_cast<PtrToIntInst>(AU)) {
                       for (auto PTIU : ptrToInt->users()) {
                            if (IntToPtrInst* intToPtr = dyn_cast<IntToPtrInst>(PTIU)) {
                                replaced = true;
                                intToPtr->replaceAllUsesWith(cast);
                                _deleteInstructions.push_back(intToPtr);
                            }
                        }
                        _deleteInstructions.push_back(ptrToInt);
                    }
                }
                return replaced;
            }

            AddrSpaceCastInst* insertAddrSpaceCast(Argument& arg, BasicBlock* BB) {
                PointerType* ptrType = dyn_cast<PointerType>(arg.getType());
                AddrSpaceCastInst* cast = new AddrSpaceCastInst(&arg, PointerType::get(ptrType->getElementType(), 0), "", BB);
                return cast;
            }

        private:

            vector<Instruction *> _deleteInstructions;
        };
    };


    char PACXXAddrSpaceTransform::ID = 0;
}

namespace llvm {
    Pass *createPACXXAddrSpaceTransformPass() { return new PACXXAddrSpaceTransform(); }
}
