// Created by lars

#define PACXX_PASS_NAME "PACXXAddrSpaceTransformer"
#include "Log.h"

#include <llvm/Transforms/Vectorize.h>

#include "ModuleHelper.h"
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

                _deleteInstructions.clear();

                BasicBlock* functionEntry = &F.front();

                BasicBlock* BB = BasicBlock::Create(F.getContext(), "addrspacecast", &F, functionEntry);

                for (Function::arg_iterator I = F.arg_begin(), E = F.arg_end(); I != E; ++I) {
                    if ((*I).getType()->isPointerTy()) {
                        PointerType* ptrType = dyn_cast<PointerType>((*I).getType());
                        if (ptrType->getAddressSpace() != 0) {
                            AddrSpaceCastInst* cast = insertAddrSpaceCast(*I, BB);
                            if(!gatherAndReplaceAddrSpaceCasts(*I, cast))
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

                Type *oldType = arg.getType();
                arg.mutateType(cast->getType());
                for (auto user : arg.users()) {
                    if (!isa<AddrSpaceCastInst>(user)) {
                        replaced = true;
                        user->replaceUsesOfWith(&arg, cast);
                        user->mutateType(cast->getType());
                        for (auto Uuser : user->users()) {
                            if (isa<AddrSpaceCastInst>(Uuser)) {
                                Uuser->mutateType(user->getType());
                                Uuser->replaceAllUsesWith(user);
                                _deleteInstructions.push_back(llvm::cast<AddrSpaceCastInst>(Uuser));
                            }
                        }
                    }
                }

                arg.mutateType(oldType);
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
