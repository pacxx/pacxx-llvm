#include "ModuleHelper.h"

#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "Log.h"

using namespace llvm;
using namespace pacxx;

namespace {
    class PACXXTargetSelect : public ModulePass {

    public:
        static char ID;

        PACXXTargetSelect(const SmallVector<std::string, 2>& targets) : ModulePass(ID), _targets(targets) {}

        virtual ~PACXXTargetSelect() {}

        virtual bool runOnModule(Module &M) {

            bool modified = false;
            auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");
            for (auto kernel : kernels) {
                if (auto MD = kernel->getMetadata("pacxx.target")) {
                    auto Target = cast<MDString>(MD->getOperand(0).get());
                    if (!supportedTarget(Target)) {
                        kernel->eraseFromParent();
                        modified = true;
                    }
                }
            }
            return modified;
        }

    private:

        bool supportedTarget(MDString *Target) {
            bool supported = false;
            for(auto &target : _targets) {
               if(Target->getString().str() == target)
                   supported = true;
            }
            return supported;
        }

        SmallVector<std::string, 2> _targets;
    };
}

char PACXXTargetSelect::ID = 0;

namespace llvm {
Pass *createPACXXTargetSelectPass(const SmallVector<std::string, 2>& targets) {
    return new PACXXTargetSelect(targets); }
}
