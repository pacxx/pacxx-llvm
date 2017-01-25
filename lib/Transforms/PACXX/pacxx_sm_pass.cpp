//
// Created by lars

#include "pacxx_sm_pass.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace llvm {
    void initializePACXXNativeSMTransformerPass(PassRegistry&);
}

PACXXNativeSMTransformer::PACXXNativeSMTransformer() : FunctionPass(ID) {
    initializePACXXNativeSMTransformerPass(*PassRegistry::getPassRegistry());
}

PACXXNativeSMTransformer::~PACXXNativeSMTransformer() {}

void PACXXNativeSMTransformer::releaseMemory() {}

void PACXXNativeSMTransformer::getAnalysisUsage(AnalysisUsage &AU) const {}


//TODO test
bool PACXXNativeSMTransformer::runOnFunction(Function &F) {

    auto argIt = F.arg_end();

    // the sm_size is always the second last arg
    Value *sm_size = &*(--(--argIt));

    createSharedMemoryBuffer(&F, sm_size);

    return true;
}

void PACXXNativeSMTransformer::createSharedMemoryBuffer(Function *func, Value *sm_size) {

    Module *M = func->getParent();

    auto internal_sm = getSMGlobalsUsedByKernel(M, func, true);
    auto external_sm = getSMGlobalsUsedByKernel(M, func, false);

    if(!internal_sm.empty() || !external_sm.empty()) {
        BasicBlock *entry = &func->front();
        BasicBlock *sharedMemBB = BasicBlock::Create(func->getContext(), "shared mem", func, entry);

        if (!internal_sm.empty()) {
            __verbose("internal shared memory found\n");
            createInternalSharedMemoryBuffer(*M, internal_sm, sharedMemBB);
        }

        if (!external_sm.empty()) {
            __verbose("external shared memory found\n");
            createExternalSharedMemoryBuffer(*M, external_sm, sm_size, sharedMemBB);
        }

        BranchInst::Create(entry, sharedMemBB);

        __verbose("created shared memory");
    }
}

set<GlobalVariable *> PACXXNativeSMTransformer::getSMGlobalsUsedByKernel(Module *M, Function *func, bool internal) {
    set<GlobalVariable *> sm;
    for (auto &GV : M->globals()) {
        if (internal ? GV.hasInternalLinkage() : GV.hasExternalLinkage() && GV.getMetadata("pacxx.as.shared")) {
            for (User *GVUsers : GV.users()) {
                if (Instruction *Inst = dyn_cast<Instruction>(GVUsers)) {
                    if (Inst->getParent()->getParent() == func) {
                        sm.insert(&GV);
                    }
                }

                if(ConstantExpr *constExpr = dyn_cast<ConstantExpr>(GVUsers)) {
                    vector<ConstantUser> smUsers = findInstruction(func, constExpr);
                    set<Constant *> constantsToRemove;
                    for(auto &smUser : smUsers) {
                        auto inst = smUser._inst;
                        for(auto constant : smUser._constants) {
                            Instruction *constInst = constant->getAsInstruction();
                            constInst->insertBefore(inst);
                            inst->replaceUsesOfWith(constant, constInst);
                            inst = constInst;
                            constantsToRemove.insert(constant);
                        }
                    }

                    for(auto constant : constantsToRemove) {
                        constant->dropAllReferences();
                    }

                    sm.insert(&GV);
                }
            }
        }
    }
    return sm;
}

vector<PACXXNativeSMTransformer::ConstantUser> PACXXNativeSMTransformer::findInstruction(Function *func, ConstantExpr * constExpr) {
    vector<ConstantUser> smUsers;
    for (auto &B : *func) {
        for (auto &I : B) {
            vector<ConstantExpr *> constants;
            vector<ConstantExpr *> tmp;
            Instruction *inst = &I;
            for (auto &op : inst->operands()) {
                if (ConstantExpr *opConstant = dyn_cast<ConstantExpr>(op.get())) {
                    bool usesSM = false;
                    if (opConstant == constExpr) {
                        constants.push_back(opConstant);
                        smUsers.push_back(ConstantUser(inst, constants));
                    }
                    else {
                        tmp.push_back(opConstant);
                        lookAtConstantOps(opConstant, constExpr, tmp, constants, &usesSM);
                        if (usesSM) {
                            smUsers.push_back(ConstantUser(inst, constants));
                        }
                    }
                }
            }
        }
    }
    return smUsers;
}

void PACXXNativeSMTransformer::lookAtConstantOps(ConstantExpr *constExp, ConstantExpr *smUser,
                                                 vector<ConstantExpr *> &tmp,
                                                 vector<ConstantExpr *> &constants,
                                                 bool *usesSM) {
   for(auto &op : constExp->operands()) {
       if(ConstantExpr *opConstant = dyn_cast<ConstantExpr>(op.get())) {
           if(opConstant == smUser) {
               for(auto constant : tmp) {
                   constants.push_back(constant);
               }
               constants.push_back(opConstant);
               *usesSM = true;
               return;
           }

           tmp.push_back(opConstant);
           lookAtConstantOps(opConstant, smUser, tmp, constants, usesSM);
       }
   }
}

void PACXXNativeSMTransformer::createInternalSharedMemoryBuffer(Module &M, set<GlobalVariable *> &globals,
                                                                BasicBlock *sharedMemBB) {

    for (auto GV : globals) {
        Type *sm_type = GV->getType()->getElementType();
        AllocaInst *sm_alloc = new AllocaInst(sm_type, nullptr,
                                              M.getDataLayout().getABITypeAlignment(sm_type), "internal_sm",
                                              sharedMemBB);

        if (GV->hasInitializer() && !isa<UndefValue>(GV->getInitializer()))
            new StoreInst(GV->getInitializer(), sm_alloc, sharedMemBB);

        GV->replaceAllUsesWith(sm_alloc);
    }
}

// Currently we aren't handling shared_memory<type>, because it will be removed from pacxx soon
void PACXXNativeSMTransformer::createExternalSharedMemoryBuffer(Module &M, set<GlobalVariable *> &globals,
                                                                Value *sm_size, BasicBlock *sharedMemBB) {
    for (auto GV : globals) {
        Type *GVType = GV->getType()->getElementType();
        Type *sm_type = nullptr;

        sm_type = GVType->getPointerElementType();

        Value *typeSize = ConstantInt::get(Type::getInt32Ty(M.getContext()),
                                           M.getDataLayout().getTypeSizeInBits(sm_type) / 8);

        //calc number of elements
        BinaryOperator *div = BinaryOperator::CreateUDiv(sm_size, typeSize, "numElem", sharedMemBB);
        AllocaInst *sm_alloc = new AllocaInst(sm_type, div, M.getDataLayout().getABITypeAlignment(sm_type),
                                              "external_sm", sharedMemBB);

        AllocaInst *ptrToSM = new AllocaInst(PointerType::get(sm_type, 0), nullptr, "ptrToSM", sharedMemBB);
        new StoreInst(sm_alloc, ptrToSM, sharedMemBB);

        GV->replaceAllUsesWith(ptrToSM);
    }
}

namespace llvm {
    Pass* createPACXXNativeSMPass() { return new PACXXNativeSMTransformer(); }
}

char PACXXNativeSMTransformer::ID = 0;

INITIALIZE_PASS_BEGIN(PACXXNativeSMTransformer, "native-sm", "creation of shared memory", true, true)
INITIALIZE_PASS_END(PACXXNativeSMTransformer, "native-sm", "creation of shared memory", true, true)


