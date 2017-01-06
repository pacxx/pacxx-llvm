//IMPORTANT needs to be run before any modifications are made to the wrapper function foo
//
// Created by lars on 17/11/16.

#include "Log.h"
#include "llvm/Pass.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/PassSupport.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Transforms/Vectorize.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/Support/Host.h"
#include "llvm/Transforms/PACXXTransforms.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "ModuleHelper.h"
#include "llvm/Transforms/PACXXTransforms.h"
#include "wfv/wfvInterface.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {

    class SPMDVectorizer : public llvm::ModulePass {

    public:
        static char ID;

        SPMDVectorizer() : llvm::ModulePass(ID) { initializeSPMDVectorizerPass(*PassRegistry::getPassRegistry()); }

        virtual ~SPMDVectorizer() {}

        void releaseMemory() override;

        void getAnalysisUsage(AnalysisUsage& AU) const override;

        bool runOnModule(llvm::Module& M) override;

    private:

        Function *createVectorizedKernelHeader(Module *M, Function *kernel);

        unsigned determineVectorWidth(Function *F, unsigned registerWidth);

        bool modifyWrapperLoop(unsigned vectorWidth, Module& M, bool requiresSequentialVersion);

        bool modifyIncrementationOfX(Module &M, Function *foo, Value *__x, unsigned vectorWidth);

        bool modifyOldLoop(Module &M);

        BasicBlock *determineOldLoopPreHeader(Module& M);

        Value *determineMaxx(Function *F);

        Value *determine_x(Function *F);

    };
}

void SPMDVectorizer::releaseMemory() {}

void SPMDVectorizer::getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequired<LoopInfoWrapperPass>();
    AU.addRequired<TargetTransformInfoWrapperPass>();
}

bool SPMDVectorizer::runOnModule(Module& M) {

    bool kernelsVectorized = true;

    auto kernels = getTagedFunctions(&M, "nvvm.annotations", "kernel");

    for (auto kernel : kernels) {

        bool vectorized = false;
        bool requiresSequentialVersion = true;

        TargetTransformInfo* TTI = &getAnalysis<TargetTransformInfoWrapperPass>().getTTI(*kernel);

        unsigned vectorWidth = determineVectorWidth(kernel, TTI->getRegisterBitWidth(true));

        __verbose("vectorWidth: ", vectorWidth);

        if(kernel->hasFnAttribute("num-threads")) {
            int numThreads = stoi(kernel->getFnAttribute("num-threads").getValueAsString().str());
            __verbose("number of threads: ", numThreads);
            requiresSequentialVersion = numThreads % vectorWidth != 0;
        }

        //TODO REMOVE !!!!!
        requiresSequentialVersion = false;

        __verbose("requires sequential version: ", requiresSequentialVersion);


        Function *vectorizedKernel  = createVectorizedKernelHeader(&M, kernel);

        WFVInterface::WFVInterface wfv(&M,
                                       &M.getContext(),
                                       kernel,
                                       vectorizedKernel,
                                       TTI,
                                       vectorWidth,
                                       -1,
                                       false,
                                       false,
                                       false,
                                       true,
                                       false);

        vectorized = wfv.run();
        //vectorized = wfv.analyze();

        if(vectorized) {
            modifyWrapperLoop(vectorWidth, M, requiresSequentialVersion);
            vectorizedKernel->addFnAttr("simd-size", to_string(vectorWidth));
        }
        else
            vectorizedKernel->eraseFromParent();

        kernelsVectorized &= vectorized;
    }

    return kernelsVectorized;
}

unsigned SPMDVectorizer::determineVectorWidth(Function *F, unsigned registerWidth) {
    unsigned MaxWidth = 8;
    const DataLayout &DL = F->getParent()->getDataLayout();

    for (auto &B : *F) {
        for (auto &I : B) {
            Type *T = I.getType();

            if (!isa<LoadInst>(&I) && !isa<StoreInst>(&I) && !isa<PHINode>(&I))
                continue;

            if (StoreInst * ST = dyn_cast<StoreInst>(&I))
                T = ST->getValueOperand()->getType();

            // Ignore loaded pointer types
            if (T->isPointerTy())
                continue;

            MaxWidth = std::max(MaxWidth, (unsigned) DL.getTypeSizeInBits(T->getScalarType()));
        }
    }
    return registerWidth / MaxWidth;
}

Function *SPMDVectorizer::createVectorizedKernelHeader(Module *M, Function *kernel) {

    Function* vectorizedKernel = Function::Create(kernel->getFunctionType(), GlobalValue::LinkageTypes::ExternalLinkage,
                                           std::string("__vectorized__") + kernel->getName().str(), M);

    Function::arg_iterator AI = vectorizedKernel->arg_begin();
    for(const Argument& I : kernel->args()) {
        AI->setName(I.getName());
    }

    return vectorizedKernel;
}

bool SPMDVectorizer::modifyWrapperLoop(unsigned vectorWidth, Module& M, bool requiresSequentialVersion) {

    auto& ctx = M.getContext();
    auto int32_type = Type::getInt32Ty(ctx);

    Function* F = M.getFunction("foo");
    Function* dummyFunction = M.getFunction("__dummy_kernel");
    Function* vecDummyCall = Function::Create(dummyFunction->getFunctionType(),
                                              GlobalValue::LinkageTypes::ExternalLinkage,
                                              "__vectorized__dummy_kernel", &M);
    BasicBlock* oldLoopHeader = determineOldLoopPreHeader(M);
    if(!oldLoopHeader)
        return false;

    Value* maxx = determineMaxx(F);
    if(!maxx)
        return false;

    Value* __x = determine_x(F);
    if(!__x)
        return false;

    // if we dont need a sequential version just replace the call and change the loop var incrementation
    if(!requiresSequentialVersion) {
        modifyIncrementationOfX(M, F, __x, vectorWidth);
        CallInst *remove = nullptr;
        for (auto U : dummyFunction->users()) {
            if (auto CI = dyn_cast<CallInst>(U)) {
                if (CI->getParent()->getParent() == F) {
                    std::vector < Value *> args;
                    for (Value *argOperand : CI->arg_operands()) {
                        args.push_back(argOperand);
                    }
                    CallInst::Create(vecDummyCall, args, "", CI);
                    remove = CI;
                }
            }
        }
        remove->eraseFromParent();
        dummyFunction->eraseFromParent();
        return true;
    }

    modifyOldLoop(M);

    // construct required BasicBlocks
    BasicBlock* loopEnd = BasicBlock::Create(ctx, "loop-end", F, oldLoopHeader);
    BasicBlock* loopBody = BasicBlock::Create(ctx, "loop-body", F, loopEnd);
    BasicBlock* loopHeader = BasicBlock::Create(ctx, "loop-header", F, loopBody);
    BasicBlock* loopPreHeader = BasicBlock::Create(ctx, "pre-header", F, loopHeader);

    //modify predecessor of oldLoopPreHeader to branch into the newLoopPreHeader
    BasicBlock* predecessor = oldLoopHeader->getUniquePredecessor();
    if(BranchInst* BI = dyn_cast<BranchInst>(predecessor->getTerminator()))
        for(unsigned i = 0; i < BI->getNumSuccessors(); ++i) {
            if(BI->getSuccessor(i) == oldLoopHeader)
                BI->setSuccessor(i, loopPreHeader);
        }

    //insert instructions into loop preHeader
    new StoreInst(ConstantInt::get(int32_type, 0), __x, loopPreHeader);
    BranchInst::Create(loopHeader, loopPreHeader);

    //insert Instruction into loop header
    LoadInst* headerLoadVar = new LoadInst(__x, "loadVar", loopHeader);
    LoadInst* loadMaxx = new LoadInst(maxx, "loadmaxx", loopHeader);
    Instruction* inc = BinaryOperator::CreateAdd(headerLoadVar, ConstantInt::get(int32_type, vectorWidth),
                                                 "increment loop var", loopHeader);
    ICmpInst* varLessThanMaxx = new ICmpInst(*loopHeader, ICmpInst::ICMP_SLT, inc, loadMaxx, "cmp");
    BranchInst::Create(loopBody, oldLoopHeader, varLessThanMaxx, loopHeader);


    std::vector<Value *> args;
    std::vector<Instruction *> instructionsToMove;

    for (auto U : dummyFunction->users()) {
        if (CallInst* CI = dyn_cast<CallInst>(U)) {
            for(auto &I : *(CI->getParent())) {
                if (!isa<CallInst>(&I) &&
                        !isa<TerminatorInst>(&I))
                    instructionsToMove.push_back(&I);
            }
            for(Value* argOperand : CI->arg_operands()) {
                args.push_back(argOperand);
            }
        }
    }

    CallInst* vecFunction = CallInst::Create(vecDummyCall, args, "", loopBody);

    for (auto U : dummyFunction->users()) {
        if (CallInst* CI = dyn_cast<CallInst>(U)) {
            CI->dump();
            __verbose("num args ", CI->getNumArgOperands(), "\n");
            for(Value *arg : CI->arg_operands()) {
                if (ZExtInst *ZI = dyn_cast<ZExtInst>(arg))
                    if (LoadInst *LI = dyn_cast<LoadInst>(ZI->getOperand(0)))
                        if (LI->getPointerOperand()->getName() == "__x" ||
                                LI->getPointerOperand()->getName() == "__y" ||
                                LI->getPointerOperand()->getName() == "__z") {
                            LoadInst *copy = new LoadInst(LI->getPointerOperand(), "copied load", CI);
                            ZExtInst *zext = new ZExtInst(copy, arg->getType(), "", CI);
                            CI->replaceUsesOfWith(arg, zext);
                        }
            }
        }
    }

    //move load of Params
    for(auto I : instructionsToMove) {
        I->moveBefore(vecFunction);
    }

    BranchInst::Create(loopEnd, loopBody);

    //insert instructions into loop end
    LoadInst* loadLoopVar = new LoadInst(__x, "loadVar", loopEnd);
    Instruction* incLoopEnd = BinaryOperator::CreateAdd(loadLoopVar, ConstantInt::get(int32_type, vectorWidth),
                                                        "increment loop var", loopEnd);
    new StoreInst(incLoopEnd, __x, loopEnd);
    BranchInst::Create(loopHeader, loopEnd);

    return true;
}

bool SPMDVectorizer::modifyIncrementationOfX(Module &M, Function *foo, Value *__x, unsigned vectorWidth) {

    LoopInfo* LI = &getAnalysis<LoopInfoWrapperPass>(*foo).getLoopInfo();

    // we know that there are only 3 nested loops
    Loop* xLoop = ((*LI->begin())->getSubLoops().front())->getSubLoops().front();
    // search in all blocks of the loop for the incrementation of x
    for(auto *block : xLoop->getBlocks()) {
        for(auto &inst : *block) {
            if(inst.getOpcode() == Instruction::Add) {
                if(isa<Constant>(inst.getOperand(1)))
                    if(LoadInst *loadX = dyn_cast<LoadInst>(inst.getOperand(0))) {
                        if(loadX->getPointerOperand() == __x) {
                            BinaryOperator *incVecWidth = BinaryOperator::CreateAdd(loadX,
                                                                                    ConstantInt::get(Type::getInt32Ty(M.getContext()),
                                                                                                     vectorWidth),
                                                                                    "inc",
                                                                                    &inst);
                            inst.replaceAllUsesWith(incVecWidth);
                            inst.eraseFromParent();
                            return true;
                        }
                    }
            }
        }
    }
    return false;
}

bool SPMDVectorizer::modifyOldLoop(Module &M) {

    std::vector<StoreInst *> storesToRemove;

    BasicBlock* oldLoopHeader = determineOldLoopPreHeader(M);
    if(!oldLoopHeader)
        return false;

    for (auto &I : *(oldLoopHeader)) {
        if(StoreInst* SI = dyn_cast<StoreInst>(&I)) {
            if(SI->getPointerOperand()->getName() == "__x") {
                storesToRemove.push_back(SI);
            }
        }
    }

    for(auto SI : storesToRemove) {
        SI->eraseFromParent();
    }

    return true;
}

BasicBlock * SPMDVectorizer::determineOldLoopPreHeader(Module& M) {
    Function* F = M.getFunction("foo");
    LoopInfo* LI = &getAnalysis<LoopInfoWrapperPass>(*F).getLoopInfo();
    // we know that there are only 3 nested loops
    Loop* xLoop = ((*LI->begin())->getSubLoops().front())->getSubLoops().front();
    BasicBlock* loopPreHeader = xLoop->getLoopPredecessor();
    return loopPreHeader;
}

Value * SPMDVectorizer::determineMaxx(Function *F) {
    Value* maxx;
    Function::arg_iterator argIt = F->arg_begin();
    if(!((&*argIt)->getName() == "__maxx"))
        ++argIt;
    for (auto U : (&*argIt)->users()) {
        if (StoreInst* SI = dyn_cast<StoreInst>(U)) {
            maxx = SI->getPointerOperand();
        }
    }
    return maxx;
}

Value *SPMDVectorizer::determine_x(Function *F) {
    Value* __x;
    for (auto &B : *F) {
        for (auto &I : B) {
            if (AllocaInst * alloca = dyn_cast<AllocaInst>(&I))
                if (alloca->getName() == "__x")
                    __x = alloca;
        }
    }
    return __x;
}

char SPMDVectorizer::ID = 0;

INITIALIZE_PASS_BEGIN(SPMDVectorizer, "spmd",
                "SPMD vectorizer", true, true)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetTransformInfoWrapperPass)
INITIALIZE_PASS_END(SPMDVectorizer, "spmd",
                "SPMD vectorizer", true, true)

namespace llvm {
    llvm::Pass *createSPMDVectorizer() {
        return new SPMDVectorizer();
    }
}
