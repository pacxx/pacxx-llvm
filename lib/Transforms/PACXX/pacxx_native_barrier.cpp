// Created by lars

#include "Log.h"

#include "llvm/Pass.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/ADT/SCCIterator.h"
#include "../../IR/LLVMContextImpl.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {

    class PACXXNativeBarrier : public llvm::ModulePass {

    public:
        static char ID;

        PACXXNativeBarrier() : llvm::ModulePass(ID) { initializePACXXNativeBarrierPass(*PassRegistry::getPassRegistry()); }

        virtual ~PACXXNativeBarrier() {};

        void releaseMemory() override;

        void getAnalysisUsage(AnalysisUsage& AU) const override;

        bool runOnModule(llvm::Module& M) override;

    private:

        struct BarrierInfo {
            BarrierInfo(const unsigned id,
                        const Instruction *barrier,
                        const BasicBlock *entry,
                        SetVector<const BasicBlock *> parts,
                        SetVector<const Value*> livingValues,
                        StructType *livingValuesType)
                    : _id(id),
                      _barrier(barrier),
                      _entry(entry),
                      _parts(parts),
                      _livingValues(livingValues),
                      _livingValuesType(livingValuesType),
                      _func(nullptr) {}

            ~BarrierInfo() {}

            const unsigned _id;
            const Instruction *_barrier;
            const BasicBlock *_entry;
            SetVector<const BasicBlock *> _parts;
            SetVector<const Value *> _livingValues;
            StructType *_livingValuesType;

            Function *_func;
            ValueToValueMapTy _VMap;

            string toString() {
                string text;
                raw_string_ostream ss(text);

                ss << "barrier with id: " << _id << "\n";

                for(auto part : _parts) {
                    ss << part->getName().str();
                    ss << "\n";
                }

                ss << "\n";
                ss << "living values: \n";
                for(auto value : _livingValues) {
                    value->print(ss, true);
                    ss << "\n";
                }

                return ss.str();
            }
        };

        struct Alloca3 {

            Alloca3(AllocaInst *x, AllocaInst *y, AllocaInst *z) : _x(x), _y(y), _z(z) {}

            ~Alloca3() {}

            AllocaInst *_x;
            AllocaInst *_y;
            AllocaInst *_z;

        };

        struct CaseInfo {

            CaseInfo(unsigned id,
                     Function *foo,
                     BasicBlock *switchBB,
                     BasicBlock *breakBB,
                     AllocaInst *switchParam,
                     Alloca3 &max,
                     pair<AllocaInst*, AllocaInst*> loadStruct,
                     pair<AllocaInst*, AllocaInst*> nextStruct,
                     pair<StructType*, StructType*> loadType,
                     pair<Function*, Function*> calledFunctions)
                    : _id(id),
                      _foo(foo),
                      _switchBB(switchBB),
                      _breakBB(breakBB),
                      _switchParam(switchParam),
                      _max3(max),
                      _loadStruct(loadStruct),
                      _nextStruct(nextStruct),
                      _loadType(loadType),
                      _calledFunctions(calledFunctions) {}

            ~CaseInfo() {}

            const unsigned _id;
            Function *_foo;
            BasicBlock *_switchBB;
            BasicBlock *_breakBB;
            AllocaInst *_switchParam;
            const Alloca3 _max3;
            const pair<AllocaInst*, AllocaInst *> _loadStruct;
            const pair<AllocaInst *, AllocaInst *> _nextStruct;
            const pair<StructType*, StructType*> _loadType;
            const pair<Function *, Function *> _calledFunctions;
        };

        unsigned getVectorWidth(Function *kernel);

        bool runOnFunction(Module &M, Function *kernel, SetVector<BarrierInfo *> &infoVec, bool vecVersion = false);

        vector<Instruction *> findBarriers(Function *kernel);

        bool isaBarrier(const Instruction *inst);

        void splitAtBarriers(Module &M, vector<Instruction *> barriers);

        BarrierInfo* createFirstInfo(LLVMContext &ctx, Function *kernel);

        BarrierInfo* createBarrierInfo(LLVMContext &ctx, Instruction *barrier, unsigned id);

        SetVector<const BasicBlock *> getPartsOfBarrier(Instruction *barrier);

        void recursivePartFinding(BasicBlock *block, SetVector<const BasicBlock *> &parts);

        bool hasBarrier(const BasicBlock *block);

        SetVector<const Value *> getLivingValuesForBarrier(SetVector<const BasicBlock *> &parts);

        bool definedInParts(SetVector<const BasicBlock *> &parts, Value * value);

        StructType *getLivingValuesType(LLVMContext &ctx, SetVector<const Value *> &livingValues);

        Function *createFunction(Module &M, Function *kernel, BarrierInfo *info);

        void storeLiveValues(Module &M, BarrierInfo *info);

        void prepareFunctionForLinker(Module &M, Function *oldFunc, Function *newFunc);

        void createSpecialFooWrapper(Module &M, Function *foo,
                                     SetVector<BarrierInfo *> barrierInfo,
                                     SetVector<BarrierInfo *> vecBarrierInfo);

        AllocaInst *createMemForLivingValues(const DataLayout &dl, BarrierInfo *info, Value *numThreads, BasicBlock *BB);

        BasicBlock *createCase(Module &M, const CaseInfo &info, bool vectorized);

        pair<BasicBlock *, BasicBlock*> createXLoop(Module &M,
                                                    const CaseInfo &info,
                                                    Alloca3 id,
                                                    BasicBlock *afterBB,
                                                    BasicBlock *falseBB,
                                                    bool vectorized);
        void fillLoopXBody(Module &M,
                           const CaseInfo &info,
                           BasicBlock *loopBody,
                           BasicBlock *nextBB,
                           Alloca3 &id,
                           bool vectorized);

        void fillLoopHeader(BasicBlock *loopHeader, AllocaInst *loopVar, AllocaInst *loopMax,
                                 BasicBlock *trueBB, BasicBlock *falseBB);

        void fillLoopEnd(BasicBlock *loopEnd, BasicBlock *branchBB, AllocaInst *loopValue, Type *int32_type,
                         unsigned incVal);

    private:
        ValueToValueMapTy _origFnValueMap;
        vector<CallInst *> _inlineCalls;
        unsigned _vectorWidth;
    };
}

void PACXXNativeBarrier::releaseMemory() {}

void PACXXNativeBarrier::getAnalysisUsage(AnalysisUsage &AU) const {}

bool PACXXNativeBarrier::runOnModule(llvm::Module &M) {

    auto kernels = getTagedFunctions(&M, "nvvm.annotations", "kernel");

    auto foo = M.getFunction("foo");

    bool modified = false;

    for(auto kernel : kernels) {

        SetVector<BarrierInfo *> barrierInfo;
        SetVector<BarrierInfo *> vecBarrierInfo;

        bool modified_kernel = runOnFunction(M, kernel, barrierInfo);

        if(modified_kernel) {
            //if we have a vectorized version of the kernel also eliminate barriers
            auto vec_kernel = M.getFunction("__vectorized__" + kernel->getName().str());

            if (vec_kernel) {
                _vectorWidth = getVectorWidth(vec_kernel);
                _origFnValueMap.clear();
                runOnFunction(M, vec_kernel, vecBarrierInfo, true);
            }

            createSpecialFooWrapper(M, foo, barrierInfo, vecBarrierInfo);

            //now we can delete the original function and its vectorized version
            kernel->dropAllReferences();
            kernel->eraseFromParent();

            if (vec_kernel) {
                vec_kernel->dropAllReferences();
                vec_kernel->eraseFromParent();
            }
        }

        modified |= modified_kernel;
    }

    return modified;
}

unsigned PACXXNativeBarrier::getVectorWidth(Function *kernel) {
    unsigned numThreads = stoi(kernel->getFnAttribute("simd-size").getValueAsString().str());
    return numThreads;
}

bool PACXXNativeBarrier::runOnFunction(Module &M, Function *kernel, SetVector<BarrierInfo *> &infoVec,
                                       bool vecVersion) {

    LLVMContext &ctx = M.getContext();

    auto barriers = findBarriers(kernel);

    unsigned numBarriers = barriers.size();

    if(numBarriers == 0) {
        __verbose("no barriers found \n");
        return false;
    }

    __verbose("Found ", numBarriers, " barriers. Modifying \n");

    splitAtBarriers(M, barriers);

    infoVec.insert(createFirstInfo(ctx, kernel));

    unsigned id = 1;
    for(auto barrier : barriers) {
        infoVec.insert(createBarrierInfo(ctx, barrier, id));
        id++;
    }

    for(auto info : infoVec) {
        Function *newFunc = createFunction(M, kernel, info);
        storeLiveValues(M, info);
        if(info->_id == 0 && !vecVersion) {
            prepareFunctionForLinker(M, kernel, newFunc);
        }
    }
    return true;
}

vector<Instruction *> PACXXNativeBarrier::findBarriers(Function *kernel) {
    vector<Instruction *> barriers;
    for (scc_iterator<Function *> I = scc_begin(kernel), IE = scc_end(kernel); I != IE; ++I) {
        const vector<BasicBlock *> &SCCBBs = *I;
        for (auto BBI = SCCBBs.rbegin(), BBIE = SCCBBs.rend(); BBI != BBIE; ++BBI) {
            BasicBlock *block = *BBI;
            __verbose("collectBarriers(", block->getName().str(), ")\n");
            for(auto &I : *block) {
                if(isaBarrier(&I)) {
                    __verbose("Found barrier");
                    barriers.push_back(&I);
                }
            }
        }
    }
    return barriers;
}

bool PACXXNativeBarrier::isaBarrier(const Instruction *inst) {
    if (auto CI = dyn_cast<CallInst>(inst)) {
        auto called = CI->getCalledFunction();
        if (called && called->isIntrinsic()) {
            auto intrin_id = called->getIntrinsicID();
            if (intrin_id == Intrinsic::nvvm_barrier0)
                return true;
        }
    }
    return false;
}

void PACXXNativeBarrier::splitAtBarriers(Module &M, vector<Instruction *> barriers) {

    __verbose("Splitting blocks at barriers \n");

    for(auto barrier : barriers) {

        BasicBlock *barrierBlock = barrier->getParent();

        //keep barrier instruction in original block
        auto it = ++barrier->getIterator();

        barrierBlock->splitBasicBlock(it, "split");
    }
}

PACXXNativeBarrier::BarrierInfo* PACXXNativeBarrier::createFirstInfo(LLVMContext &ctx, Function *kernel) {
    __verbose("Creating info for first barrier \n");
    SetVector<const Value *> livingValues;
    SmallVector<Type*, 8> params;
    for (auto I=kernel->arg_begin(), IE=kernel->arg_end(); I!=IE; ++I) {
        livingValues.insert(&*I);
        params.push_back((&*I)->getType());
    }

    StructType* type = StructType::get(ctx, params, false);

    SetVector<const BasicBlock *> parts;
    BasicBlock *functionEntry = &kernel->getEntryBlock();
    parts.insert(functionEntry);
    recursivePartFinding(functionEntry, parts);

    BarrierInfo *info = new BarrierInfo(0, nullptr, parts[0], parts, livingValues, type);

    __verbose("Finished creating barrier info\n");
    __verbose("Info: ", info->toString());

    return info;
}

PACXXNativeBarrier::BarrierInfo* PACXXNativeBarrier::createBarrierInfo(LLVMContext &ctx, Instruction *barrier, unsigned id) {

    auto parts = getPartsOfBarrier(barrier);
    auto livingValues = getLivingValuesForBarrier(parts);
    auto livingValuesType = getLivingValuesType(ctx, livingValues);
    BarrierInfo *info = new BarrierInfo(id, barrier, parts[0], parts, livingValues, livingValuesType);

    __verbose("Finished creating barrier info\n");
    __verbose("Info: ", info->toString());

    return info;
}

SetVector<const BasicBlock *> PACXXNativeBarrier::getPartsOfBarrier(Instruction *barrier) {
    SetVector<const BasicBlock *> parts;
    BasicBlock *barrierParent = barrier->getParent();
    recursivePartFinding(barrierParent, parts);
    return parts;
}

void PACXXNativeBarrier::recursivePartFinding(BasicBlock *block, SetVector<const BasicBlock *> &parts) {
    for (auto I = succ_begin(block), IE = succ_end(block); I != IE; ++I) {
        BasicBlock *B = *I;
        //if we already inserted this block ignore it
        if(parts.count(B) == 0) {
            parts.insert(B);
            if (!hasBarrier(B))
                recursivePartFinding(B, parts);
        }
    }
}

bool PACXXNativeBarrier::hasBarrier(const BasicBlock *block) {
    for(auto &I : *block) {
        if(isaBarrier(&I))
            return true;
    }
    return false;
}

SetVector<const Value *> PACXXNativeBarrier::getLivingValuesForBarrier(SetVector<const BasicBlock *> &parts) {
    __verbose("get living values for barrier \n");
    SetVector<const Value *> livingValues;
    for(auto block : parts) {
        // If a used value is defined outside the region, it's an input
        for (auto I = block->begin(), IE = block->end(); I != IE; ++I) {
            for (auto OI = I->op_begin(), OE = I->op_end(); OI != OE; ++OI)
                if (definedInParts(parts, *OI))
                    livingValues.insert(*OI);
        }
    }
    return livingValues;
}

bool PACXXNativeBarrier::definedInParts(SetVector<const BasicBlock *> &parts, Value * value) {
    if (Instruction *I = dyn_cast<Instruction>(value))
        if (!parts.count(I->getParent()))
            return true;
    return false;
}

StructType *PACXXNativeBarrier::getLivingValuesType(LLVMContext &ctx, SetVector<const Value *> &livingValues) {

    SmallVector<Type*, 8> params;
    for(auto value : livingValues) {
        params.push_back(value->getType());
    }

    return StructType::get(ctx, params, false);
}

Function *PACXXNativeBarrier::createFunction(Module &M, Function *kernel, BarrierInfo *info) {

    __verbose("Creating function for barrier \n");

    LLVMContext &ctx = M.getContext();

    SetVector<const BasicBlock *> &parts = info->_parts;
    SetVector<const Value *> &livingValues = info->_livingValues;

    StructType *livingValuesType = info->_livingValuesType;

    string name = kernel->getName().str() + to_string(info->_id);

    SmallVector < Type * , 8 > params;
    params.insert(params.end(), livingValuesType->element_begin(), livingValuesType->element_end());
    // pointer to data where to store liveValues
    params.push_back(Type::getInt8PtrTy(ctx));

    FunctionType *fnType = FunctionType::get(Type::getInt32Ty(ctx), params, false);

    Function *newFunc = Function::Create(fnType, Function::ExternalLinkage, kernel->getName(), &M);

    // clone corresponding basic blocks into new function
    ValueToValueMapTy &VMap = info->_VMap;

    // map instructions of old function to new ones
    ValueToValueMapTy &OrigVMap = _origFnValueMap;

    auto liveValueArg = --newFunc->arg_end();
    for (unsigned i = 0; i < livingValuesType->getNumElements(); ++i) {
        --liveValueArg;
    }

    for (auto value : livingValues) {
        VMap[value] = &*liveValueArg;
        (liveValueArg++)->setName(value->getName());
    }

    SmallVector<BasicBlock *, 8> clonedBlocks;
    for(auto block : parts) {
        BasicBlock * cloned = CloneBasicBlock(block, VMap, "", newFunc);
        VMap[block] = cloned;
        // map all instructions of cloned basic blocks
        auto CI = cloned->begin();
        for(auto I = block->begin(), IE = block->end(); I != IE; ++I, CI++) {
            OrigVMap[&*I] = &*CI;
        }
        clonedBlocks.push_back(cloned);
    }

    remapInstructionsInBlocks(clonedBlocks, VMap);

    //replace all returns with return -1
    __verbose("replace returns with -1 \n");
    for(auto I = newFunc->begin(), IE = newFunc->end(); I != IE; ++I) {
        if(ReturnInst * RI = dyn_cast<ReturnInst>(I->getTerminator())) {
            ReturnInst::Create(ctx, ConstantInt::getSigned(Type::getInt32Ty(ctx), -1), RI);
            RI->eraseFromParent();
        }
    }

    //replace branch after barrier with return
    __verbose("replace branch after barrier \n");
    for (auto I=parts.begin(), IE=parts.end(); I != IE; ++I) {
        const BasicBlock *origBB = *I;
        BasicBlock *copyBB = cast<BasicBlock>(VMap[origBB]);
        if(!hasBarrier(origBB))
            continue;
        copyBB->getTerminator()->eraseFromParent();
        ReturnInst::Create(ctx, ConstantInt::get(Type::getInt32Ty(ctx), info->_id, true), copyBB);
    }

    __verbose("Finished creating function \n");

    info->_func = newFunc;

    return newFunc;
}

void PACXXNativeBarrier::storeLiveValues(Module &M, BarrierInfo *info) {

    __verbose("create store for live values \n");

    LLVMContext &ctx = M.getContext();

    const Instruction *origBarrier = info->_barrier;

    if(!origBarrier)
        return;

    Instruction *barrier = cast<Instruction>(_origFnValueMap[origBarrier]);

    auto livingValues = info->_livingValues;

    StructType *type = info->_livingValuesType;

    Function *func = barrier->getParent()->getParent();

    Value *storeTo = &*(--func->arg_end());

    BitCastInst *cast = new BitCastInst(storeTo, PointerType::getUnqual(type), "", barrier);

    unsigned i = 0;
    for (auto origValue : livingValues) {
        Value *value = _origFnValueMap[origValue];
        SmallVector<Value*, 8> idx;
        idx.push_back(ConstantInt::getNullValue(Type::getInt32Ty(ctx)));
        idx.push_back(ConstantInt::get(ctx, APInt(32, i++)));
        GetElementPtrInst *gep = GetElementPtrInst::Create(nullptr, cast, idx, "", barrier);
        unsigned align = M.getDataLayout().getPrefTypeAlignment(value->getType());
        new StoreInst(value, gep, false, align, barrier);
    }

    //now we can remove the barrier
    barrier->eraseFromParent();

    __verbose("Finished creating store \n");
}

void PACXXNativeBarrier::prepareFunctionForLinker(Module &M, Function *oldFunc, Function *newFunc) {
    __verbose("preparing function for linker \n");

    LLVMContext &ctx = oldFunc->getContext();

    // mark first function as kernel and mark use of a barrier
    newFunc->addFnAttr("barrier");

    // replace old kernel in metadata with new one
    //this work around is needed, replaceAllUsesWith fails cause of different types
    NamedMDNode *MD = M.getNamedMetadata("nvvm.annotations");

    Metadata *MDVals[] = {ConstantAsMetadata::get(newFunc), MDString::get(ctx, "kernel"),
                          ConstantAsMetadata::get(ConstantInt::get(Type::getInt32Ty(ctx), 1))};

    MD->setOperand(0, MDNode::get(ctx, MDVals));


    while (!oldFunc->use_empty()) {
        auto &U = *oldFunc->use_begin();
        U.set(newFunc);
    }

    __verbose("finished preparing \n");
}

void PACXXNativeBarrier::createSpecialFooWrapper(Module &M, Function *foo,
                                                 SetVector<BarrierInfo *> barrierInfo,
                                                 SetVector<BarrierInfo *> vecBarrierInfo) {

    __verbose("Creating special foo wrapper \n");

    LLVMContext &ctx = M.getContext();
    const DataLayout &dl = M.getDataLayout();

    Type *int32_type = Type::getInt32Ty(ctx);

    SmallVector<pair<AllocaInst *, AllocaInst *>, 8> livingValuesMem;

    bool vectorized = vecBarrierInfo.size() == 0 ? false : true;

    if(vectorized)
        assert( barrierInfo.size() == vecBarrierInfo.size() &&
                        "number of infos for vectorized version and sequential version expected to be equal");

    // special foo wrapper
    Function *newFoo = Function::Create(foo->getFunctionType(), Function::ExternalLinkage,
                                        "__barrier__foo__" + barrierInfo[0]->_func->getName().str() , &M);

    BasicBlock *entry = BasicBlock::Create(ctx, "entry", newFoo);
    BasicBlock *allocBB = BasicBlock::Create(ctx, "allocBB", newFoo);
    BasicBlock *switchBB = BasicBlock::Create(ctx, "switchBB", newFoo);
    BasicBlock *breakBB = BasicBlock::Create(ctx, "break", newFoo);
    BasicBlock *lastBB = BasicBlock::Create(ctx, "exit", newFoo);

    // mem for foo params
    auto argIt = newFoo->arg_begin();
    AllocaInst *allocMax_x = new AllocaInst(argIt->getType(), nullptr, dl.getPrefTypeAlignment(argIt->getType()),
                                           "", entry);
    new StoreInst(&*argIt, allocMax_x, entry);
    (argIt++)->setName("__maxx");

    AllocaInst *allocMax_y = new AllocaInst(argIt->getType(), nullptr, dl.getPrefTypeAlignment(argIt->getType()),
                                           "", entry);
    new StoreInst(&*argIt, allocMax_y, entry);
    (argIt++)->setName("__maxy");

    AllocaInst *allocMax_z = new AllocaInst(argIt->getType(), nullptr, dl.getPrefTypeAlignment(argIt->getType()),
                                           "", entry);
    new StoreInst(&*argIt, allocMax_z, entry);
    (argIt++)->setName("__maxz");

    Alloca3 alloc_max = Alloca3(allocMax_x, allocMax_y, allocMax_z);

    BranchInst::Create(allocBB, entry);

    AllocaInst *allocSwitchParam = new AllocaInst(int32_type, nullptr,
                                                  dl.getPrefTypeAlignment(int32_type), "", allocBB);
    new StoreInst(ConstantInt::get(int32_type, 0), allocSwitchParam, allocBB);

    // calc max threads
    LoadInst *loadMaxx = new LoadInst(allocMax_x, "loadMaxx", allocBB);
    LoadInst *loadMaxy = new LoadInst(allocMax_y, "loadMaxy", allocBB);
    LoadInst *loadMaxz = new LoadInst(allocMax_z, "loadMaxz", allocBB);
    BinaryOperator *mulXY = BinaryOperator::CreateMul(loadMaxx, loadMaxy, "", allocBB);
    BinaryOperator *numThreads = BinaryOperator::CreateMul(mulXY, loadMaxz, "numThreads", allocBB);

    // create Memory for living values
    for(unsigned i = 0; i < barrierInfo.size(); ++i) {
        AllocaInst *mem = createMemForLivingValues(dl, barrierInfo[i], numThreads, allocBB);
        AllocaInst *mem_vec = nullptr;
        if(vectorized)
            mem_vec =  createMemForLivingValues(dl, vecBarrierInfo[i], numThreads, allocBB);
        livingValuesMem.push_back(make_pair(mem, mem_vec));
    }

    BranchInst::Create(switchBB, allocBB);

    //create switch
    LoadInst *loadSwitchParam = new LoadInst(allocSwitchParam, "loadSwitchParam", switchBB);
    SwitchInst *switchInst = SwitchInst::Create(loadSwitchParam, lastBB, barrierInfo.size(), switchBB);


    //create Cases
    for(unsigned i = 0; i < barrierInfo.size(); ++i) {

        // handle last case where we have no next mem
        pair<AllocaInst*, AllocaInst*> nextMem;
        if(i != barrierInfo.size() -1)
            nextMem = livingValuesMem[i+1];
        else
            nextMem = make_pair(nullptr, nullptr);

        pair<Function *, Function *> calledFunctions = make_pair(barrierInfo[i]->_func,
                                                                 vectorized ? vecBarrierInfo[i]->_func : nullptr);
        pair<StructType*, StructType*> loadTypes = make_pair(barrierInfo[i]->_livingValuesType,
                                                  vectorized ? vecBarrierInfo[i]->_livingValuesType : nullptr);

        const CaseInfo info = CaseInfo(i, newFoo, switchBB, breakBB, allocSwitchParam, alloc_max,
                                 livingValuesMem[i], nextMem, loadTypes, calledFunctions);
        BasicBlock *caseBlock = createCase(M, info, vectorized);

        //add case to switch
        switchInst->addCase(cast<ConstantInt>(ConstantInt::get(int32_type, i)), caseBlock);
    }

    //break from switch
    BranchInst::Create(switchBB, breakBB);

    // return from foo
    ReturnInst::Create(ctx, nullptr, lastBB);

    //now inline all calls and remove the no longer required functions
    for(auto call : _inlineCalls) {
        InlineFunctionInfo IFI;
        InlineFunction(call, IFI);
        call->getCalledFunction()->eraseFromParent();
    }
}

AllocaInst *PACXXNativeBarrier::createMemForLivingValues(const DataLayout &dl, BarrierInfo *info, Value *numThreads,
                                                         BasicBlock *BB) {

    if(info->_id == 0)
        return nullptr;

    Type *type = info->_livingValuesType;

    return new AllocaInst(type, numThreads, dl.getPrefTypeAlignment(type), "", BB);
}

BasicBlock *PACXXNativeBarrier::createCase(Module &M,
                                           const CaseInfo &info,
                                           bool vectorized) {

    LLVMContext &ctx = M.getContext();

    const DataLayout dl = M.getDataLayout();

    Type *int32_type = Type::getInt32Ty(ctx);

    Function *newFoo = info._foo;

    BasicBlock *caseEntry = BasicBlock::Create(ctx, "entryCase", newFoo);
    //caseEntry->moveAfter(info._switchBB);

    // alloc z
    AllocaInst *alloc_z = new AllocaInst(int32_type, nullptr,
                                                  dl.getPrefTypeAlignment(int32_type), "__z", caseEntry);
    new StoreInst(ConstantInt::get(int32_type, 0), alloc_z, caseEntry);

    // blocks of z loop
    BasicBlock *loopHeader_z = BasicBlock::Create(ctx, "LoopHeader_Z", newFoo);
    loopHeader_z->moveAfter(caseEntry);

    BranchInst::Create(loopHeader_z, caseEntry);

    BasicBlock *end_z = BasicBlock::Create(ctx, "End_Z", newFoo);

    // blocks of y loop
    BasicBlock *loopHeader_y = BasicBlock::Create(ctx, "LoopHeader_Y", newFoo);
    loopHeader_y->moveAfter(loopHeader_z);

    BasicBlock *end_y = BasicBlock::Create(ctx, "End_Y", newFoo);

    // alloc y
    AllocaInst *alloc_y = new AllocaInst(int32_type, nullptr,
                                                  dl.getPrefTypeAlignment(int32_type), "__y", loopHeader_z);
    new StoreInst(ConstantInt::get(int32_type, 0), alloc_y, loopHeader_z);

    // alloc x
    AllocaInst *alloc_x = new AllocaInst(int32_type, nullptr,
                                                  dl.getPrefTypeAlignment(int32_type), "__x", loopHeader_y);
    new StoreInst(ConstantInt::get(int32_type, 0), alloc_x, loopHeader_y);

    Alloca3 id = Alloca3(alloc_x, alloc_y, alloc_z);

    // create the x loop
    auto p = createXLoop(M, info, id, loopHeader_y, end_y, vectorized);

    // fill loop header blocks
    fillLoopHeader(loopHeader_z, alloc_z, info._max3._z, loopHeader_y, info._breakBB);

    fillLoopHeader(loopHeader_y, alloc_y, info._max3._y, p.first, end_z);

    //move end of y loop
    end_y->moveAfter(p.second);

    // move end of z loop
    end_z->moveAfter(end_y);

    // fill loop end blocks
    fillLoopEnd(end_z, loopHeader_z, alloc_z, int32_type, 1);

    fillLoopEnd(end_y, loopHeader_y, alloc_y, int32_type, 1);

    return caseEntry;
}

pair<BasicBlock *, BasicBlock*> PACXXNativeBarrier::createXLoop(Module &M,
                                                               const CaseInfo &info,
                                                               Alloca3 id,
                                                               BasicBlock *afterBB,
                                                               BasicBlock *falseBB,
                                                               bool vectorized) {

    __verbose("Creating the x-loop \n");

    LLVMContext &ctx = M.getContext();

    Type *int32_type = Type::getInt32Ty(ctx);

    Function *newFoo = info._foo;

    //loop header of x
    BasicBlock *loopHeader_x = BasicBlock::Create(ctx, "LoopHeader_X", newFoo);

    BasicBlock *firstBB = loopHeader_x;

    if(vectorized) {

        __verbose("Creating vectorized part of the case \n");

        //loop header of vectorized x
        BasicBlock *loopHeader_vecx = BasicBlock::Create(ctx, "LoopHeader_VecX", newFoo);
        loopHeader_vecx->moveAfter(afterBB);

        //body of vec x
        BasicBlock *body_vecx = BasicBlock::Create(ctx, "Body_VecX", newFoo);
        body_vecx->moveAfter(loopHeader_vecx);

        //end of vec x
        BasicBlock *end_vecx = BasicBlock::Create(ctx, "End_VecX", newFoo);
        end_vecx->moveAfter(body_vecx);

        // loop header of vec x
        LoadInst *load = new LoadInst(id._x, "load", loopHeader_vecx);

        LoadInst *loadMax = new LoadInst(info._max3._x, "loadMax", loopHeader_vecx);

        BinaryOperator *add = BinaryOperator::CreateAdd(load, ConstantInt::get(int32_type, _vectorWidth),
                                                        "addSIMD", loopHeader_vecx);

        ICmpInst *cmp = new ICmpInst(*loopHeader_vecx, ICmpInst::ICMP_SLT, add, loadMax, "cmp");
        BranchInst::Create(body_vecx, loopHeader_x, cmp, loopHeader_vecx);

        fillLoopXBody(M,
                      info,
                      body_vecx,
                      end_vecx,
                      id,
                      true);

        fillLoopEnd(end_vecx, loopHeader_vecx, id._x, int32_type, _vectorWidth);

        afterBB = end_vecx;
        firstBB = loopHeader_vecx;
    }

    __verbose("Creating sequential part of the case \n");

    loopHeader_x->moveAfter(afterBB);

    //body of x
    BasicBlock *body_x = BasicBlock::Create(ctx, "Body_X", newFoo);
    body_x->moveAfter(loopHeader_x);

    //end of x
    BasicBlock *end_x = BasicBlock::Create(ctx, "End_X", newFoo);
    end_x->moveAfter(body_x);

    fillLoopHeader(loopHeader_x, id._x, info._max3._x, body_x, falseBB);

    //fill body of x
    fillLoopXBody(M, info, body_x, end_x, id, false);

    fillLoopEnd(end_x, loopHeader_x, id._x, int32_type, 1);

    return pair<BasicBlock *, BasicBlock *>(firstBB, end_x);

    __verbose("Done creating the x-loop \n");
}

void PACXXNativeBarrier::fillLoopXBody(Module &M,
                                       const CaseInfo &info,
                                       BasicBlock *loopBody,
                                       BasicBlock *nextBB,
                                       Alloca3 &id,
                                       bool vectorized) {

    __verbose("Filling body of the x-loop \n");

    LLVMContext &ctx = M.getContext();

    SmallVector<Value *, 8> args;

    LoadInst *load_x = new LoadInst(id._x, "__x", loopBody);
    CastInst *zext_x = ZExtInst::CreateZExtOrBitCast(load_x, Type::getInt64Ty(ctx), "", loopBody);
    LoadInst *load_y = new LoadInst(id._y, "__y", loopBody);
    CastInst *zext_y = ZExtInst::CreateZExtOrBitCast(load_y, Type::getInt64Ty(ctx), "", loopBody);
    LoadInst *load_z = new LoadInst(id._z, "__z", loopBody);
    CastInst *zext_z = ZExtInst::CreateZExtOrBitCast(load_z, Type::getInt64Ty(ctx), "", loopBody);

    LoadInst *load_max_x = new LoadInst(info._max3._x, "maxx", loopBody);
    LoadInst *load_max_y = new LoadInst(info._max3._y, "maxy", loopBody);

    if(info._id == 0) {
        args.push_back(zext_x);
        args.push_back(zext_y);
        args.push_back(zext_z);

        Function *calledFunc = vectorized ? info._calledFunctions.second : info._calledFunctions.first;

        //replace return with a store, because we inline dummy later and so the return is void
        for(auto I = calledFunc->begin(), IE = calledFunc->end(); I != IE; ++I) {
            if (ReturnInst *RI = dyn_cast<ReturnInst>(I->getTerminator())) {
                new StoreInst(RI->getReturnValue(), info._switchParam, loopBody);
            }
        }

        Function *dummy = vectorized ? M.getFunction("__vectorized__dummy_kernel") : M.getFunction("__dummy_kernel");

        CallInst::Create(dummy, args, "", loopBody);
        BranchInst::Create(nextBB, loopBody);
        __verbose("created x-loop body for first case \n");
    }
    else {

        // calc local id
        // blockIdx.x
        // + blockIdx.y * gridDim.x
        // + gridDim.x * gridDim.y * blockIdx.z;
        BinaryOperator *mul_y_maxx = BinaryOperator::CreateMul(load_y, load_max_x, "", loopBody);
        BinaryOperator *mul_maxx_maxy = BinaryOperator::CreateMul(load_max_x, load_max_y, "", loopBody);
        BinaryOperator *mul_mulmaxxmaxy_z = BinaryOperator::CreateMul(mul_maxx_maxy, load_z, "", loopBody);
        BinaryOperator *add = BinaryOperator::CreateAdd(mul_y_maxx, mul_mulmaxxmaxy_z, "", loopBody);
        BinaryOperator *blockId = BinaryOperator::CreateAdd(load_x, add, "", loopBody);

        StructType *type = vectorized ? info._loadType.second : info._loadType.first;
        AllocaInst *mem = vectorized ? info._loadStruct.second : info._loadStruct.first;

        __verbose("Setting living values args \n");
        for(unsigned i = 0; i < type->getStructNumElements(); ++i) {
            SmallVector<Value*, 8> idx;
            idx.push_back(blockId);
            idx.push_back(ConstantInt::get(ctx, APInt(32, i)));
            GetElementPtrInst *struct_gep = GetElementPtrInst::Create(type, mem, idx, "", loopBody);
            LoadInst *load = new LoadInst(struct_gep, "", loopBody);
            args.push_back(load);
        }

        __verbose("Setting ptr to next struct \n");

        // set ptr to next struct
        AllocaInst *nextMem = vectorized ? info._nextStruct.second : info._nextStruct.first;
        // handle last case where we have no next memory
        if(nextMem) {
            GetElementPtrInst *nextGEP = GetElementPtrInst::Create(nextMem->getType(), nextMem, blockId, "", loopBody);
            LoadInst *loadNextMem = new LoadInst(nextGEP, "nextLivingValues", loopBody);
            args.push_back(loadNextMem);
        }
        else {
            args.push_back(UndefValue::get(Type::getInt8PtrTy(ctx)));
        }

        Function *func = vectorized ? info._calledFunctions.second : info._calledFunctions.first;
        CallInst *call = CallInst::Create(func, args, "", loopBody);
        new StoreInst(call, info._switchParam, loopBody);

        BranchInst::Create(nextBB, loopBody);

        //save call to inline it later
        _inlineCalls.push_back(call);
    }

    __verbose("Done filling x-loop body \n");
}

void PACXXNativeBarrier::fillLoopHeader(BasicBlock *loopHeader, AllocaInst *loopVar, AllocaInst *loopMax,
                                         BasicBlock *trueBB, BasicBlock *falseBB) {

    LoadInst *load = new LoadInst(loopVar, "load", loopHeader);
    LoadInst *loadMax = new LoadInst(loopMax, "loadMax", loopHeader);

    ICmpInst *cmp = new ICmpInst(*loopHeader, ICmpInst::ICMP_SLT, load, loadMax, "cmp");
    BranchInst::Create(trueBB, falseBB, cmp, loopHeader);
}

void PACXXNativeBarrier::fillLoopEnd(BasicBlock *loopEnd, BasicBlock *branchBB,
                                      AllocaInst *loopValue, Type *int32_type, unsigned incVal) {

    LoadInst *load = new LoadInst(loopValue, "load", loopEnd);
    BinaryOperator *inc = BinaryOperator::CreateAdd(load, ConstantInt::get(int32_type, incVal), "inc", loopEnd);
    new StoreInst(inc, loopValue, loopEnd);
    BranchInst::Create(branchBB, loopEnd);
}

char PACXXNativeBarrier::ID = 0;

INITIALIZE_PASS_BEGIN(PACXXNativeBarrier, "barrier",
                "Native barrier", true, true)
INITIALIZE_PASS_END(PACXXNativeBarrier, "barrier",
                "Native barrier", true, true)

namespace llvm {
    llvm::Pass *createPACXXNativeBarrierPass() {
        return new PACXXNativeBarrier();
    }
}
