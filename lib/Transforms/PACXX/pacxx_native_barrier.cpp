// Created by lars
// Based on the concept described in Improving Performance of OpenCL on CPUs by Ralf Karrenberg and Sebastian Hack (http://llvm.org/devmtg/2012-04-12/Slides/Ralf_Karrenberg.pdf)
// and the implementation available at: 
// https://github.com/karrenberg/wfvopencl/blob/master/src/passes/continuationGenerator.cpp

#include "Log.h"
#include "pacxx_liveness_analysis.h"

using namespace llvm;
using namespace std;
using namespace pacxx;


class PACXXNativeBarrier : public ModulePass {

public:
    static char ID;

    PACXXNativeBarrier() : llvm::ModulePass(ID) { initializePACXXNativeBarrierPass(*PassRegistry::getPassRegistry()); }

    virtual ~PACXXNativeBarrier() {};

    void releaseMemory() override;

    void getAnalysisUsage(AnalysisUsage &AU) const override;

    bool runOnModule(llvm::Module &M) override;

private:

    struct BarrierInfo {
        BarrierInfo(const unsigned id,
                    const Instruction *barrier,
                    const BasicBlock *entry,
                    SetVector<const BasicBlock *> parts,
                    SetVector<const Value *> livingValues,
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
        ValueToValueMapTy _OrigFnMap;

        string toString() {
            string text;
            raw_string_ostream ss(text);

            ss << "barrier with id: " << _id << "\n";

            for (auto part : _parts) {
                ss << part->getName().str();
                ss << "\n";
            }

            ss << "\n";
            ss << "living values: \n";
            for (auto value : _livingValues) {
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
                 uint64_t maxStructSize,
                 Function *pacxx_block,
                 Function *kernel,
                 BasicBlock *switchBB,
                 BasicBlock *breakBB,
                 AllocaInst *switchParam,
                 Alloca3 &max,
                 pair<AllocaInst *, AllocaInst *> loadStruct,
                 pair<AllocaInst *, AllocaInst *> nextStruct,
                 pair<StructType *, StructType *> loadType,
                 pair<Function *, Function *> calledFunctions)
                : _id(id),
                  _maxStructSize(maxStructSize),
                  _pacxx_block(pacxx_block),
                  _origKernel(kernel),
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
        const uint64_t _maxStructSize;
        Function *_pacxx_block;
        Function *_origKernel;
        BasicBlock *_switchBB;
        BasicBlock *_breakBB;
        AllocaInst *_switchParam;
        const Alloca3 _max3;
        const pair<AllocaInst *, AllocaInst *> _loadStruct;
        const pair<AllocaInst *, AllocaInst *> _nextStruct;
        const pair<StructType *, StructType *> _loadType;
        const pair<Function *, Function *> _calledFunctions;
    };

    unsigned getVectorWidth(Function *kernel);

    bool runOnFunction(Module &M, Function *kernel, SetVector<BarrierInfo *> &infoVec, bool vecVersion = false);

    vector<Instruction *> findBarriers(Function *kernel);

    bool isaBarrier(const Instruction *inst);

    void splitAtBarriers(Module &M, vector<Instruction *> barriers);

    BarrierInfo *createFirstInfo(LLVMContext &ctx, Function *kernel);

    BarrierInfo *createBarrierInfo(LLVMContext &ctx, Instruction *barrier, unsigned id);

    SetVector<const BasicBlock *> getPartsOfBarrier(Instruction *barrier);

    void recursivePartFinding(BasicBlock *block, SetVector<const BasicBlock *> &parts);

    bool hasBarrier(const BasicBlock *block);

    SetVector<const Value *> getLivingValuesForBarrier(SetVector<const BasicBlock *> &parts);

    StructType *getLivingValuesType(LLVMContext &ctx, SetVector<const Value *> &livingValues);

    Function *createFunction(Module &M, Function *kernel, BarrierInfo *info);

    void storeLiveValues(Module &M, BarrierInfo *info, ValueToValueMapTy &origFnMap);

    void createSpecialFooWrapper(Module &M, Function *pacxx_block, Function *kernel,
                                 SetVector<BarrierInfo *> barrierInfo,
                                 SetVector<BarrierInfo *> vecBarrierInfo);

    uint64_t getMaxStructSize(const DataLayout &dl, SetVector<BarrierInfo *> infos);

    AllocaInst *createMemForLivingValues(const DataLayout &dl, uint64_t maxStructSize,
                                         Value *numThreads, BasicBlock *BB);

    BasicBlock *createCase(Module &M, const CaseInfo &info, bool vectorized);

    pair<BasicBlock *, BasicBlock *> createXLoop(Module &M,
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
    PACXXNativeLivenessAnalyzer *_livenessAnalyzer;
    DenseMap<const Instruction *, unsigned> _indexMap;
    vector<CallInst *> _inlineCalls;
    unsigned _vectorWidth;
};

void PACXXNativeBarrier::releaseMemory() {}

void PACXXNativeBarrier::getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequired<PACXXNativeLivenessAnalyzer>();
}

bool PACXXNativeBarrier::runOnModule(llvm::Module &M) {

    auto kernels = getTagedFunctions(&M, "nvvm.annotations", "kernel");

    auto pacxx_block = M.getFunction("__pacxx_block");

    bool modified = false;

    for(auto kernel : kernels) {

        string kernelName = kernel->getName().str();

        SetVector<BarrierInfo *> barrierInfo;
        SetVector<BarrierInfo *> vecBarrierInfo;

        bool modified_kernel = runOnFunction(M, kernel, barrierInfo);

        if(modified_kernel) {

            //if we have a vectorized version of the kernel also eliminate barriers
            if (kernel->hasFnAttribute("vectorized")) {

                auto vec_kernel = M.getFunction("__vectorized__" + kernelName);

                __verbose("Running for vectorized kernel");
                _vectorWidth = getVectorWidth(vec_kernel);
                runOnFunction(M, vec_kernel, vecBarrierInfo, true);
            }

            createSpecialFooWrapper(M, pacxx_block, kernel, barrierInfo, vecBarrierInfo);
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
    _indexMap.clear();
    auto barriers = findBarriers(kernel);

    unsigned numBarriers = barriers.size();

    if(numBarriers == 0) {
        __verbose("no barriers found \n");
        return false;
    }

    __verbose("Found ", numBarriers, " barriers. Modifying \n");
    // mark kernel as a kernel with barriers
    kernel->addFnAttr("barrier");


    splitAtBarriers(M, barriers);

    __verbose("Getting living value analysis \n");
    _livenessAnalyzer = &getAnalysis<PACXXNativeLivenessAnalyzer>(*kernel);

    infoVec.insert(createFirstInfo(ctx, kernel));

    unsigned id = 1;
    for(auto barrier : barriers) {
        infoVec.insert(createBarrierInfo(ctx, barrier, id));
        id++;
    }

    for(auto info : infoVec) {
        createFunction(M, kernel, info);
    }

    for(auto info : infoVec) {
        auto origBarrier = info->_barrier;
        for(auto lookForMap : infoVec) {
            if (lookForMap->_OrigFnMap.count(origBarrier))
                storeLiveValues(M, info, lookForMap->_OrigFnMap);
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
            if (intrin_id == Intrinsic::pacxx_barrier0)
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

    _indexMap[barrier] = id;

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
    auto livingValues = _livenessAnalyzer->getLivingInValuesForBlock(parts[0]);
    return SetVector<const Value *>(livingValues.begin(), livingValues.end());
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

    string name = kernel->getName().str();

    SetVector<const BasicBlock *> &parts = info->_parts;
    SetVector<const Value *> &livingValues = info->_livingValues;

    StructType *livingValuesType = info->_livingValuesType;

    SmallVector < Type * , 8 > params;

    for(auto &arg : kernel->args()) {
        params.push_back(arg.getType());
    }

    params.insert(params.end(), livingValuesType->element_begin(), livingValuesType->element_end());

    // pointer to data where to store liveValues
    params.push_back(Type::getInt8PtrTy(ctx));

    FunctionType *fnType = FunctionType::get(Type::getInt32Ty(ctx), params, false);

    Function *newFunc = Function::Create(fnType, Function::ExternalLinkage, name, &M);

    // clone corresponding basic blocks into new function

    // map instructions of old function to new ones
    ValueToValueMapTy &OrigVMap = info->_OrigFnMap;

    auto newArg = newFunc->arg_begin();
    for(auto &arg : kernel->args()) {
        OrigVMap[&arg] = &*newArg;
        newArg++;
    }


    auto liveValueArg = newFunc->arg_end();
    --liveValueArg;
    liveValueArg->setName("native.struct");
    for (unsigned i = 0; i < livingValuesType->getNumElements(); ++i) {
        --liveValueArg;
    }

    for (auto value : livingValues) {
        OrigVMap[value] = &*liveValueArg;
        (liveValueArg++)->setName(value->getName());
    }

    SmallVector<BasicBlock *, 8> clonedBlocks;
    for(auto block : parts) {
        BasicBlock * cloned = CloneBasicBlock(block, OrigVMap, "", newFunc);
        OrigVMap[block] = cloned;
        // map all instructions of cloned basic blocks
        auto CI = cloned->begin();
        for(auto I = block->begin(), IE = block->end(); I != IE; ++I, CI++) {
            OrigVMap[&*I] = &*CI;
        }
        clonedBlocks.push_back(cloned);
    }

    remapInstructionsInBlocks(clonedBlocks, OrigVMap);

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
        BasicBlock *copyBB = cast<BasicBlock>(OrigVMap[origBB]);
        if(!hasBarrier(origBB))
            continue;
        const Instruction* barrier = &*(--(--origBB->end()));

        copyBB->getTerminator()->eraseFromParent();
        const unsigned barrierIndex = _indexMap[barrier];
        ReturnInst::Create(ctx, ConstantInt::get(Type::getInt32Ty(ctx), barrierIndex, true), copyBB);

    }

    //manually map living values that are not correctly mapped
    __verbose("start manual remapping \n");
    auto argIt = newFunc->arg_end();
    --argIt;
    liveValueArg->setName("native.struct");
    for (unsigned i = 0; i < livingValuesType->getNumElements(); ++i) {
        --argIt;
    }

    DominatorTree domTree = DominatorTree(*newFunc);
    for(auto livingValue : livingValues) {
        if (isa<Argument>(livingValue)) {
            __verbose("is an argument \n");
            argIt++;
            continue;
        }

        // if all uses were already replaced above, skip this value
        // (conditions map to the two cases where uses of instructions are replaced above)
        if (OrigVMap.find(livingValue) == OrigVMap.end() || OrigVMap[livingValue] == &*argIt) {
            __verbose("is already mapped \n");
            argIt++;
            continue;
        }


        Value *newVal = OrigVMap[livingValue];

        // if the value is defined in one of the copied blocks, we must only
        // replace those uses that are not dominated by their definition anymore
        SetVector<Instruction *> replaceNeeded;
        if (const Instruction *inst = dyn_cast<Instruction>(livingValue)) {
            if (parts.count(inst->getParent())) {
                Instruction *newInst = cast<Instruction>(newVal);
                for (auto user : newInst->users()) {
                    assert(isa<Instruction>(user) && "not a instruction");
                    Instruction *userInst = dyn_cast<Instruction>(user);
                    if (!(domTree.dominates(newInst, userInst))) {
                        __verbose("not dominated. replacing \n");
                        replaceNeeded.insert(userInst);
                    }
                }
                for (auto inst : replaceNeeded) {
                    inst->replaceUsesOfWith(newInst, &*argIt);
                }
                argIt++;
                continue;
            }
        }

        newVal->replaceAllUsesWith(&*argIt);
        argIt++;
    }

    __verbose("Finished manual remapping \n");

    __verbose("Finished creating function \n");

    info->_func = newFunc;

    return newFunc;
}

void PACXXNativeBarrier::storeLiveValues(Module &M, BarrierInfo *info, ValueToValueMapTy &origFnMap) {

    __verbose("create store for live values \n");

    LLVMContext &ctx = M.getContext();

    const Instruction *origBarrier = info->_barrier;

    if(!origBarrier)
        return;

    Instruction *barrier = cast<Instruction>(origFnMap[origBarrier]);

    auto livingValues = info->_livingValues;

    StructType *type = info->_livingValuesType;

    Function *func = barrier->getParent()->getParent();

    auto storeToIt = func->arg_end();
    --storeToIt;
    Value *storeTo = &*storeToIt;

    BitCastInst *cast = new BitCastInst(storeTo, PointerType::getUnqual(type), "", barrier);

    unsigned i = 0;
    for (auto origValue : livingValues) {
        __verbose("creating store for \n");
        Value *value = origFnMap[origValue];
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

void PACXXNativeBarrier::createSpecialFooWrapper(Module &M, Function *pacxx_block, Function *kernel,
                                                 SetVector<BarrierInfo *> barrierInfo,
                                                 SetVector<BarrierInfo *> vecBarrierInfo) {

    __verbose("Creating special pacxx_block wrapper \n");

    LLVMContext &ctx = M.getContext();
    const DataLayout &dl = M.getDataLayout();

    Type *int32_type = Type::getInt32Ty(ctx);

    bool vectorized = vecBarrierInfo.size() == 0 ? false : true;

    if(vectorized)
        assert( barrierInfo.size() == vecBarrierInfo.size() &&
                        "number of infos for vectorized version and sequential version expected to be equal");

    // special pacxx_block wrapper
    SmallVector<Type *, 8> Params;
    for(auto &arg : pacxx_block->args()) {
        Params.push_back(arg.getType());
    }
    for(auto &arg : kernel->args()) {
        Params.push_back(arg.getType());
    }
    FunctionType *FTy = FunctionType::get(Type::getVoidTy(ctx), Params, false);
    Function *newFoo = Function::Create(FTy, Function::ExternalLinkage,
                                        "__barrier__pacxx_block__" + kernel->getName().str(), &M);

    BasicBlock *entry = BasicBlock::Create(ctx, "entry", newFoo);
    BasicBlock *allocBB = BasicBlock::Create(ctx, "allocBB", newFoo);
    BasicBlock *switchBB = BasicBlock::Create(ctx, "switchBB", newFoo);
    BasicBlock *breakBB = BasicBlock::Create(ctx, "break", newFoo);
    BasicBlock *lastBB = BasicBlock::Create(ctx, "exit", newFoo);

    // mem for pacxx_block params
    auto argIt = newFoo->arg_begin();
    AllocaInst *allocMax_x = new AllocaInst(argIt->getType(), 0, nullptr, dl.getPrefTypeAlignment(argIt->getType()),
                                           "", entry);
    new StoreInst(&*argIt, allocMax_x, entry);
    (argIt++)->setName("__maxx");

    AllocaInst *allocMax_y = new AllocaInst(argIt->getType(), 0, nullptr, dl.getPrefTypeAlignment(argIt->getType()),
                                           "", entry);
    new StoreInst(&*argIt, allocMax_y, entry);
    (argIt++)->setName("__maxy");

    AllocaInst *allocMax_z = new AllocaInst(argIt->getType(), 0, nullptr, dl.getPrefTypeAlignment(argIt->getType()),
                                           "", entry);
    new StoreInst(&*argIt, allocMax_z, entry);
    (argIt++)->setName("__maxz");

    Alloca3 alloc_max = Alloca3(allocMax_x, allocMax_y, allocMax_z);

    BranchInst::Create(allocBB, entry);

    AllocaInst *allocSwitchParam = new AllocaInst(int32_type, 0, nullptr,
                                                  dl.getPrefTypeAlignment(int32_type), "", allocBB);
    new StoreInst(ConstantInt::get(int32_type, 0), allocSwitchParam, allocBB);

    // calc max threads
    LoadInst *loadMaxx = new LoadInst(allocMax_x, "loadMaxx", allocBB);
    LoadInst *loadMaxy = new LoadInst(allocMax_y, "loadMaxy", allocBB);
    LoadInst *loadMaxz = new LoadInst(allocMax_z, "loadMaxz", allocBB);
    BinaryOperator *mulXY = BinaryOperator::CreateMul(loadMaxx, loadMaxy, "", allocBB);
    BinaryOperator *numThreads = BinaryOperator::CreateMul(mulXY, loadMaxz, "numThreads", allocBB);

    // create memory for the living values
    uint64_t maxStructSize = vectorized ? getMaxStructSize(dl, vecBarrierInfo) : getMaxStructSize(dl, barrierInfo);
    AllocaInst *mem = createMemForLivingValues(dl, maxStructSize, numThreads, allocBB);
    AllocaInst *mem_vec = vectorized ? createMemForLivingValues(dl, maxStructSize, numThreads, allocBB) : nullptr;

    auto livingValuesMem = make_pair(mem, mem_vec);

    BranchInst::Create(switchBB, allocBB);

    //create switch
    LoadInst *loadSwitchParam = new LoadInst(allocSwitchParam, "loadSwitchParam", switchBB);
    SwitchInst *switchInst = SwitchInst::Create(loadSwitchParam, lastBB, barrierInfo.size(), switchBB);


    //create Cases
    for(unsigned i = 0; i < barrierInfo.size(); ++i) {

        pair<Function *, Function *> calledFunctions = make_pair(barrierInfo[i]->_func,
                                                                 vectorized ? vecBarrierInfo[i]->_func : nullptr);
        pair<StructType*, StructType*> loadTypes = make_pair(barrierInfo[i]->_livingValuesType,
                                                  vectorized ? vecBarrierInfo[i]->_livingValuesType : nullptr);

        const CaseInfo info = CaseInfo(i, maxStructSize, newFoo, kernel, switchBB, breakBB, allocSwitchParam, alloc_max,
                                 livingValuesMem, livingValuesMem, loadTypes, calledFunctions);
        BasicBlock *caseBlock = createCase(M, info, vectorized);

        //add case to switch
        switchInst->addCase(cast<ConstantInt>(ConstantInt::get(int32_type, i)), caseBlock);
    }

    //break from switch
    BranchInst::Create(switchBB, breakBB);

    // return from pacxx_block
    ReturnInst::Create(ctx, nullptr, lastBB);

    //now inline all calls and remove the no longer required functions
    for(auto call : _inlineCalls) {
	Function *calledFunction = call->getCalledFunction();
        InlineFunctionInfo IFI;
        InlineFunction(call, IFI);
        calledFunction->eraseFromParent();
    }
    _inlineCalls.clear(); 
}

uint64_t PACXXNativeBarrier::getMaxStructSize(const DataLayout &dl, SetVector<BarrierInfo *> infos) {
    uint64_t  maxStructSize = 0;
    for(auto info : infos) {
        if(info->_id == 0)
            continue;
        uint64_t size = dl.getTypeAllocSize(info->_livingValuesType);
        maxStructSize = size > maxStructSize ? size : maxStructSize;
    }
    return maxStructSize;
}

AllocaInst *PACXXNativeBarrier::createMemForLivingValues(const DataLayout &dl, uint64_t maxStructSize,
                                                         Value *numThreads, BasicBlock *BB) {

    LLVMContext &ctx = BB->getContext();

    Type *i8_type = Type::getInt8Ty(ctx);

    Value *maxSize = ConstantInt::get(ctx, APInt(32, maxStructSize));
    Value *allocaSize = BinaryOperator::CreateMul(maxSize, numThreads, "allocSize", BB);

    return new AllocaInst(i8_type, 0, allocaSize, dl.getPrefTypeAlignment(i8_type), "livingMem", BB);
}

BasicBlock *PACXXNativeBarrier::createCase(Module &M,
                                           const CaseInfo &info,
                                           bool vectorized) {

    LLVMContext &ctx = M.getContext();

    const DataLayout dl = M.getDataLayout();

    Type *int32_type = Type::getInt32Ty(ctx);

    Function *newFoo = info._pacxx_block;

    BasicBlock *caseEntry = BasicBlock::Create(ctx, "entryCase", newFoo);

    auto nullNode = MDNode::get(ctx, nullptr);

    // alloc z
    AllocaInst *alloc_z = new AllocaInst(int32_type, 0, nullptr,
                                                  dl.getPrefTypeAlignment(int32_type), "__z", caseEntry);
    alloc_z->setMetadata("pacxx_read_tid_z", nullNode);
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
    AllocaInst *alloc_y = new AllocaInst(int32_type, 0, nullptr,
                                                  dl.getPrefTypeAlignment(int32_type), "__y", loopHeader_z);
    alloc_y->setMetadata("pacxx_read_tid_y", nullNode);
    new StoreInst(ConstantInt::get(int32_type, 0), alloc_y, loopHeader_z);

    // alloc x
    AllocaInst *alloc_x = new AllocaInst(int32_type, 0, nullptr,
                                                  dl.getPrefTypeAlignment(int32_type), "__x", loopHeader_y);
    alloc_x->setMetadata("pacxx_read_tid_x", nullNode);
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

    Function *newFoo = info._pacxx_block;

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
    LoadInst *load_y = new LoadInst(id._y, "__y", loopBody);
    LoadInst *load_z = new LoadInst(id._z, "__z", loopBody);

    LoadInst *load_max_x = new LoadInst(info._max3._x, "maxx", loopBody);
    LoadInst *load_max_y = new LoadInst(info._max3._y, "maxy", loopBody);

    // calc local id
    // blockIdx.x
    // + blockIdx.y * gridDim.x
    // + gridDim.x * gridDim.y * blockIdx.z;
    BinaryOperator *mul_y_maxx = BinaryOperator::CreateMul(load_y, load_max_x, "", loopBody);
    BinaryOperator *mul_maxx_maxy = BinaryOperator::CreateMul(load_max_x, load_max_y, "", loopBody);
    BinaryOperator *mul_mulmaxxmaxy_z = BinaryOperator::CreateMul(mul_maxx_maxy, load_z, "", loopBody);
    BinaryOperator *add = BinaryOperator::CreateAdd(mul_y_maxx, mul_mulmaxxmaxy_z, "", loopBody);
    BinaryOperator *blockId = BinaryOperator::CreateAdd(load_x, add, "", loopBody);

    ConstantInt *jump_size = ConstantInt::get(ctx, APInt(32, info._maxStructSize));

    BinaryOperator *offset = BinaryOperator::CreateMul(blockId, jump_size, "", loopBody);

    Function *func = vectorized ? info._calledFunctions.second : info._calledFunctions.first;

    auto argIt = info._pacxx_block->arg_end();
    unsigned numArgs = info._origKernel->arg_size();
    for(unsigned i = 0; i < numArgs; ++i)
        --argIt;

    for(unsigned i = 0; i < numArgs; ++i) {
        args.push_back(&*argIt);
        argIt++;
    }

    StructType *type = vectorized ? info._loadType.second : info._loadType.first;
    AllocaInst *mem = vectorized ? info._loadStruct.second : info._loadStruct.first;

    GetElementPtrInst *mem_gep = GetElementPtrInst::Create(Type::getInt8Ty(ctx), mem, offset, "", loopBody);
    CastInst *cast = CastInst::Create(CastInst::BitCast, mem_gep, PointerType::getUnqual(type), "", loopBody);

    __verbose("Setting living values args \n");
    for (unsigned i = 0; i < type->getStructNumElements(); ++i) {
        SmallVector<Value *, 8> idx;
        idx.push_back(ConstantInt::getNullValue(Type::getInt32Ty(ctx)));
        idx.push_back(ConstantInt::get(ctx, APInt(32, i)));
        GetElementPtrInst *struct_gep = GetElementPtrInst::Create(nullptr,
                                                                  cast, idx, "", loopBody);
        LoadInst *load = new LoadInst(struct_gep, "",false, M.getDataLayout().getPrefTypeAlignment(struct_gep->getResultElementType()), loopBody);
        args.push_back(load);
    }

    __verbose("Setting ptr to next struct \n");

    // set ptr to next struct
    AllocaInst *nextMem = vectorized ? info._nextStruct.second : info._nextStruct.first;
    // handle last case where we have no next memory
    if (nextMem) {
        args.push_back(mem_gep);
    } else {
        args.push_back(UndefValue::get(Type::getInt8PtrTy(ctx)));
    }

    CallInst *call = CallInst::Create(func, args, "", loopBody);
    new StoreInst(call, info._switchParam, loopBody);

    BranchInst::Create(nextBB, loopBody);

    //save call to inline it later
    _inlineCalls.push_back(call);

    __verbose("Done filling x-loop body \n");
}

void PACXXNativeBarrier::fillLoopHeader(BasicBlock *loopHeader, AllocaInst *loopVar, AllocaInst *loopMax,
                                         BasicBlock *trueBB, BasicBlock *falseBB) {
    __verbose("Filling loop header");

    LoadInst *load = new LoadInst(loopVar, "load", loopHeader);
    LoadInst *loadMax = new LoadInst(loopMax, "loadMax", loopHeader);

    ICmpInst *cmp = new ICmpInst(*loopHeader, ICmpInst::ICMP_SLT, load, loadMax, "cmp");
    BranchInst::Create(trueBB, falseBB, cmp, loopHeader);

    __verbose("Finished filling loop header");
}

void PACXXNativeBarrier::fillLoopEnd(BasicBlock *loopEnd, BasicBlock *branchBB,
                                      AllocaInst *loopValue, Type *int32_type, unsigned incVal) {
    __verbose("filling loop End");

    LoadInst *load = new LoadInst(loopValue, "load", loopEnd);
    BinaryOperator *inc = BinaryOperator::CreateAdd(load, ConstantInt::get(int32_type, incVal), "inc", loopEnd);
    new StoreInst(inc, loopValue, loopEnd);
    BranchInst::Create(branchBB, loopEnd);

    __verbose("Finished filling loop end");
}

char PACXXNativeBarrier::ID = 0;

INITIALIZE_PASS_BEGIN(PACXXNativeBarrier, "native-barrier",
                "Native barrier", true, true)
INITIALIZE_PASS_DEPENDENCY(PACXXNativeLivenessAnalyzer)
INITIALIZE_PASS_END(PACXXNativeBarrier, "native-barrier",
                "Native barrier", true, true)

namespace llvm {
    llvm::Pass *createPACXXNativeBarrierPass() {
        return new PACXXNativeBarrier();
    }
}
