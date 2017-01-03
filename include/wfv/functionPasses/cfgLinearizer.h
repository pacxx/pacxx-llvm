/**
 * @file   cfgLinearizer.h
 * @date   06.06.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */
#ifndef _CFGLINEARIZER_H
#define	_CFGLINEARIZER_H

#include "llvm/Pass.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SetVector.h"

#include <deque>

#include "llvm/IR/Dominators.h"

class WFVInfo;
class MaskAnalysis;
class LoopLiveValueAnalysis;

namespace llvm {
class TerminatorInst;
class LoopInfo;
class AllocaInst;
class StoreInst;
class LoadInst;
}

using namespace llvm;


namespace {

class CFGLinearizer : public FunctionPass
{
public:
    static char ID __attribute__((unused)); // Pass identification, replacement for typeid.

    CFGLinearizer();
	~CFGLinearizer();

    virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    virtual bool doInitialization(Module& M);
    virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
    virtual void print           (raw_ostream& O, const Module* M) const;

    bool verify(const Function& f) const;

private:
    const WFVInfo*               mInfo;
    const LoopInfo*              mLoopInfo;
    MaskAnalysis*                mMaskAnalysis;
    const LoopLiveValueAnalysis* mLoopLiveValueAnalysis;

    typedef SmallVector<BasicBlock*, 16> Schedule;

    void collectLoopExitInfo(Function* f);
    void linearize(Function* f);

    enum OutgoingEdgeType
    {
        LEAVE_UNTOUCHED,
        REMOVE,
        REWIRE,
        REWIRE_MULTI,
        NEW
    };

    struct Edge
    {
        BasicBlock* mSource;
        BasicBlock* mTarget;
        Edge(BasicBlock* source, BasicBlock* target) : mSource(source), mTarget(target) {}
        Edge(const Edge& other) : mSource(other.mSource), mTarget(other.mTarget) {}
        bool operator==(const Edge& other) const
        {
            return other.mSource == this->mSource && other.mTarget == this->mTarget;
        }
    };

    struct LinearizeInfo
    {
        BasicBlock*                  mSuccessor;
        OutgoingEdgeType             mEdgeType;
        SmallVector<BasicBlock*, 2>* mNewTargets;
        SmallVector<BasicBlock*, 2>* mNewTargetPreds;
        LinearizeInfo(BasicBlock*                  successor,
                      OutgoingEdgeType             edgeType,
                      SmallVector<BasicBlock*, 2>* newTargets,
                      SmallVector<BasicBlock*, 2>* newTargetPreds)
            : mSuccessor(successor),
            mEdgeType(edgeType),
            mNewTargets(newTargets),
            mNewTargetPreds(newTargetPreds)
        {}
    };

    typedef SmallVector<BasicBlock*, 32> RewireList;
    struct Cluster
    {
        BasicBlock* mEntry;
        BasicBlock* mPostDom;
        SmallPtrSet<BasicBlock*, 2>* mDCBlocks;
        SmallPtrSet<BasicBlock*, 2>* mRewireTargets;
        RewireList* mRewireList;
    };
    typedef DenseMap<BasicBlock*, Cluster*> ClusterMap;
    typedef DenseMap<BasicBlock*, SmallVector<LinearizeInfo*, 2>* > LinearizeInfoMap;

    ClusterMap mClusterMap;
    LinearizeInfoMap mLinearizeInfoMap;

    void determineClusters(Function* f);
    void createClusters(SetVector<BasicBlock*>& dcBlockSet);
    void determineRewireOrders();
    void determineRewireOrder(Cluster& cluster);
    bool hasUnseenNonLatchPred(BasicBlock*                         block,
                               const Cluster&                      cluster,
                               const LoopInfo&                     loopInfo,
                               const SmallPtrSet<BasicBlock*, 16>& scheduledBlocks);
    void determineNewEdges(Function* f);
    void determineNewEdges(BasicBlock* block);
    void getRewireTargets(BasicBlock* block,
                          BasicBlock* succBB,
                          SmallVector<BasicBlock*, 2>& rewireTargets,
                          SmallVector<BasicBlock*, 2>& rewireCausingBlocks);
    BasicBlock* getFirstMandatoryLoopExit(Loop* loop);

    struct BlockInfo
    {
        BlockInfo(BasicBlock* block);
        BasicBlock* mBlock;

        typedef SmallPtrSet<BasicBlock*, 2> OrigPredSetType;
        typedef SmallPtrSet<BasicBlock*, 2> OrigSuccSetType;
        OrigPredSetType mOriginalPredecessors;
        OrigSuccSetType mOriginalSuccessors;
    };
    struct LoopExitInfo
    {
        LoopExitInfo(BasicBlock* exitBB,
                     BasicBlock* exitingBB,
                     const bool  isUniform,
                     const bool  isTargetMandatory,
                     Value*      exitMask,
                     Value*      headerMask);
        BasicBlock* mExitBB;
        BasicBlock* mExitingBB;
        const bool  mIsUniform;
        const bool  mIsTargetMandatory;
        Value*      mExitMask;
        Value*      mHeaderMask;

        SmallVector<Value*, 2> mExitLiveVals;
        SmallVector<Value*, 2> mHeaderLiveVals;
        SmallVector<Value*, 2> mLatchLiveVals;
    };
    // Exits can be queried by their exiting block.
    typedef MapVector<BasicBlock*, LoopExitInfo*> LoopExitMapType;

    // All these maps are used to keep track of what is done during linearization.
    typedef DenseMap<const BasicBlock*, BlockInfo*>       BlockInfoMapType;
    typedef DenseMap<const Loop*,       LoopExitMapType*> LoopExitInfoMapType;
    BlockInfoMapType    mBlockMap;
    LoopExitInfoMapType mLoopExitInfoMap;

    struct MemInfo
    {
        MemInfo(Instruction*                origInst,
                AllocaInst*                 alloca,
                SmallVector<LoadInst*, 2>*  reloads,
                SmallVector<StoreInst*, 2>* stores)
            : mTargetIsPHI(!origInst),
            mOrigInst(origInst),
            mAlloca(alloca),
            mReloads(reloads),
            mStores(stores)
        {}
        ~MemInfo()
        {
            mReloads->clear(); delete mReloads;
            mStores->clear();  delete mStores;
        }

        const bool                  mTargetIsPHI;
        Instruction*                mOrigInst;
        AllocaInst*                 mAlloca;
        SmallVector<LoadInst*, 2>*  mReloads;
        SmallVector<StoreInst*, 2>* mStores;
    };

    // NOTE: We must not use LLVM's ValueToValueMapTy here, since that uses
    //       WeakVH types that automatically null out if the value is destroyed.
    typedef std::vector<MemInfo*>               MemInfoMapType;
    typedef DenseMap<const Value*, Value*>      MaskValueMapType;
    typedef DenseMap<const Value*, BasicBlock*> MaskBlockMapType;
    typedef SmallPtrSet<StoreInst*, 8>          StoreSetType;
    typedef SmallVector<StoreInst*, 2>          StoreVecType;
    typedef SmallVector<LoadInst*, 2>           LoadVecType;
    typedef DenseMap<StoreInst*, StoreSetType*> OverwriteMapType;

    void linearize(Function*         f,
                   MemInfoMapType&   memInfos,
                   MaskValueMapType& maskValueMap,
                   MaskValueMapType& maskPhiValueMap);

    BasicBlock* getNextMandatoryBlock(Schedule::iterator it,
                                      Schedule&          schedule,
                                      const Loop*        loop) const;

    DominatorTreeBase<BasicBlock>* DTB;
    DominatorTreeBase<BasicBlock>* PTB;
    OutgoingEdgeType getOutgoingEdgeType(const BasicBlock* source,
                                         const BasicBlock* target) const;

    TerminatorInst* rewireEdge(TerminatorInst* terminator,
                               BasicBlock*     oldTarget,
                               BasicBlock*     newTarget);

    void createSingleVaryingLoopExitEdge(const Loop*       loop,
                                         BasicBlock*       block,
                                         BasicBlock*       exitTarget,
                                         MemInfoMapType&   memInfos,
                                         MaskValueMapType& maskValueMap,
                                         MaskValueMapType& maskPhiValueMap);

    void repairLoopExitMasks(Function* f);

    void reg2mem(Function*         f,
                 MemInfoMapType&   memInfos,
                 MaskValueMapType& maskValueMap,
                 MaskValueMapType& maskPhiValueMap,
                 MaskValueMapType& allocaValueMap,
                 MaskBlockMapType& maskBlockMap,
                 StoreSetType&     undefStoreSet);
    void repairOverlappingPaths(MemInfoMapType&   memInfos,
                                MaskValueMapType& maskValueMap,
                                MaskValueMapType& maskPhiValueMap,
                                MaskValueMapType& allocaValueMap,
                                MaskBlockMapType& maskBlockMap,
                                StoreSetType&     undefStoreSet);
    void findOverwritingStores(const StoreVecType& stores,
                               const StoreSetType& undefStoreSet,
                               OverwriteMapType&   overwriteMap,
                               const BasicBlock*   doNotTraverse=nullptr) const;
    void mem2reg(Function*         f,
                 MemInfoMapType&   memInfos,
                 MaskValueMapType& maskValueMap,
                 MaskValueMapType& maskPhiValueMap,
                 MaskValueMapType& allocaValueMap,
                 MaskBlockMapType& maskBlockMap);

    const Loop* getInnermostLoopForAlloca(const LoadVecType&  reloads,
                                          const StoreVecType& stores) const;
};

} // unnamed namespace


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeCFGLinearizerPass(PassRegistry&);
FunctionPass* createCFGLinearizerPass();
}


#endif	/* _CFGLINEARIZER_H */
