/**
 * @file   vectorizationAnalysis.h
 * @date   28.03.2012, 19.02.2015
 * @author Ralf Karrenberg, Simon Moll
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 * Requires information about user-defined functions to compute exact information for calls.
 * Requires information about the target SIMD width to compute alignment information.
 * Requires loop information.
 *
 * In contrast to the other WFV passes, this one is meant to be independent of WFVInfo s.t.
 * it can be reused by other projects.
 *
 */
#ifndef _VECTORIZATIONANALYSIS_H
#define	_VECTORIZATIONANALYSIS_H

#include "llvm/Pass.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Analysis/TargetTransformInfo.h"

#include "wfv/analysis/analysisCfg.h"

#include "wfv/utils/wfvTools.h"

namespace WFV {
class ValueInfoMap;
class FunctionInfoMap;
}

namespace llvm {
class Value;
class Constant;
class PHINode;
class LoopInfo;
class Instruction;
class BranchInst;
class Loop;
struct PostDominatorTree;
class DominatorTree;
class LoadInst;
class StoreInst;
class GetElementPtrInst;
class SelectInst;
class CallInst;
}

using namespace llvm;


//namespace {

class VectorizationAnalysis : public FunctionPass
{
public:
    static char ID __attribute__((unused)); // Pass identification, replacement for typeid.

    // We need an empty constructor for the PassManager infrastructure,
    // but it should never be called!
    VectorizationAnalysis();
    VectorizationAnalysis(Function*                   scalarFn,
                          const Function*             simdFn,
                          const unsigned              vectorizationFactor,
                          const int                   maskPosition,
                          TargetTransformInfo		  *TTI,
                          const WFV::ValueInfoMap*    valueInfoMap,
                          const WFV::FunctionInfoMap* functionInfoMap,
                          const bool                  disableMemAccessAnalysis,
                          const bool                  disableControlFlowDivAnalysis,
                          const bool                  disableAllAnalyses,
						  const bool				  pacxx,
                          const bool                  verbose,
                          bool*                       failure);

	~VectorizationAnalysis();

    virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    virtual bool doInitialization(Module& M);
    virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
    virtual void print           (raw_ostream& O, const Module* M) const;


#ifdef WFV_ENABLE_LEGACY_API
    // Typedefs required to be "public" for operator<<(), nothing else!
    // First:  true  edge reachable
    // Second: false edge reachable
    typedef DenseMap<const BranchInst*, std::pair<bool, bool> > EdgeMapType;

    struct DivergenceInfo
    {
        DivergenceInfo() : mReference(nullptr), mReachableEdges(nullptr) {}
        ~DivergenceInfo() { if (mReachableEdges) delete mReachableEdges; }
        const BasicBlock* mReference;
        EdgeMapType*      mReachableEdges;
    };
    typedef DenseMap<const BasicBlock*, DivergenceInfo*> ReachableEdgesMapType;
#endif
private:
    const LoopInfo*             mLoopInfo;
    const PostDominatorTree*    mPostDomTree;
    const DominatorTree*        mDomTree;

    Function*                   mScalarFunction;
    const Function*             mSimdFunction;
    TargetTransformInfo			*mTTI;

    const int                   mMaskPosition;

    const WFV::ValueInfoMap*    mValueInfoMap;
    const WFV::FunctionInfoMap* mFunctionInfoMap;

    const bool                  mDisableMemAccessAnalysis;
    const bool                  mDisableControlFlowDivAnalysis;
    const bool                  mDisableAllAnalyses;
	const bool 					mPacxx;
    const bool                  mVerbose;
    bool*                       mFailure;

    bool markValueAs(Value* value, const char* mark);

    ///////////////////////////////////////////////////////////////////////////
    // Place marks according to user calls to addSIMDMapping / addSIMDSemantics.
    ///////////////////////////////////////////////////////////////////////////

    void setMarksFromOutside(Value*                   value,
                             const WFV::ValueInfoMap& valueInfoMap);
    void setUserDefinedMarks(Function*                   scalarFn,
                             const WFV::ValueInfoMap&    valueInfoMap,
                             const WFV::FunctionInfoMap& functionInfoMap);

    ///////////////////////////////////////////////////////////////////////////
    // No Analysis
    ///////////////////////////////////////////////////////////////////////////

    void analyzeNothing(Function* scalarFn, const bool uniformReturn);

    ///////////////////////////////////////////////////////////////////////////
    // Uniform Analysis
    ///////////////////////////////////////////////////////////////////////////


	void analyzePACXX(Function *scalarFn);

    void analyzeUniformInfo(Function*                   scalarFn,
                            const bool                  uniformReturn,
                            const SmallVector<bool, 4>& uniformArgs);

    bool recursivelyMarkVarying(Instruction* inst, BasicBlock* block);

    ///////////////////////////////////////////////////////////////////////////
    // Divergence Analysis
    ///////////////////////////////////////////////////////////////////////////
#ifdef WFV_ENABLE_LEGACY_API
    ReachableEdgesMapType mReachableEdgeMap;

    typedef DenseMap<const BasicBlock*, SmallPtrSet<const BasicBlock*, 2>* > VisitedEdgeMapType;
    VisitedEdgeMapType mVisitedEdgeMap;
#endif

    bool markAlwaysByAllBlocks(Function* scalarFn);
    bool markDivergentBlocks(Function* scalarFn);

    bool markMandatoryBlocks(Function* scalarFn);
    bool markABAONBlocks(Function* scalarFn);
#ifdef WFV_ENABLE_LEGACY_API
    void findReachableEdges(Function* scalarFn);
    void visitEdge(const BasicBlock* block,
                   const BasicBlock* parent);

    bool joinEdgeMaps(EdgeMapType&       edgeMap1,
                      const EdgeMapType& edgeMap2);

    bool insertReachableEdge(EdgeMapType&      edgeMap,
                             const BranchInst* branch,
                             const BasicBlock* block);

    bool removePostDominatedEdges(const BasicBlock*        block,
                                  DivergenceInfo&          info,
                                  const PostDominatorTree& postDomTree);

    bool allPredsVisitedOrBackEdge(const BasicBlock* block) const;

    bool allSuccessorsVisited(const BasicBlock* block) const;

    const BasicBlock* getTransitiveReference(const BasicBlock* block) const;
#endif
    // efficient implementation of the disjoint paths criterion
    // used for isDivergent() [works]
    bool checkForDisjointPaths(const BasicBlock*             block,
    				bool doNotBranchToOuterLoops,
                    ConstBlockSet & divergenceCausingBlocks) const;

    // used for isMandatoyExit() [work-in-progress]
    bool checkForDisjointLoopPaths(const BasicBlock*             block,
    				const Loop * loop,
					BlockSet * divCauseBlocks) const;


    Loop *
    getCommonLoop(const BasicBlock * A, const BasicBlock * B) const;

    bool isDivergent(BasicBlock*             block,
                     SmallVector<Value*, 2>& divergenceCausingBlocks) const;
    bool isMandatory(const BasicBlock* block,
                     const LoopInfo&   loopInfo) const;
    bool isMandatoryExit(const BasicBlock* block,
                         const Loop*       loop) const;
#ifdef WFV_ENABLE_LEGACY_API
    bool isDivergentDueToDisjointPaths(const BasicBlock& block,
                                       const BasicBlock& truePathStart,
                                       const BasicBlock& falsePathStart,
                                       const BasicBlock& truePathEnd,
                                       const BasicBlock& falsePathEnd,
                                       const BasicBlock& branchParent) const;

    bool hasDisjointPaths(const WFV::PathVecType& pathsToExit,
                          const WFV::PathVecType& pathsToLatch,
                          const bool              ignoreStartBlock) const;
    bool pathsAreDisjoint(const WFV::PathType& pathsToExit,
                          const WFV::PathType& pathsToLatch,
                          const bool           ignoreStartBlock) const;

    bool hasPathWithBackedge(const BasicBlock&            block,
                             const BasicBlock&            target,
                             const Loop*                  allowedBackedge,
                             const bool                   allowParentLoopBackedges,
                             DenseSet<const BasicBlock*>& visitedBlocks) const;
#endif
    ///////////////////////////////////////////////////////////////////////////
    // Updating of phis that depend on divergent control-flow.
    ///////////////////////////////////////////////////////////////////////////

    bool updateUniformPhisWithDivergenceInfo(Function* scalarFn);

    ///////////////////////////////////////////////////////////////////////////
    // Updating of operations with possible side effects that depend on
    // divergent control-flow.
    ///////////////////////////////////////////////////////////////////////////

    bool updateUniformSideEffectOperations(Function* scalarFn);

    ///////////////////////////////////////////////////////////////////////////
    // Updating of uniform allocas.
    ///////////////////////////////////////////////////////////////////////////

    bool updateUniformAllocas(Function* scalarFn);

    ///////////////////////////////////////////////////////////////////////////
    // Updating of values whose live range crosses a loop boundary.
    ///////////////////////////////////////////////////////////////////////////

    bool updateUniformLALBValues(Function* scalarFn);
    bool updateUniformLALBValues(Loop* loop);
    PHINode* findLoopPhiForInstruction(Instruction* inst, Loop* loop);

    ///////////////////////////////////////////////////////////////////////////
    // Marking of loops.
    ///////////////////////////////////////////////////////////////////////////

    bool markDivergentLoops(const LoopInfo& loopInfo);
    bool markDivergentLoop(Loop* loop, const LoopInfo& loopInfo);
    bool isLoopDivergent(Loop* loop, const LoopInfo& loopInfo);

	void markNestedDivergentTopLevelLoops();
	void markNestedDivergentTopLevelLoops(Loop* loop);

    ///////////////////////////////////////////////////////////////////////////
    // Index & Aliasing Analysis.
    ///////////////////////////////////////////////////////////////////////////

    unsigned mVectorizationFactor;

    struct IndexAlignedInfo
    {
        IndexAlignedInfo() : indexInfo(NULL), alignInfo(NULL) {}
        IndexAlignedInfo(const char* ii, const char*ai) : indexInfo(ii), alignInfo(ai) {}
        const char* indexInfo;
        const char* alignInfo;
    };

    void analyzeConsecutiveAlignedInfo(Function* scalarFn);
    void markIndexAlignValueAndOps(Value*                   value,
                                   SmallPtrSet<Value*, 64>& markedValues,
                                   const char**             indexInfo,
                                   const char**             alignInfo);
    void getIndexAlignedInfo(Value*       value,
                             const char** indexInfo,
                             const char** alignInfo) const;
    const char* deriveIndexInfo(Instruction* inst,
                                const std::vector<const char*>& iiVec) const;
    const char* deriveAlignmentInfo(Instruction* inst,
                                    const std::vector<const char*>& aiVec) const;
    const char* deriveIndexInfo(const Constant* c) const;
    const char* deriveAlignedInformation(const Constant* c) const;

    ///////////////////////////////////////////////////////////////////////////
    // Splitting Analysis.
    ///////////////////////////////////////////////////////////////////////////

    void analyzeSplitInfo(Function* scalarFn);

    bool requiresScalarResults(const Instruction& inst) const;
    bool mayRequireGuards(const Instruction& inst) const;

    bool requiresSplit(const Instruction& inst) const;

    bool requiresSplit(const LoadInst& load) const;
    bool requiresSplit(const StoreInst& store) const;
    bool requiresSplit(const GetElementPtrInst& gep) const;
    bool requiresSplit(const PHINode& phi) const;
    bool requiresSplit(const SelectInst& select) const;
    bool requiresSplit(const CallInst& call) const;

    bool hasVaryingIndex(const GetElementPtrInst& gep) const;

    bool requiresSpecialHandling(const Instruction& inst) const;
    bool hasSequentialOperand(const Instruction& inst) const;
    bool operandRequiresSequentialExec(const Value& value) const;

    ///////////////////////////////////////////////////////////////////////////
    // Mask Analysis.
    ///////////////////////////////////////////////////////////////////////////

	bool analyzeMaskInfo(Function* scalarFn);
	bool markAsMask(Instruction* inst);
};

//} // unnamed namespace
#ifdef WFV_ENABLE_LEGACY_API
bool operator==(const VectorizationAnalysis::EdgeMapType& edgeMap1,
                const VectorizationAnalysis::EdgeMapType& edgeMap2);
bool operator!=(const VectorizationAnalysis::EdgeMapType& edgeMap1,
                const VectorizationAnalysis::EdgeMapType& edgeMap2);
raw_ostream& operator<<(raw_ostream& ro, const VectorizationAnalysis::EdgeMapType& edgeMap);
raw_ostream& operator<<(raw_ostream& ro, const VectorizationAnalysis::DivergenceInfo& info);
raw_ostream& operator<<(raw_ostream& ro, const VectorizationAnalysis::ReachableEdgesMapType& edgeMap);
#endif
namespace WFV {
bool VerifyVectorizationAnalysis(const Function& f);
}


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeVectorizationAnalysisPass(PassRegistry&);
FunctionPass*
createVectorizationAnalysisPass(Function*                   scalarFn=nullptr,
                                const Function*             simdFn=nullptr,
                                const unsigned              vectorizationFactor=4,
                                const int                   maskPosition=-1,
								TargetTransformInfo			*TTI=nullptr,
                                const WFV::ValueInfoMap*    valueInfoMap=nullptr,
                                const WFV::FunctionInfoMap* functionInfoMap=nullptr,
                                const bool                  disableMemAccessAnalysis=false,
                                const bool                  disableControlFlowDivAnalysis=false,
                                const bool                  disableAllAnalyses=false,
								const bool 					pacxx=false,
                                const bool                  verbose=false,
                                bool*                       failure=nullptr);
}


#endif	/* _VECTORIZATIONANALYSIS_H */
