/**
 * @file   cfgLinearizer.cpp
 * @date   06.06.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */

#include "wfv/utils/metadata.h"
#include "wfv/utils/wfvTools.h"

#include "wfv/functionPasses/cfgLinearizer.h"
#include "wfv/wfvInfo.h"
#include "wfv/wfvConfig.h"
#include "wfv/analysis/maskAnalysis.h"
#include "wfv/analysis/loopLiveValueAnalysis.h"

#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/Scalar.h" // SROA
#include "llvm/Transforms/Utils/SSAUpdater.h"
#include "llvm/Transforms/Utils/PromoteMemToReg.h"
#include "llvm/Transforms/Utils/Local.h" // DemoteRegToStack()

#include <stdexcept>

// Copied from DemoteRegToStack.cpp
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/Transforms/Utils/Local.h"

using namespace llvm;


// These are modified version of llvm::DemoteRegToStack and llvm::DemotePHIToStack from llvm/lib/Transform/Utils/DemoteRegToStack.cpp
// Different to the original versions, a store slot is created even for Instructions/PHI nodes without llvm::Users
// This is necessary, as these values may be referenced by users outside of LLVM (in particular WFV's MaskGraph)


/// DemoteRegToStack - This function takes a virtual register computed by an
/// Instruction and replaces it with a slot in the stack frame, allocated via
/// alloca.  This allows the CFG to be changed around without fear of
/// invalidating the SSA information for the value.  It returns the pointer to
/// the alloca inserted to create a stack slot for I.
static
AllocaInst *
SafelyDemoteRegToStack(Instruction &I, bool VolatileLoads,
                                   Instruction *AllocaPoint) {

  // Create a stack slot to hold the value.
  AllocaInst *Slot;
  if (AllocaPoint) {
    Slot = new AllocaInst(I.getType(), 0, nullptr,
                          I.getName()+".reg2mem", AllocaPoint);
  } else {
    Function *F = I.getParent()->getParent();
    Slot = new AllocaInst(I.getType(), 0, nullptr, I.getName()+".reg2mem",
                          &*F->getEntryBlock().begin());
  }

  // Change all of the users of the instruction to read from the stack slot.
  while (!I.use_empty()) {
    Instruction *U = cast<Instruction>(I.user_back());
    if (PHINode *PN = dyn_cast<PHINode>(U)) {
      // If this is a PHI node, we can't insert a load of the value before the
      // use.  Instead insert the load in the predecessor block corresponding
      // to the incoming value.
      //
      // Note that if there are multiple edges from a basic block to this PHI
      // node that we cannot have multiple loads. The problem is that the
      // resulting PHI node will have multiple values (from each load) coming in
      // from the same block, which is illegal SSA form. For this reason, we
      // keep track of and reuse loads we insert.
      DenseMap<BasicBlock*, Value*> Loads;
      for (unsigned i = 0, e = PN->getNumIncomingValues(); i != e; ++i)
        if (PN->getIncomingValue(i) == &I) {
          Value *&V = Loads[PN->getIncomingBlock(i)];
          if (!V) {
            // Insert the load into the predecessor block
            V = new LoadInst(Slot, I.getName()+".reload", VolatileLoads,
                             PN->getIncomingBlock(i)->getTerminator());
          }
          PN->setIncomingValue(i, V);
        }

    } else {
      // If this is a normal instruction, just insert a load.
      Value *V = new LoadInst(Slot, I.getName()+".reload", VolatileLoads, U);
      U->replaceUsesOfWith(&I, V);
    }
  }


  // Insert stores of the computed value into the stack slot. We have to be
  // careful if I is an invoke instruction, because we can't insert the store
  // AFTER the terminator instruction.
  BasicBlock::iterator InsertPt;
  if (!isa<TerminatorInst>(I)) {
    InsertPt = ++I.getIterator();
    for (; isa<PHINode>(InsertPt) || InsertPt->isEHPad(); ++InsertPt)
        /* empty */;   // Don't insert before PHI nodes or landingpad instrs.
  } else {
      InvokeInst &II = cast<InvokeInst>(I);
      if (II.getNormalDest()->getSinglePredecessor())
          InsertPt = II.getNormalDest()->getFirstInsertionPt();
  }

  new StoreInst(&I, Slot, &*InsertPt);
  return Slot;
}

static
AllocaInst * SafelyDemotePHIToStack(PHINode *P, Instruction *AllocaPoint) {
  // Create a stack slot to hold the value.
  AllocaInst *Slot;
  if (AllocaPoint) {
    Slot = new AllocaInst(P->getType(), 0, nullptr,
                          P->getName()+".reg2mem", AllocaPoint);
  } else {
    Function *F = P->getParent()->getParent();
    Slot = new AllocaInst(P->getType(), 0, nullptr, P->getName()+".reg2mem",
                          &F->getEntryBlock().front());
  }

  // Iterate over each operand inserting a store in each predecessor.
  for (unsigned i = 0, e = P->getNumIncomingValues(); i < e; ++i) {
    if (InvokeInst *II = dyn_cast<InvokeInst>(P->getIncomingValue(i))) {
      assert(II->getParent() != P->getIncomingBlock(i) &&
             "Invoke edge not supported yet"); (void)II;
    }
    new StoreInst(P->getIncomingValue(i), Slot,
                  P->getIncomingBlock(i)->getTerminator());
  }

  // Insert a load in place of the PHI and replace all uses.
  BasicBlock::iterator InsertPt = P->getIterator();

  for (; isa<PHINode>(*InsertPt) || isa<LandingPadInst>(*InsertPt); ++InsertPt)
    /* empty */;   // Don't insert before PHI nodes or landingpad instrs.

  Value *V = new LoadInst(Slot, P->getName()+".reload", &*InsertPt);
  P->replaceAllUsesWith(V);

  // Delete PHI.
  P->eraseFromParent();
  return Slot;
}








char CFGLinearizer::ID = 0;
// NOTE: The order of initialized dependencies is important
//       to prevent 'Unable to schedule' errors!
INITIALIZE_PASS_BEGIN(CFGLinearizer, "cfgLinearizer", "CFGLinearizer", false, false)
INITIALIZE_PASS_DEPENDENCY(WFVInfo)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(MaskAnalysis)
INITIALIZE_PASS_DEPENDENCY(LoopLiveValueAnalysis)
INITIALIZE_PASS_END(CFGLinearizer, "cfgLinearizer", "CFGLinearizer", false, false)

// Public interface to the CFGLinearizer pass
FunctionPass*
llvm::createCFGLinearizerPass()
{
	return new CFGLinearizer();
}


CFGLinearizer::CFGLinearizer()
    : FunctionPass(ID),
    mInfo(nullptr),
    mLoopInfo(nullptr),
    mMaskAnalysis(nullptr),
    mLoopLiveValueAnalysis(nullptr)
{
    initializeCFGLinearizerPass(*PassRegistry::getPassRegistry());
}

CFGLinearizer::~CFGLinearizer()
{
    // Deleted by PassManager.
    mInfo                  = nullptr;
    mLoopInfo              = nullptr;
    mMaskAnalysis          = nullptr;
    mLoopLiveValueAnalysis = nullptr;
}

void
CFGLinearizer::releaseMemory()
{
}

void
CFGLinearizer::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<WFVInfo>();
    AU.addPreserved<WFVInfo>();

    AU.addRequired<MaskAnalysis>();
    AU.addPreserved<MaskAnalysis>();

    AU.addRequired<LoopInfoWrapperPass>();
    AU.addPreserved<LoopInfoWrapperPass>();

    AU.addRequired<LoopLiveValueAnalysis>();
    AU.addPreserved<LoopLiveValueAnalysis>();
}

bool
CFGLinearizer::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
CFGLinearizer::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
CFGLinearizer::runOnFunction(Function& F)
{
    mInfo                  = &getAnalysis<WFVInfo>();
    mLoopInfo              = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    mMaskAnalysis          = &getAnalysis<MaskAnalysis>();
    mLoopLiveValueAnalysis = &getAnalysis<LoopLiveValueAnalysis>();

    if(mInfo->mVerbose) {
        outs() << "\n#########################################################\n";
        outs() << "## CFG LINEARIZATION\n";
        outs() << "#########################################################\n";
    }

    // If an error occurred in one of the previous phases, abort.
    assert (mInfo->mFailure);
    if (*mInfo->mFailure) return false;

    // Collect information about loop exits etc. that we need after linearization.
    collectLoopExitInfo(&F);
    linearize(&F);

    if(mInfo->mVerbose) {
        outs() << "after cfgLinearizer \n";
        F.print(outs());
    }

    if (!verify(F)) *mInfo->mFailure = true;

    return false;
}

void
CFGLinearizer::print(raw_ostream& O, const Module* M) const
{
}

namespace {

bool
verifyIncomingEdges(const Function& f)
{
    for (auto &BB : f)
    {
        for (auto &I : BB)
        {
            if (!isa<PHINode>(I)) break;
            const PHINode* phi = cast<PHINode>(&I);
            if (phi->getNumIncomingValues() != WFV::getNumIncomingEdges(BB))
            {
                errs() << "INTERNAL ERROR: ";
                errs() << "phi has different number of incoming edges than parent block ('";
                errs() << BB.getName() << "'): " << *phi << "\n";
                return false;
            }
            for (const_pred_iterator P=pred_begin(&BB), PE=pred_end(&BB); P!=PE; ++P)
            {
                if (phi->getBasicBlockIndex(*P) == -1)
                {
                    errs() << "INTERNAL ERROR: ";
                    errs() << "block '" << (*P)->getName() << "' has incoming edge ";
                    errs() << "that does not exist in phi: " << *phi << "\n";
                    return false;
                }
            }
            for (unsigned i=0, e=phi->getNumIncomingValues(); i<e; ++i)
            {
                BasicBlock* incBB = phi->getIncomingBlock(i);
                bool found = false;
                for (const_pred_iterator P=pred_begin(&BB), PE=pred_end(&BB); P!=PE; ++P)
                {
                    if (incBB != *P) continue;
                    found = true;
                    break;
                }
                if (!found)
                {
                    errs() << "INTERNAL ERROR: ";
                    errs() << "incoming block of phi ('" << incBB->getName() << "') ";
                    errs() << "is no incoming edge of parent block ('" << BB.getName();
                    errs() << "') of phi: " << *phi << "\n";
                    return false;
                }
            }
        }
    }

    return true;
}

} // unnamed namespace

bool
CFGLinearizer::verify(const Function& f) const
{
    bool verified = true;

    // Test if there are any blocks without predecessor (except for entry block).
    for (auto &BB : f)
    {
        if (&BB == &f.getEntryBlock()) continue;

        if (pred_begin(&BB) == pred_end(&BB))
        {
            errs() << "INTERNAL ERROR: Block '" << BB.getName()
                << "' has no predecessors after CFG linearization!\n";
            verified = false;
        }
    }

    // Make sure all incoming edges to a block match incoming blocks of phis.
    verified &= verifyIncomingEdges(f);

    return verified;
}



void
CFGLinearizer::linearize(Function* f)
{
    assert (f);

    determineClusters(f);
    determineRewireOrders();
    determineNewEdges(f);

    MemInfoMapType   memInfos;
    MaskValueMapType maskValueMap;
    MaskValueMapType maskPhiValueMap;
    MaskValueMapType allocaValueMap;
    MaskBlockMapType maskBlockMap;
    StoreSetType     undefStoreSet;
    reg2mem(f, memInfos, maskValueMap, maskPhiValueMap, allocaValueMap, maskBlockMap, undefStoreSet);

    //outs() << *f;
    //f->viewCFGOnly();

    if(mInfo->mVerbose) {
        for (auto &MI : memInfos) {
            if (!MI->mTargetIsPHI) continue;
            assert(MI->mReloads->size() == 1);
            BasicBlock *parentBB = MI->mReloads->front()->getParent();
            if (WFV::hasMetadata(parentBB, WFV::WFV_METADATA_OPTIONAL)) continue;

            const StoreVecType &stores = *MI->mStores;
            OverwriteMapType overwriteMap;
            findOverwritingStores(stores, undefStoreSet, overwriteMap);
            if (!overwriteMap.empty()) {
                errs() << "WARNING: Stores overwrite each other before linearization!\n";
                errs() << "         (This is most likely because of a critical edge.)\n";
                for (const auto &pair : overwriteMap) {
                    const StoreInst *store = pair.first;
                    outs() << "\nstore overwrites others ('" << store->getParent()->getName();
                    outs() << "'): " << *store << "\n";
                    for (const auto &ovwStore : *pair.second) {
                        outs() << " * ('" << ovwStore->getParent()->getName();
                        outs() << "'): " << *ovwStore << "\n";
                    }
                }
            }
            assert(overwriteMap.empty() &&
                   "must not have overlapping paths before linearization!");
        }
    }

    // For temporary sanity check below.
    typedef SmallPtrSet<BasicBlock*, 2> TMPTYPE1;
    typedef DenseMap<BasicBlock*, TMPTYPE1*> TMPTYPE2;
    TMPTYPE2 tmpRetainedEdges;
    if(mInfo->mVerbose) {
        for (auto &BB : *f) {
            if (mLoopInfo->isLoopHeader(&BB)) {
                tmpRetainedEdges[&BB] = new TMPTYPE1();
                for (pred_iterator P = pred_begin(&BB); P != pred_end(&BB); ++P) {
                    tmpRetainedEdges[&BB]->insert(*P);
                }
                assert(tmpRetainedEdges[&BB]->size() == 2);
                continue;
            }
            // ... store more edges that should not change.
        }
    }

    // Linearize function, thereby rewiring edges that target MANDATORY blocks.
    linearize(f, memInfos, maskValueMap, maskPhiValueMap);

    // Temporary sanity check: We never change predecessors of loop headers.
    if(mInfo->mVerbose) {
        for (auto &pair : tmpRetainedEdges) {
            BasicBlock *block = pair.first;
            for (auto BB : *pair.second) {
                bool found = false;
                for (pred_iterator P = pred_begin(block); P != pred_end(block); ++P) {
                    if (*P != BB) continue;
                    found = true;
                    break;
                }
                assert(found);
            }
            delete pair.second;
        }
    }


    // Repair any dominance relations that were destroyed.
    if(mInfo->mVerbose) outs() << "\nRepairing SSA form after CFG linearization...\n";

    repairOverlappingPaths(memInfos, maskValueMap, maskPhiValueMap, allocaValueMap, maskBlockMap, undefStoreSet);
    mem2reg(f, memInfos, maskValueMap, maskPhiValueMap, allocaValueMap, maskBlockMap);

    // Update mask analysis.
    mMaskAnalysis->mapMaskValues(maskValueMap);
    mMaskAnalysis->mapMaskValues(maskPhiValueMap);

    if(mInfo->mVerbose) outs() << "\nLinearization of function finished!\n";

    // We invalidate insert points of masks since they may have been removed.
    mMaskAnalysis->invalidateInsertPoints();
}

// Collect information about loop exits etc. that we need after linearization.
void
CFGLinearizer::collectLoopExitInfo(Function* f)
{
    assert (f);

    SmallVector<Loop*, 2> loopStack;
    for (LoopInfo::iterator L=mLoopInfo->begin(), LE=mLoopInfo->end(); L!=LE; ++L)
    {
        loopStack.push_back(*L);
    }

    while (!loopStack.empty())
    {
        Loop* loop = loopStack.pop_back_val();

        for (auto &SL : *loop)
        {
            loopStack.push_back(SL);
        }

        if (!WFV::hasMetadata(loop, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE)) continue;

        LoopExitMapType* loopExitMap = mLoopExitInfoMap[loop];
        if (!loopExitMap)
        {
            loopExitMap = new LoopExitMapType();
            mLoopExitInfoMap[loop] = loopExitMap;
        }

        SmallVector<BasicBlock*, 2> exitBlocks;
        loop->getExitBlocks(exitBlocks);
        for (auto &exitBB : exitBlocks)
        {
            BasicBlock* exitingBB = exitBB->getUniquePredecessor();
            assert (exitingBB && "loop not canonicalized?!");

            const bool targetIsMandatory = WFV::hasMetadata(exitBB, WFV::WFV_METADATA_MANDATORY);
            const bool isUniform = WFV::hasMetadata(exitingBB->getTerminator(),
                                                    WFV::WFV_METADATA_OP_UNIFORM);

            // If the exit branch is uniform, we do not maintain an exit mask
            // since it would be equal to the loop active mask.
            const bool needLoopExitMasks = !isUniform;

            // The exiting block mask is given by the exit mask of the edge.
            Value* exitMask = needLoopExitMasks ?
                mMaskAnalysis->getLoopExitMaskUpdate(*loop, *exitingBB) : // disj. of all x updates?
                mMaskAnalysis->getExitMask(*exitingBB, *exitBB);

            // The header mask of the mandatory exit block is given by the
            // combined exit mask phi. Since this doesn't exist, we first have
            // to join the exit mask phis of all mandatory exits.
            // The header mask of each optional exit block is given by the loop
            // mask phi (the active mask).
            Value* headerMask = needLoopExitMasks ?
                mMaskAnalysis->getLoopExitMaskPhi(*loop, *exitingBB) :
                mMaskAnalysis->getLoopMaskPhi(*loop);

            LoopExitInfo* LEI = new LoopExitInfo(exitBB,
                                                 exitingBB,
                                                 isUniform,
                                                 targetIsMandatory,
                                                 exitMask,
                                                 headerMask);
            (*loopExitMap)[exitingBB] = LEI;
        }

        LoopLiveValueAnalysis::LoopLiveValueInfo& liveValues =
            *mLoopLiveValueAnalysis->getLiveValueInfo(loop);

        if (liveValues.empty()) continue;

        // Get loop result information.
        LoopResults& loopResults = *mLoopLiveValueAnalysis->getLoopResults(loop);

        // TODO: We basically duplicate the information from loopLiveValueAnalysis here.
        // TODO: This is not used anyway at the moment because we repair every incoming
        //       value of every phi and blend in repairSSA().
        for (auto &exitBB : exitBlocks)
        {
            BasicBlock* exitingBB = exitBB->getUniquePredecessor();
            LoopExitInfo* LEI = (*loopExitMap)[exitingBB];
            for (auto &I : *exitBB)
            {
                if (!isa<PHINode>(I)) break;
                PHINode* phi = cast<PHINode>(&I);
                for (unsigned i=0, e=phi->getNumIncomingValues(); i<e; ++i)
                {
                    for (auto &liveValue : liveValues)
                    {
                        if (liveValue != phi->getIncomingValue(i)) continue;
                        LEI->mExitLiveVals.push_back(liveValue);
                        LEI->mLatchLiveVals.push_back(loopResults.getResult(liveValue));
                        LEI->mHeaderLiveVals.push_back(loopResults.getResultPhi(liveValue));
                    }
                }
            }
        }
    }

    for (auto &BB : *f)
    {
        BlockInfo* LBI = new BlockInfo(&BB);
        mBlockMap[&BB] = LBI;

        for (pred_iterator P=pred_begin(&BB), PE=pred_end(&BB); P!=PE; ++P)
        {
            LBI->mOriginalPredecessors.insert(*P);
        }
        TerminatorInst* terminator = BB.getTerminator();
        for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
        {
            LBI->mOriginalSuccessors.insert(terminator->getSuccessor(i));
        }
    }
}



////////////////////////////////////////////////////////////////////////////////
void
CFGLinearizer::determineClusters(Function* f)
{
    SetVector<BasicBlock*> dcBlockSet;

    if(mInfo->mVerbose) outs() << "\ndc blocks:\n";
    for (auto &BB : *f)
    {
        if (isa<ReturnInst>(BB.getTerminator())) continue;
        if (!WFV::hasMetadata(BB.getTerminator(), WFV::WFV_METADATA_OP_VARYING)) continue;
        dcBlockSet.insert(&BB);
    }

    typedef SmallVector<Value*, 2> VecType;
    DEBUG_WFV_NO_VERBOSE(
        for (auto &BB : *f)
        {
            VecType dcBlocks;
            WFV::getDivergenceCausingBlocks(BB, dcBlocks);
            for (auto &dcBB : dcBlocks)
            {
                assert (dcBlockSet.count(cast<BasicBlock>(dcBB)));
            }
        }
    );

    if (dcBlockSet.empty()) return;

    createClusters(dcBlockSet);

    if(mInfo->mVerbose) {
        std::set<const Cluster *> cSet;
        for (const auto pair : mClusterMap) cSet.insert(pair.second);
        outs() << "\nClusters:\n";
        for (const auto &cluster : cSet) {
            outs() << "Cluster '" << cluster->mEntry->getName() << "':\n";
            outs() << "  post dom: '" << cluster->mPostDom->getName() << "'\n";
            outs() << "  dc blocks:";
            for (const auto dcBB : *cluster->mDCBlocks) {
                outs() << " '" << dcBB->getName() << "'";
            }
            outs() << "\n";
            outs() << "  rewire target set:";
            for (const auto &rtBB : *cluster->mRewireTargets) {
                outs() << " '" << rtBB->getName() << "'";
            }
            outs() << "\n";
        }
        outs() << "\nClusterMap:\n";
        for (const auto &pair : mClusterMap) {
            const BasicBlock *block = pair.first;
            const Cluster *cluster = pair.second;
            outs() << "Block '" << block->getName() << "' -> Cluster '";
            outs() << cluster->mEntry->getName() << "'\n";
        }
        outs() << "\n";
    }

    DEBUG_WFV_NO_VERBOSE(
        for (const auto &BB : dcBlockSet)
        {
            DEBUG_WFV( if (!mClusterMap.count(BB)) outs() << BB->getName() << "\n"; );
            assert (mClusterMap.count(BB));
        }
    );

    DEBUG_WFV_NO_VERBOSE(
        for (auto &BB : *f)
        {
            if (isa<ReturnInst>(BB.getTerminator())) continue;
            if (!WFV::hasMetadata(BB.getTerminator(), WFV::WFV_METADATA_OP_VARYING)) continue;
            DEBUG_WFV( if (!mClusterMap.count(&BB)) outs() << BB.getName() << "\n"; );
            assert (mClusterMap.count(&BB));
        }
    );
}

void
CFGLinearizer::createClusters(SetVector<BasicBlock*>& dcBlockSet)
{
    assert (!dcBlockSet.empty());
    assert (mClusterMap.empty());
    if(mInfo->mVerbose) outs() << "\ncreateClusters()\n";

    DominatorTreeBase<BasicBlock>* PTB = new DominatorTreeBase<BasicBlock>(true);
    PTB->recalculate(*(*dcBlockSet.begin())->getParent());

    // Create a cluster for each DC block and initialize.
    for (auto BB : dcBlockSet)
    {
        Cluster* cluster = new Cluster();
        cluster->mEntry = BB;
        cluster->mPostDom = PTB->getNode(BB)->getIDom()->getBlock();
        cluster->mDCBlocks = new SmallPtrSet<BasicBlock*, 2>();
        cluster->mDCBlocks->insert(BB);
        cluster->mRewireTargets = new SmallPtrSet<BasicBlock*, 2>();

        // Initialize unordered rewire target set.
        SmallVector<Value*, 2> rewireTargets;
        WFV::getRewireTargetsOfDCBlock(*BB, rewireTargets);
        for (auto RT : rewireTargets)
        {
            cluster->mRewireTargets->insert(cast<BasicBlock>(RT));
        }

        // Ordered rewire target list is filled in determineRewireOrder().
        cluster->mRewireList = new RewireList();
        mClusterMap[BB] = cluster;
    }

    bool changed = true;
    while (changed)
    {
        changed = false;
        for (auto BB0 : dcBlockSet)
        {
            Cluster* cluster0 = mClusterMap[BB0];
            assert (cluster0);
            //Loop* loop = mLoopInfo->getLoopFor(BB0);

            SmallPtrSet<BasicBlock*, 2> remapSet;
            for (auto BB1 : dcBlockSet)
            {
                if (BB0 == BB1) continue;

                // If BB1 already belongs to cluster0, do not merge.
                Cluster* cluster1 = mClusterMap[BB1];
                assert (cluster1);
                if (cluster0 == cluster1) continue;

                // If BB1 is the post dominator of cluster0, do not merge.
                if (cluster0->mPostDom == BB1) continue;

                // If BB1 is not reachable from BB0, do not merge.
                // Disallow passing post dominator, allow loop exits, disallow any back edges.
                if (!WFV::isReachable(BB1, BB0, cluster0->mPostDom, nullptr, nullptr, true, mLoopInfo)) continue;

                assert (!WFV::isReachable(BB0, BB1, cluster1->mPostDom, nullptr, nullptr, true, mLoopInfo));
                assert (cluster0->mPostDom == cluster1->mPostDom ||
                        WFV::isReachable(cluster0->mPostDom, cluster1->mPostDom));

                if(mInfo->mVerbose) {
                    outs() << "  '" << BB1->getName() << "' can be reached from '";
                    outs() << BB0->getName() << "', merging clusters...\n";
                }

                // BB1 can be reached from BB0.
                // -> Merge cluster1 into cluster0
                cluster0->mDCBlocks->insert(cluster1->mDCBlocks->begin(),
                                            cluster1->mDCBlocks->end());
                cluster0->mRewireTargets->insert(cluster1->mRewireTargets->begin(),
                                                 cluster1->mRewireTargets->end());

                // Remap all blocks that referenced cluster1 to cluster0.
                for (auto pair : mClusterMap)
                {
                    if (pair.second != cluster1) continue;
                    remapSet.insert(pair.first);
                }

                // Delete cluster1.
                delete cluster1;

                changed = true;
                break;
            }

            for (auto BB : remapSet)
            {
                assert (mClusterMap[BB]->mDCBlocks->count(BB));
                mClusterMap[BB] = cluster0;
            }

            if (changed) break;
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
void
CFGLinearizer::determineRewireOrders()
{
    SmallPtrSet<Cluster*, 2> visitedSets;
    for (auto &pair : mClusterMap)
    {
        Cluster* cluster = pair.second;
        if (visitedSets.count(cluster)) continue;
        visitedSets.insert(cluster);
        determineRewireOrder(*cluster);
    }

    // Remove clusters without rewire targets (e.g. MND loop exits of loops with only this exit).
    SmallPtrSet<BasicBlock*, 2> deleteSet;
    for (auto &pair : mClusterMap)
    {
        Cluster* cluster  = pair.second;
        if (!cluster->mRewireList->empty()) continue;
        assert (cluster->mDCBlocks->size() == 1);
        deleteSet.insert(pair.first);
    }
    for (auto BB : deleteSet)
    {
        delete mClusterMap[BB];
        mClusterMap.erase(BB);
    }

    if(mInfo->mVerbose) {
        std::set < Cluster * > cSet;
        for (auto pair : mClusterMap) cSet.insert(pair.second);
        outs() << "\nClusters:\n";
        for (auto &cluster : cSet) {
            outs() << "Cluster '" << cluster->mEntry->getName() << "':\n";
            outs() << "  post dom: '" << cluster->mPostDom->getName() << "'\n";
            outs() << "  dc blocks:";
            for (const auto &dcBB : *cluster->mDCBlocks) {
                outs() << " '" << dcBB->getName() << "'";
            }
            outs() << "\n";
            outs() << "  rewire target set:";
            for (const auto &rtBB : *cluster->mRewireTargets) {
                outs() << " '" << rtBB->getName() << "'";
            }
            outs() << "\n";
            outs() << "  rewire target list:";
            for (const auto &rtBB : *cluster->mRewireList) {
                outs() << " '" << rtBB->getName() << "'";
            }
            outs() << "\n";
        }
        outs() << "\n";
    }
}

void
CFGLinearizer::determineRewireOrder(Cluster& cluster)
{
    assert (cluster.mRewireList->empty());

#if 1
    BasicBlock* startBB = &cluster.mEntry->getParent()->getEntryBlock();
#elif 0
    // This could prove to be more efficient for large functions, since we do not
    // traverse the entire function for every cluster.
    DominatorTreeBase<BasicBlock>* DTB = new DominatorTreeBase<BasicBlock>(false);
    DTB->recalculate(*cluster.mEntry->getParent());
    BasicBlock* startBB = DTB->getNode(cluster.mPostDom)->getIDom()->getBlock();
    assert (startBB);
    assert (startBB == cluster.mEntry ||
            WFV::hasMetadata(startBB->getTerminator(), WFV::WFV_METADATA_OP_UNIFORM));

    Loop* clusterLoop = mLoopInfo->getLoopFor(cluster.mEntry);
    while (clusterLoop && clusterLoop->getParentLoop())
    {
        clusterLoop = clusterLoop->getParentLoop();
    }
    if (clusterLoop)
    {
        startBB = DTB->findNearestCommonDominator(startBB, clusterLoop->getHeader());
    }
#elif 0
    // This would be cleaner, but is problematic because we do not want to allow back edges.
    BasicBlock* startBB = cluster.mEntry;
#endif

    SmallPtrSet<BasicBlock*, 16> scheduledBlocks;
    typedef SmallVector<BasicBlock*, 32> WorkList;
    WorkList workList;
    workList.push_back(startBB);

    while (!workList.empty())
    {
        BasicBlock* block = workList.pop_back_val();

        if (scheduledBlocks.count(block)) continue;
        scheduledBlocks.insert(block);
        if(mInfo->mVerbose) outs() << "determineRewireOrder(" << block->getName() << ")\n";

        Loop* loop = mLoopInfo->getLoopFor(block);
        const bool isHeader = loop && loop->getHeader() == block;
        //const bool postDomIsOutside = loop && !loop->contains(cluster.mPostDom);

        // If this is a non-header MANDATORY block, check if it is MND because
        // of a block of this cluster. This is not necessarily the case, e.g.
        // when clusters overlap without the start blocks being reachable from
        // each other. Thus, the current block is only a rewire target for this
        // cluster, if one of the cluster blocks causes it to be MND.
        if (!isHeader && cluster.mRewireTargets->count(block))
        {
            assert (WFV::hasMetadata(block, WFV::WFV_METADATA_MANDATORY));
            cluster.mRewireList->push_back(block);
        }

        if (block == cluster.mPostDom) continue;

        // If this is a header of a loop, and the post dominator where we
        // want to stop is outside, add loop exits to the worklist.
        if (isHeader)
        {
            SmallVector<BasicBlock*, 2> exitBlocks;
            loop->getExitBlocks(exitBlocks);
            for (auto &exitBB : exitBlocks) // "E"
            {
                BasicBlock* exitingBB = exitBB->getUniquePredecessor(); // "X"
                assert (exitingBB && "loop not simplified?!");

                // Determine whether this is the outermost loop that is left.
                if (loop->getParentLoop() &&
                    loop->getParentLoop()->isLoopExiting(exitingBB))
                {
                    continue; // It is not.
                }

                workList.push_back(exitBB);
            }
        }

        TerminatorInst* TI = block->getTerminator();
        for (unsigned i=0, e=TI->getNumSuccessors(); i<e; ++i)
        {
            BasicBlock* succBB = TI->getSuccessor(i);

            // If the successor is on a higher nesting level, this is a loop exit.
            Loop* targetLoop = mLoopInfo->getLoopFor(succBB);
            if (loop)
            {
                if (loop->isLoopExiting(block) && targetLoop != loop) continue;
                // Also ignore back edges.
                if (loop->getHeader() == succBB && loop->getLoopLatch() == block) continue;
            }

            if (hasUnseenNonLatchPred(succBB, cluster, *mLoopInfo, scheduledBlocks)) continue;
            workList.push_back(succBB);
        }
    }
}

bool
CFGLinearizer::hasUnseenNonLatchPred(BasicBlock*                         block,
                                     const Cluster&                      cluster,
                                     const LoopInfo&                     loopInfo,
                                     const SmallPtrSet<BasicBlock*, 16>& scheduledBlocks)
{
    // Do not recurse into successors for which we have not yet
    // visited all incoming edges (except for loop headers).
    // Ignore predecessors which are additional (uniform) entries
    // to the cluster (= can not be reached from cluster entry).
    SmallVector<const BasicBlock*, 2> unseenPreds;
    for (pred_iterator P=pred_begin(block), PE=pred_end(block); P!=PE; ++P)
    {
        BasicBlock* predBB = *P;
        if (predBB == block) continue;
        if (scheduledBlocks.count(predBB)) continue;
        unseenPreds.push_back(predBB);
        if (unseenPreds.size() > 1) return true;
    }

    if (unseenPreds.empty()) return false;
    assert (unseenPreds.size() == 1);

    const bool unseenPredIsLoopLatch = loopInfo.getLoopFor(block) &&
        loopInfo.getLoopFor(block)->getLoopLatch() == unseenPreds.front();

    assert (!unseenPredIsLoopLatch || loopInfo.isLoopHeader(block));
    return !unseenPredIsLoopLatch;
}

////////////////////////////////////////////////////////////////////////////////
void
CFGLinearizer::determineNewEdges(Function* f)
{
    for (auto &BB : *f)
    {
        determineNewEdges(&BB);
    }

    if(mInfo->mVerbose) {
        outs() << "\nLinearize info:\n";
        for (auto &pair : mLinearizeInfoMap) {
            BasicBlock *block = pair.first;
            auto &edgeInfos = *pair.second;

            outs() << "Block '" << block->getName() << "':\n";
            for (auto &LI : edgeInfos) {
                outs() << "  old successor : '";
                outs() << (LI->mSuccessor ? LI->mSuccessor->getName() : "") << "'\n";
                outs() << "  edge type     : " << LI->mEdgeType << "\n";
                outs() << "  new successors:";
                for (auto &BB : *LI->mNewTargets) {
                    outs() << " '" << BB->getName() << "'";
                }
                outs() << "\n";
                outs() << "  rewire-causing:";
                for (auto &BB : *LI->mNewTargetPreds) {
                    if (!BB) continue;
                    outs() << " '" << BB->getName() << "'";
                }
                outs() << "\n";
            }
        }
    }
}

void
CFGLinearizer::determineNewEdges(BasicBlock* block)
{
    assert (block);
    if(mInfo->mVerbose) outs() << "determineNewEdges('" << block->getName() << "')\n";

    Loop* sourceLoop = mLoopInfo->getLoopFor(block);
    TerminatorInst* TI = block->getTerminator();
    for (unsigned i=0, e=TI->getNumSuccessors(); i<e; ++i)
    {
        BasicBlock* succBB = TI->getSuccessor(i);

        Loop* targetLoop = mLoopInfo->getLoopFor(succBB);

        SmallVector<BasicBlock*, 2>* newTargets = new SmallVector<BasicBlock*, 2>();
        SmallVector<BasicBlock*, 2>* newTargetPreds = new SmallVector<BasicBlock*, 2>();
        OutgoingEdgeType edgeType = LEAVE_UNTOUCHED;
        if (WFV::hasMetadata(succBB, WFV::WFV_METADATA_OPTIONAL) ||
            (targetLoop && targetLoop->getHeader() == succBB))
        {
            newTargets->push_back(succBB);
            newTargetPreds->push_back(nullptr);
        }
        else
        {
            const bool isLoopExit = sourceLoop &&
                                    sourceLoop->isLoopExiting(block) &&
                                    targetLoop != sourceLoop;
            if (isLoopExit)
            {
                // There can be cases where a non-divergent inner loop is left via an exit
                // to the latch of a divergent outer loop (which is MANDATORY). That exit
                // must not be removed. See e.g. LoopNested3UniformInnerLoop.
                if (!WFV::hasMetadata(sourceLoop, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE))
                {
                    edgeType = LEAVE_UNTOUCHED;
                }
                else
                {
                    edgeType = REMOVE;
                }
            }
            else
            {
                getRewireTargets(block, succBB, *newTargets, *newTargetPreds);
                edgeType = newTargets->size() > 1 ? REWIRE_MULTI : REWIRE;
            }
        }

        LinearizeInfo* info = new LinearizeInfo(succBB,
                                                edgeType,
                                                newTargets,
                                                newTargetPreds);
        if (!mLinearizeInfoMap.count(block))
        {
            mLinearizeInfoMap[block] = new SmallVector<LinearizeInfo*, 2>();
        }
        mLinearizeInfoMap[block]->push_back(info);
    }

    const bool isLatch = sourceLoop &&
                         sourceLoop->getLoopLatch() == block;
    if (!isLatch) return;
    if (!WFV::hasMetadata(sourceLoop, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE)) return;

    SmallVector<BasicBlock*, 2>* newTargets = new SmallVector<BasicBlock*, 2>();
    SmallVector<BasicBlock*, 2>* newTargetPreds = new SmallVector<BasicBlock*, 2>();
    BasicBlock* firstExit = getFirstMandatoryLoopExit(sourceLoop);
    if (!firstExit)
    {
        // If there is no appropriate exit block for this loop,
        // rewire to the latch of the next outer loop.
        Loop* parentLoop = sourceLoop->getParentLoop();
        assert (parentLoop);
        firstExit = parentLoop->getLoopLatch();
    }

    newTargets->push_back(firstExit);
    newTargetPreds->push_back(nullptr);
    LinearizeInfo* info = new LinearizeInfo(nullptr,
                                            NEW,
                                            newTargets,
                                            newTargetPreds);
    if (!mLinearizeInfoMap.count(block))
    {
        mLinearizeInfoMap[block] = new SmallVector<LinearizeInfo*, 2>();
    }
    mLinearizeInfoMap[block]->push_back(info);
}

BasicBlock*
CFGLinearizer::getFirstMandatoryLoopExit(Loop* loop)
{
    assert (loop);
    assert (WFV::hasMetadata(loop, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE));

    SmallVector<BasicBlock*, 2> exitBlocks;
    loop->getExitBlocks(exitBlocks);
    //for (auto &exitBB : exitBlocks) // "E"
    for (auto rit=exitBlocks.rbegin(), RE=exitBlocks.rend(); rit!=RE; ++rit)
    {
        BasicBlock* exitBB = *rit;
        BasicBlock* exitingBB = exitBB->getUniquePredecessor(); // "X"
        assert (exitingBB && "loop not simplified?!");

        if (!WFV::hasMetadata(exitBB, WFV::WFV_METADATA_MANDATORY)) continue;

        // Determine whether this is the outermost loop that is left.
        if (loop->getParentLoop() &&
            loop->getParentLoop()->isLoopExiting(exitingBB))
        {
            continue; // It is not.
        }

        return exitBB;
    }

    return nullptr;
}

void
CFGLinearizer::getRewireTargets(BasicBlock* block,
                                BasicBlock* succBB,
                                SmallVector<BasicBlock*, 2>& rewireTargets,
                                SmallVector<BasicBlock*, 2>& rewireCausingBlocks)
{
    assert (block && succBB);
    assert (WFV::hasMetadata(succBB, WFV::WFV_METADATA_MANDATORY));
    assert (rewireTargets.empty());
    assert (rewireCausingBlocks.empty());

    SmallVector<Value*, 2> dcBlocks;
    WFV::getDivergenceCausingBlocks(*succBB, dcBlocks);

    DEBUG_WFV_NO_VERBOSE(
        for (auto &dcBB : dcBlocks)
        {
            assert (mClusterMap.count(cast<BasicBlock>(dcBB)));
        }
    );

    // Since the DC blocks do not contain direct predecessors of varying
    // branches, add them now.
    for (pred_iterator P=pred_begin(succBB), PE=pred_end(succBB); P!=PE; ++P)
    {
        BasicBlock* predBB = *P;
        TerminatorInst* TI = predBB->getTerminator();
        if (isa<ReturnInst>(TI)) continue;
        if (!WFV::hasMetadata(TI, WFV::WFV_METADATA_OP_VARYING)) continue;
        dcBlocks.push_back(predBB);
    }

    // This is only necessary because loop latches do not store dc blocks.
    if (dcBlocks.empty())
    {
        rewireTargets.push_back(succBB);
        return;
    }

    // Find out which disjoint clusters these dc-blocks belong to.
    // Store them deterministically in order.
    SmallPtrSet<Cluster*, 2> clusterSet;
    SmallVector<Cluster*, 2> clusters;
    for (auto &dcBB : dcBlocks)
    {
        // There may be blocks without cluster associated if they do
        // not require any rewiring (e.g. loop exits of loops with one exit).
        if (!mClusterMap.count(cast<BasicBlock>(dcBB))) continue;
        Cluster* cluster = mClusterMap[cast<BasicBlock>(dcBB)];
        if (clusterSet.count(cluster)) continue;
        clusterSet.insert(cluster);
        clusters.push_back(cluster);
    }

    // Get next rewire target for each of the clusters.
    for (auto cluster : clusters)
    {
        assert (WFV::isReachable(succBB,
                                 cluster->mEntry,
                                 cluster->mPostDom,
                                 nullptr,
                                 mLoopInfo->getLoopFor(cluster->mEntry)));

        RewireList& rewireList = *cluster->mRewireList;
        BasicBlock* newTarget = nullptr;

        bool findNext = true;
        RewireList::iterator it = rewireList.begin();
        while (it != rewireList.end())
        {
            BasicBlock* rewireBlock = *it++;
            Loop* rewireLoop = mLoopInfo->getLoopFor(rewireBlock);
            const bool isReachable = WFV::isReachable(block,
                                                      rewireBlock,
                                                      cluster->mPostDom,
                                                      rewireLoop,
                                                      rewireLoop);

            if (!findNext)
            {
                if (isReachable) findNext = true;
                continue;
            }

            if (isReachable) continue;
            Loop* origSuccLoop  = mLoopInfo->getLoopFor(succBB);
            Loop* newTargetLoop = mLoopInfo->getLoopFor(rewireBlock);
            if (origSuccLoop != newTargetLoop) continue;
            newTarget = rewireBlock;
            findNext = false;
        }
        assert (newTarget);

        rewireTargets.push_back(newTarget);
        rewireCausingBlocks.push_back(cluster->mEntry); // required when creating switch
    }

    // If there are multiple clusters from which this block can be reached,
    // but only some of them are divergence-causing, we still need to retain
    // paths for the other clusters. These paths simply retain the original
    // edge, i.e., their rewire target is the original successor.
    // We have to make this explicit, because otherwise the edge may only be
    // rewired to the one target. This may result in additional dynamic tests
    // where control flow came from.
    SmallPtrSet<Cluster*, 2> otherClusters;
    for (auto &pair : mClusterMap)
    {
        Cluster* cluster = pair.second;
        if (clusterSet.count(cluster)) continue;
        if (!WFV::isReachable(succBB,
                              cluster->mEntry,
                              cluster->mPostDom,
                              nullptr,
                              nullptr,
                              true,
                              mLoopInfo))
        {
            continue;
        }
        otherClusters.insert(cluster);
    }

    for (auto cluster : otherClusters)
    {
        rewireTargets.push_back(succBB);
        rewireCausingBlocks.push_back(cluster->mEntry);
    }
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


CFGLinearizer::BlockInfo::BlockInfo(BasicBlock* block)
: mBlock(block)
{
}

CFGLinearizer::LoopExitInfo::LoopExitInfo(BasicBlock* exitBB,
                                          BasicBlock* exitingBB,
                                          const bool  isUniform,
                                          const bool  isTargetMandatory,
                                          Value*      exitMask,
                                          Value*      headerMask)
: mExitBB(exitBB),
    mExitingBB(exitingBB),
    mIsUniform(isUniform),
    mIsTargetMandatory(isTargetMandatory),
    mExitMask(exitMask),
    mHeaderMask(headerMask)
{
}

void
CFGLinearizer::linearize(Function*         f,
                         MemInfoMapType&   memInfos,
                         MaskValueMapType& maskValueMap,
                         MaskValueMapType& maskPhiValueMap)
{
    // Get a list of the different clusters (ordered, since we want to be
    // deterministic).
    SmallPtrSet<Cluster*, 2> clusterSet;
    SmallVector<Cluster*, 2> clusters;
    for (auto &BB : *f)
    {
        if (!mClusterMap.count(&BB)) continue;
        Cluster* cluster = mClusterMap[&BB];
        if (clusterSet.count(cluster)) continue;
        clusters.push_back(cluster);
        clusterSet.insert(cluster);
    }

    // Create alloca in each "header" of divergence-causing block clusters so
    // that we can identify where we were coming from during execution.
    AllocaInst* idxAlloca = nullptr;
    LoadVecType* reloads  = nullptr;
    DenseMap<BasicBlock*, unsigned> indexMap;
    if (!clusters.empty())
    {
        // Create alloca
        Function* parentFn = clusters[0]->mEntry->getParent();
        Instruction* allocaPos = &*parentFn->getEntryBlock().getFirstInsertionPt();
        idxAlloca = new AllocaInst(Type::getInt32Ty(*mInfo->mContext), 0,
                                   nullptr,
                                   "alloca.idx",
                                   allocaPos);
        WFV::setMetadata(idxAlloca, WFV::WFV_METADATA_OP_UNIFORM);

        // Create one store per predecessor of new target (divergence-causing block)
        //  - stored value: identifier of new target
        reloads = new LoadVecType();
        StoreVecType* stores  = new StoreVecType();
        unsigned clusterIdx = 0;
        for (auto cluster : clusters)
        {
            ConstantInt* idxVal = ConstantInt::get(*mInfo->mContext, APInt(32, clusterIdx));
            StoreInst* store = new StoreInst(idxVal, idxAlloca, cluster->mEntry->getTerminator());
            stores->push_back(store);
            WFV::setMetadata(store, WFV::WFV_METADATA_OP_UNIFORM);
            indexMap[cluster->mEntry] = clusterIdx++;
        }

        // There is no "origInst", but we can not supply a nullptr either since
        // this already indicates a phi.
        memInfos.push_back(new MemInfo(idxAlloca, idxAlloca, reloads, stores));
    }

    // Now, linearize/modify CFG.
    for (auto &BB : *f)
    {
        BasicBlock* block = &BB;
        if (!mLinearizeInfoMap.count(block)) continue;
        SmallVector<LinearizeInfo*, 2>& edgeInfos = *mLinearizeInfoMap[block];

        if(mInfo->mVerbose) outs() << "\nrewiring edges of block '" << block->getName() << "'...\n";

        TerminatorInst* terminator = block->getTerminator();
        if (isa<ReturnInst>(terminator))
        {
            if(mInfo->mVerbose) outs() << "  is return block.\n";
            continue; // Nothing to do (no outgoing edges).
        }
        assert (terminator->getNumSuccessors() > 0);

        unsigned numLeaveUntouched = 0;
        unsigned numRemove = 0;
        unsigned numRewire = 0;
        unsigned numRewireMulti = 0;
        unsigned numNew = 0;

        LinearizeInfo* removeInfo = nullptr;
        LinearizeInfo* newExitInfo = nullptr;
        for (const auto &targetInfo : edgeInfos)
        {
            if(mInfo->mVerbose) {
                outs() << "  successor '";
                outs() << (targetInfo->mSuccessor ? targetInfo->mSuccessor->getName() : "new");
                outs() << "'\n";
            }

            switch (targetInfo->mEdgeType)
            {
                case LEAVE_UNTOUCHED:
                {
                    ++numLeaveUntouched;
                    break;
                }
                case REMOVE:
                {
                    ++numRemove;
                    assert (numRemove == 1 && "must not attempt to remove multiple edges!");
                    removeInfo = targetInfo;
                    break;
                }
                case REWIRE:
                {
                    ++numRewire;

                    BasicBlock* target    = targetInfo->mSuccessor;
                    BasicBlock* newTarget = targetInfo->mNewTargets->front();
                    assert (targetInfo->mNewTargets->size() == 1);
                    if (target == newTarget) break;

                    // Rewire edge.
                    // NOTE: This does not influence iteration over successors.
                    terminator = rewireEdge(terminator, target, newTarget);

                    // Immediately replace conditional branches that have both
                    // edges point to the same block by an unconditional branch.
                    if (BranchInst* branch = dyn_cast<BranchInst>(terminator))
                    {
                        if (branch->isConditional() &&
                            branch->getSuccessor(0) == branch->getSuccessor(1))
                        {
                        	BasicBlock * firstSuccBlock = branch->getSuccessor(0);
                            terminator->eraseFromParent();
                            terminator = BranchInst::Create(firstSuccBlock, block);
                            WFV::setMetadata(terminator, WFV::WFV_METADATA_OP_UNIFORM);
                        }
                    }
                    else if (SwitchInst* sw = dyn_cast<SwitchInst>(terminator))
                    {
                        // Do we need/want to simplify this?
                        SmallPtrSet<BasicBlock*, 2> uniqueTargets;
                        SmallPtrSet<BasicBlock*, 2> duplicateTargets;
                        for (SwitchInst::CaseIt it=sw->case_begin(),
                             E=sw->case_end(); it!=E; ++it)
                        {
                            BasicBlock* caseTarget = it->getCaseSuccessor();
                            if (uniqueTargets.count(caseTarget))
                            {
                                duplicateTargets.insert(caseTarget);
                                continue;
                            }
                            uniqueTargets.insert(caseTarget);
                        }
                        // Case values < 0 signal old edges that will be
                        // or were already rewired. If the block is already
                        // a target of the switch, the rewiring is already
                        // done, and we can remove the edge (if the edge
                        // targeted that block before already, it was not
                        // or will not be touched, so removing the edge
                        // does not break anything).
                        // We can only remove one case at a time...
                        bool changed = true;
                        while (changed)
                        {
                            changed = false;
                            for (SwitchInst::CaseIt it=sw->case_begin(),
                                 E=sw->case_end(); it!=E; ++it)
                            {
                                BasicBlock* caseTarget = it->getCaseSuccessor();
                                if (duplicateTargets.count(caseTarget) &&
                                    it->getCaseValue()->getSExtValue() < 0)
                                {
                                    sw->removeCase(it);
                                    changed = true;
                                    break;
                                }
                            }
                        }

                        const unsigned numCases = sw->getNumCases();
                        if (numCases == 2)
                        {
                            // Successor 0 is the default block.
                            BasicBlock* defaultBB = sw->getDefaultDest();
                            assert (sw->getDefaultDest()->getName().startswith("wfv_default"));
                            assert (defaultBB == sw->getSuccessor(0));

                            BasicBlock* target0 = sw->getSuccessor(1);
                            BasicBlock* target1 = sw->getSuccessor(2);
                            ConstantInt* caseVal0 = sw->findCaseDest(target0);
                            Value* condition = sw->getCondition();
                            assert (caseVal0->getType()->isIntegerTy(32));
                            assert (condition->getType()->isIntegerTy(32));
                            ICmpInst* cmp = new ICmpInst(sw,
                                                         ICmpInst::ICMP_EQ,
                                                         condition,
                                                         caseVal0,
                                                         "wfv.switch.cond");
                            WFV::setMetadata(cmp, WFV::WFV_METADATA_OP_UNIFORM);

                            sw->eraseFromParent();
                            defaultBB->eraseFromParent();
                            terminator = BranchInst::Create(target0, target1, cmp, block);
                            WFV::setMetadata(terminator, WFV::WFV_METADATA_OP_UNIFORM);
                        }
                    }
                    else
                    {
                        assert (false && "unsupported terminator found!");
                    }

                    break;
                }
                case REWIRE_MULTI:
                {
                    ++numRewireMulti;
                    //assert (numRewireMulti == 1 && "not implemented!");
                    assert (idxAlloca && reloads);

                    // This only works for uniform terminators:
                    // If the terminator was varying, and we break the edge, the new
                    // block should become the rewire target of blocks from the other
                    // side. I don't think this is easy to implement this way ->
                    // disallowing critical edges in the first place is much cleaner.
                    //assert (WFV::hasMetadata(terminator, WFV::WFV_METADATA_OP_UNIFORM));
                    // NOTE: Apparently, it *does* work... at least for our current
                    //       test suite.

                    // If the source block of this multi-rewire edge does not end with
                    // an unconditional branch, we have to introduce a new block on this
                    // edge from which edges go out to the different rewire targets.
                    // NOTE: This is equivalent to breaking critical edges beforehand.
                    BasicBlock* oldTarget = targetInfo->mSuccessor;
                    BasicBlock* ceBlock = BasicBlock::Create(*mInfo->mContext,
                                                             block->getName()+"."+oldTarget->getName()+".mrewire",
                                                             oldTarget->getParent(),
                                                             oldTarget);

                    WFV::setMetadata(ceBlock, WFV::WFV_METADATA_OPTIONAL);

                    // Change target block of current edge.
                    for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
                    {
                        BasicBlock* succBB = terminator->getSuccessor(i);
                        if (succBB != oldTarget) continue;
                        terminator->setSuccessor(i, ceBlock);
                    }

                    // In the new block, we now create the conditional branch that tests
                    // where we came from.

                    // Create reload of cluster index in current block
                    // Create switch in current block
                    //  - jump to rewire target that corresponds to cluster index
                    //  - if the edge did not require a rewire for a cluster index,
                    //    the old target block is jumped to (as default of the switch).
                    LoadInst* load = new LoadInst(idxAlloca, "reload.idx", ceBlock);
                    reloads->push_back(load);

                    const unsigned numCases = targetInfo->mNewTargets->size();
                    SwitchInst* sw = SwitchInst::Create(load, oldTarget, numCases, ceBlock);

                    WFV::setMetadata(load, WFV::WFV_METADATA_OP_UNIFORM);
                    WFV::setMetadata(sw,   WFV::WFV_METADATA_OP_UNIFORM);

                    for (unsigned i=0, e=numCases; i<e; ++i)
                    {
                        BasicBlock* target      = (*targetInfo->mNewTargets)[i];
                        BasicBlock* clusterHead = (*targetInfo->mNewTargetPreds)[i];

                        assert (indexMap.count(clusterHead));
                        const unsigned clusterIdx = indexMap[clusterHead];
                        ConstantInt* idxVal = ConstantInt::get(*mInfo->mContext, APInt(32, clusterIdx));
                        sw->addCase(idxVal, target);
                    }

                    break;
                }
                case NEW:
                {
                    ++numNew;
                    assert (numNew == 1 && "must not attempt to remove multiple edges!");
                    newExitInfo = targetInfo;
                    break;
                }
                default:
                {
                    assert (false && "unknown edge type found!");
                }
            }
        }

        // We must not remove any branch before all rewiring has been done.
        if (numRemove)
        {
            assert (removeInfo);
            assert (removeInfo->mSuccessor);
            assert (removeInfo->mNewTargets->empty());
            BasicBlock* removeTarget = removeInfo->mSuccessor;

            if (BranchInst* branch = dyn_cast<BranchInst>(terminator))
            {
                // Update mask analysis information.
                mMaskAnalysis->removeExitMask(*block, *removeTarget);

                BranchInst* newBranch = removeTarget == branch->getSuccessor(0) ?
                    BranchInst::Create(branch->getSuccessor(1), block) :
                    BranchInst::Create(branch->getSuccessor(0), block);

                WFV::setMetadata(newBranch, WFV::WFV_METADATA_OP_UNIFORM);

                branch->eraseFromParent();
                terminator = branch;
            }
            else if (SwitchInst* sw = dyn_cast<SwitchInst>(terminator))
            {
                if (sw->getDefaultDest() == removeTarget)
                {
                    BasicBlock* defaultBB = BasicBlock::Create(*mInfo->mContext,
                                                               "default",
                                                               removeTarget->getParent(),
                                                               block);
                    UnreachableInst* uinst = new UnreachableInst(*mInfo->mContext, defaultBB);
                    WFV::setMetadata(uinst,  WFV::WFV_METADATA_OP_UNIFORM);
                    sw->setDefaultDest(defaultBB);
                }
                else
                {
                    ConstantInt* caseVal = sw->findCaseDest(removeTarget);
                    SwitchInst::CaseIt it = sw->findCaseValue(caseVal);
                    sw->removeCase(it);
                }
            }
            else
            {
                assert (false && "unsupported terminator found!");
            }
        }

        if (numNew)
        {
            assert (newExitInfo);
            assert (!newExitInfo->mSuccessor);
            assert (newExitInfo->mNewTargets->size() == 1);
            const Loop* loop = mLoopInfo->getLoopFor(block);
            assert (loop && loop->getLoopLatch() == block);
            BasicBlock* exitTarget = newExitInfo->mNewTargets->front();
            createSingleVaryingLoopExitEdge(loop,
                                            block,
                                            exitTarget,
                                            memInfos,
                                            maskValueMap,
                                            maskPhiValueMap);
        }

        for (auto &targetInfo : edgeInfos)
        {
            delete targetInfo;
        }
        //delete pair.second; // edgeInfos
        delete mLinearizeInfoMap[block];
    }
}

// This method is required to replace exactly one edge
// and not change the order of the edges. This way, the
// caller can still iterate over the terminator's
// successors. Thus, it is also required to not change
// the number of successors.
TerminatorInst*
CFGLinearizer::rewireEdge(TerminatorInst* terminator,
                          BasicBlock*     oldTarget,
                          BasicBlock*     newTarget)
{
    assert (terminator && oldTarget && newTarget);
    assert (terminator->getParent());
    assert (oldTarget != newTarget);

    BasicBlock* parentBB = terminator->getParent();

    if (BranchInst* br = dyn_cast<BranchInst>(terminator))
    {
        if (br->isUnconditional())
        {
            br->eraseFromParent();
            BranchInst* newBr = BranchInst::Create(newTarget, parentBB);
            WFV::setMetadata(newBr, WFV::WFV_METADATA_OP_UNIFORM);
            return newBr;
        }
        else
        {
            const bool trueIsTarget = br->getSuccessor(0) == oldTarget;
            BranchInst* newBr =
                    BranchInst::Create(trueIsTarget ? newTarget : br->getSuccessor(0),
                                       trueIsTarget ? br->getSuccessor(1) : newTarget,
                                       br->getCondition(),
                                       parentBB);
            WFV::copyMetadata(newBr, *br);
            br->eraseFromParent();
            return newBr;
        }
    }

    if (SwitchInst* sw = dyn_cast<SwitchInst>(terminator))
    {
        bool rewired = false; WFV_UNUSED(rewired);
        // getNumCases() returns number of cases without default.
        // getSuccessor() returns blocks including default (index 0).
        for (unsigned i=0, e=sw->getNumCases()+1; i<e; ++i)
        {
            BasicBlock* caseTarget = sw->getSuccessor(i);
            if (caseTarget != oldTarget) continue;
            sw->setSuccessor(i, newTarget);
            rewired = true;
        }
        assert (rewired);
        return sw;
    }

    assert (false && "unsupported terminator found!");
    return nullptr;
}

void
CFGLinearizer::createSingleVaryingLoopExitEdge(const Loop*       loop,
                                               BasicBlock*       block,
                                               BasicBlock*       exitTarget,
                                               MemInfoMapType&   memInfos,
                                               MaskValueMapType& maskValueMap,
                                               MaskValueMapType& maskPhiValueMap)
{
    assert (loop && block && exitTarget);
    assert (WFV::hasMetadata(loop, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE));

    if(mInfo->mVerbose) outs() << "  block is latch of DIVERGENT loop, "
        "inserting all-false-exit edge...\n";

    TerminatorInst* terminator = block->getTerminator();
    assert (terminator->getNumSuccessors() == 1);

    BasicBlock* headerBB = loop->getHeader();
    Value* cond = mMaskAnalysis->getExitMask(*block, *headerBB);

    // Since we do reg2mem before linearization, this mask may not exist
    // anymore. We add an additional use to it, so the memInfo object also has
    // to be updated.
    const bool inValueMap = maskValueMap.count(cond);
    if (inValueMap || maskPhiValueMap.count(cond))
    {
        Value* maskAlloca = inValueMap ? maskValueMap[cond] : maskPhiValueMap[cond];
        assert (isa<AllocaInst>(maskAlloca));
        cond = new LoadInst(maskAlloca, "reload.mask", block->getTerminator());

        // Add this reload to the meminfo object of the mask alloca.
        bool added = false; WFV_UNUSED(added);
        for (auto &MI : memInfos)
        {
            if (MI->mAlloca != maskAlloca) continue;
            MI->mReloads->push_back(cast<LoadInst>(cond));
            added = true;
            break;
        }
        assert (added);
    }

    // Jump back to header on "true".
    BranchInst* branch = BranchInst::Create(headerBB, exitTarget, cond, block);

    WFV::setMetadata(branch, WFV::WFV_METADATA_OP_VARYING);

    // Update mask analysis information.
    // The exit condition is the combined exit mask. This is always correct
    // and includes cases where there are still uniform loop exits: Their
    // corresponding masks will always be entirely false until the exit is
    // taken (in which case this exit condition here will not be changed
    // again).
    // TODO: Can this cause problems with former users of the exit mask?
    //       Or is this exit mask only used during vectorization of the branch?
    // TODO: This seems to be wrong anyway... unless this is the combined mask
    //       of all *persisted* exit masks (i.e., it holds all instances that
    //       left the loop through a MANDATORY exit in any iteration).
    Value* loopExitCond = mMaskAnalysis->getCombinedLoopExitMask(*loop);
    mMaskAnalysis->updateExitMasks(*block,
                                   cond,
                                   loopExitCond,
                                   &*block->getFirstInsertionPt());

    if(mInfo->mVerbose) {
        outs() << "  new edge: '" << block->getName()
               << "' -> '" << exitTarget->getName() << "'\n";
    }

    terminator->eraseFromParent();
}



namespace {

// From Reg2Mem.cpp
bool
valueEscapes(const Instruction& Inst)
{
    const BasicBlock *BB = Inst.getParent();
    for (Value::const_user_iterator UI = Inst.user_begin(),
         E = Inst.user_end(); UI != E; ++UI)
    {
        const Instruction *I = cast<Instruction>(*UI);
        if (I->getParent() != BB || isa<PHINode>(I))
            return true;
    }
    return false;
}

} // unnamed namespace



void
CFGLinearizer::reg2mem(Function*         f,
                       MemInfoMapType&   memInfos,
                       MaskValueMapType& maskValueMap,
                       MaskValueMapType& maskPhiValueMap,
                       MaskValueMapType& allocaValueMap,
                       MaskBlockMapType& maskBlockMap,
                       StoreSetType&     undefStoreSet)
{
    assert (f);

    std::vector<Instruction*> instVec;
    for (auto &BB : *f)
    {
        for (auto &I : BB)
        {
            // Ignore alloca's in entry block.
            if (isa<AllocaInst>(I) && &BB == &f->getEntryBlock()) continue;
            // Ignore values that do not live out of the block.
            if (!valueEscapes(I)) continue;

            instVec.push_back(&I);
        }
    }

    Instruction* allocaPos = f->getEntryBlock().getFirstNonPHI();

    // Demote escaped instructions.
    for (auto &inst : instVec)
    {
        if(mInfo->mVerbose) {
            outs() << "\nreg2mem: " << *inst << "\n";
            outs() << "  block        ('" << inst->getParent()->getName() << "')\n";
        }

        bool hasLoopHeaderPhiUse = false;
        BasicBlock* parentBB = inst->getParent();
        Loop* parentLoop = mLoopInfo->getLoopFor(parentBB);
        if (parentLoop && !inst->getName().startswith("loopMaskUpdate"))
        {
            // Search for a use that is a phi in a loop header which has the current inst
            // as its incoming value from the latch.
            for (Value::user_iterator U=inst->user_begin(), UE=inst->user_end(); U!=UE; ++U)
            {
                if (!isa<PHINode>(*U)) continue;
                PHINode* usePhi = cast<PHINode>(*U);
                BasicBlock* useParentBB = usePhi->getParent();
                Loop* useParentLoop = mLoopInfo->getLoopFor(useParentBB);
                if (!useParentLoop || useParentLoop->getHeader() != useParentBB) continue;
                BasicBlock* latchBB = useParentLoop->getLoopLatch();
                if (usePhi->getIncomingValueForBlock(latchBB) != inst) continue;
                hasLoopHeaderPhiUse = true;
                break;
            }
        }

        AllocaInst* alloca = SafelyDemoteRegToStack(*inst, false, allocaPos);
#if 0
        if (!alloca) continue;
#endif
        WFV::copyMetadata(alloca, *inst); // Store metadata in alloca.
        if(mInfo->mVerbose) {
            outs() << "  alloca       ('" << allocaPos->getParent()->getName();
            outs() << "'): " << *alloca << "\n";
        }

        // If this is a mask, we have to update the mask analysis.
        // Similar to metadata, we use the alloca and store a value mapping.
        // This mapping is used after mem2reg.
        const bool isMask = WFV::hasMetadata(inst, WFV::WFV_METADATA_MASK);
        if (isMask)
        {
            allocaValueMap[alloca] = inst;
            maskValueMap[inst] = alloca;
            maskBlockMap[inst] = parentBB;
        }

        LoadVecType*  reloads = new LoadVecType();
        StoreVecType* stores  = new StoreVecType();
        for (Value::user_iterator U=alloca->user_begin(), UE=alloca->user_end(); U!=UE; ++U)
        {
            Instruction* useI = cast<Instruction>(*U);
            assert (isa<StoreInst>(useI) || isa<LoadInst>(useI));
            if (StoreInst* store = dyn_cast<StoreInst>(useI))
            {
                if(mInfo->mVerbose) {
                    outs() << "  new store    ('" << useI->getParent()->getName();
                    outs() << "'): " << *useI << "\n";
                }
                stores->push_back(store);
            }
            else
            {
                if(mInfo->mVerbose) {
                    outs() << "  new reload   ('" << useI->getParent()->getName();
                    outs() << "'): " << *useI << "\n";
                }
                reloads->push_back(cast<LoadInst>(useI));
            }
        }
        assert (stores->size() == 1);
        memInfos.push_back(new MemInfo(inst, alloca, reloads, stores));

        // If this value is used in an LCSSA phi of a MANDATORY exit, we have to
        // move the corresponding reload to the latch of the outermost loop that
        // is left to make sure that the updated value from the last iteration
        // is used (the result vec blend in the latch).
        for (auto &reload : *reloads)
        {
            BasicBlock* exitingBB = reload->getParent();
            Loop* reloadLoop = mLoopInfo->getLoopFor(exitingBB);
            if (!reloadLoop) continue;

            for (Value::user_iterator U=reload->user_begin(), UE=reload->user_end(); U!=UE; ++U)
            {
                if (!isa<PHINode>(*U)) continue;
                PHINode* lcssaPhi = cast<PHINode>(*U);
                BasicBlock* exitBB = lcssaPhi->getParent();
                if (WFV::isExitOfDivergentLoop(*exitBB, *mLoopInfo) &&
                    !WFV::hasMetadata(exitBB, WFV::WFV_METADATA_OPTIONAL))
                {
                    Loop* outermostLoop = WFV::getOutermostExitedLoop(*exitBB, *mLoopInfo);
                    assert (outermostLoop);
                    BasicBlock* outermostLatch = outermostLoop->getLoopLatch();
                    if (outermostLatch == exitingBB) continue;
                    reload->moveBefore(outermostLatch->getTerminator());
                    if(mInfo->mVerbose) {
                        outs() << "  moved reload ('" << outermostLatch->getName();
                        outs() << "'): " << *reload << "\n";
                    }
                    break;
                }
            }
        }

        // If there is no use as the value from the latch in a loop header phi,
        // and all defs and uses are inside some loop, move the alloca to the header.
        // Store "undef" directly after the alloca to prevent persisting the value
        // across loop iterations (or "false" in case of a mask).
        // NOTE: This actually looks like a workaround for a shortcoming of SSAUpdater
        //       that ignores where the alloca is located (thus, moving is not
        //       necessary, since it is ignored anyway).
        if (!hasLoopHeaderPhiUse)
        {
            if (const Loop* loop = getInnermostLoopForAlloca(*reloads, *stores))
            {
                Instruction* insertBefore = loop->getHeader()->getFirstNonPHI();
                Value* val = isMask ?
                    Constant::getNullValue(alloca->getAllocatedType()) :
                    UndefValue::get(alloca->getAllocatedType());
                StoreInst* lastStore = new StoreInst(val, alloca, insertBefore);
                stores->push_back(lastStore);
                undefStoreSet.insert(lastStore);
                if(mInfo->mVerbose) {
                    outs() << "  generated undef store: ";
                    outs() << loop->getHeader()->getName() << "\n";
                }
                //alloca->moveBefore(insertBefore);
                //DEBUG_WFV( outs() << "  moved alloca to header: "; );
                //DEBUG_WFV( outs() << loop->getHeader()->getName() << "\n"; );
            }
        }
    }

    // Find all phi's.
    instVec.clear();
    for (auto &BB : *f)
    {
        // Ignore loop header phis (predecessors never change).
        if (mLoopInfo->isLoopHeader(&BB)) continue;
        for (auto &I : BB)
        {
            if (!isa<PHINode>(I)) continue;
            instVec.push_back(&I);
        }
    }

    // Demote phi nodes
    for (auto &inst : instVec)
    {
        PHINode* phi = cast<PHINode>(inst);

        BasicBlock* parentBB = phi->getParent();
        Loop* parentLoop = mLoopInfo->getLoopFor(parentBB);

        bool hasLoopHeaderPhiUse = false;
        if (parentLoop)
        {
            // Search for a use that is a phi in a loop header which has the current inst
            // as its incoming value from the latch.
            for (Value::user_iterator U=phi->user_begin(), UE=phi->user_end(); U!=UE; ++U)
            {
                if (!isa<PHINode>(*U)) continue;
                PHINode* usePhi = cast<PHINode>(*U);
                BasicBlock* useParentBB = usePhi->getParent();
                Loop* useParentLoop = mLoopInfo->getLoopFor(useParentBB);
                if (!useParentLoop || useParentLoop->getHeader() != useParentBB) continue;
                BasicBlock* latchBB = useParentLoop->getLoopLatch();
                if (usePhi->getIncomingValueForBlock(latchBB) != phi) continue;
                hasLoopHeaderPhiUse = true;
                break;
            }
        }

        if(mInfo->mVerbose) {
            outs() << "\nphi2mem: " << phi << *phi << "\n";
            outs() << "  block        ('" << phi->getParent()->getName() << "')\n";
            outs() << "  addr         ('" << (void *) phi << "')\n";
        }

        const bool isMask = WFV::hasMetadata(phi, WFV::WFV_METADATA_MASK);
        Instruction* dummy = WFV::createDummy(phi->getType(), allocaPos);
        WFV::copyMetadata(dummy, *phi); // Store metadata in tmp inst.

        AllocaInst* alloca = SafelyDemotePHIToStack(phi, allocaPos);
#if 0
        if (!alloca) {
        	// work around:
        	maskPhiValueMap[phi] = nullptr;
        	maskValueMap[phi] = nullptr;
            dummy->eraseFromParent();
            continue;
        }
#endif

        WFV::copyMetadata(alloca, *dummy); // Store metadata in alloca.
        dummy->eraseFromParent();
        if(mInfo->mVerbose) {
            outs() << "  alloca       ('" << allocaPos->getParent()->getName();
            outs() << "'): " << *alloca << "\n";
        }

        if (isMask) {
            // inst is not valid anymore, but we only need the pointer values.
            // TODO: This screams for problems, use some simple identifier instead.
            // NOTE: The entry in maskBlockMap may already exist (added during inst demotion),
            //       but that does not hurt. It would hurt, though, if we did not add it ;).
            allocaValueMap[alloca] = phi;
            maskPhiValueMap[phi] = alloca;
            maskBlockMap[phi] = parentBB;
        }

        LoadVecType*  reloads = new LoadVecType();
        StoreVecType* stores  = new StoreVecType();
        for (Value::user_iterator U=alloca->user_begin(), UE=alloca->user_end(); U!=UE; ++U)
        {
            Instruction* useI = cast<Instruction>(*U);
            assert (isa<StoreInst>(useI) || isa<LoadInst>(useI));
            if (StoreInst* store = dyn_cast<StoreInst>(useI))
            {
                if(mInfo->mVerbose) {
                    outs() << "  phi store    ('" << useI->getParent()->getName();
                    outs() << "'): " << *useI << "\n";
                }
                stores->push_back(store);
            }
            else
            {
                if(mInfo->mVerbose) {
                    outs() << "  phi reload   ('" << useI->getParent()->getName();
                    outs() << "'): " << *useI << "\n";
                }
                reloads->push_back(cast<LoadInst>(useI));
            }
        }
        assert (reloads->size() == 1);
        memInfos.push_back(new MemInfo(nullptr, alloca, reloads, stores));

        // If this is an LCSSA phi of a MANDATORY exit, we have to move its
        // store from the exiting block to the latch to make sure that the
        // updated value from the last iteration is used (the result vec
        // blend in the latch).
        if (WFV::isExitOfDivergentLoop(*parentBB, *mLoopInfo) &&
            !WFV::hasMetadata(parentBB, WFV::WFV_METADATA_OPTIONAL))
        {
            BasicBlock* predBB = parentBB->getUniquePredecessor();
            assert (predBB);
            Loop* predLoop = WFV::getOutermostExitedLoop(*parentBB, *mLoopInfo);
            assert (predLoop);
            BasicBlock* predLatch = predLoop->getLoopLatch();
            if (predBB != predLatch)
            {
                assert (stores->size() == 1);
                StoreInst* store = stores->front();
                store->moveBefore(predLatch->getTerminator());
                if(mInfo->mVerbose) {
                    outs() << "  moved store  ('" << predLatch->getName();
                    outs() << "'): " << *store << "\n";
                }
            }
        }

        if (!hasLoopHeaderPhiUse)
        {
            if (const Loop* loop = getInnermostLoopForAlloca(*reloads, *stores))
            {
                Instruction* insertBefore = loop->getHeader()->getFirstNonPHI();
                Value* val = isMask ?
                    Constant::getNullValue(alloca->getAllocatedType()) :
                    UndefValue::get(alloca->getAllocatedType());
                StoreInst* lastStore = new StoreInst(val, alloca, insertBefore);
                stores->push_back(lastStore);
                undefStoreSet.insert(lastStore);
                if(mInfo->mVerbose) {
                    outs() << "  generated undef store: ";
                    outs() << loop->getHeader()->getName() << "\n";
                }
                //alloca->moveBefore(insertBefore);
                //DEBUG_WFV( outs() << "  moved alloca to header: "; );
                //DEBUG_WFV( outs() << loop->getHeader()->getName() << "\n"; );
            }
        }
    }
}

void
CFGLinearizer::repairOverlappingPaths(MemInfoMapType&   memInfos,
                                      MaskValueMapType& maskValueMap,
                                      MaskValueMapType& maskPhiValueMap,
                                      MaskValueMapType& allocaValueMap,
                                      MaskBlockMapType& maskBlockMap,
                                      StoreSetType&     undefStoreSet)
{
    // Repair overlapping paths:
    // If some of the original incoming edges do not exist anymore,
    // we have to introduce a select operation for each removed one.
    // To make sure that the incoming values of these selects are also properly
    // dominated, we rewire each store whose block is not a direct predecessor
    // of the phi's parent block anymore to its own alloca.
    // We then have to find out which store succeeds another one. At each of
    // these positions, we reload the preceeding store's alloca, blend the
    // values, and store to the succeeding store's alloca.
    // At the "lowest" store of such a path (in the direct predecessor of the
    // phi's parent block), we store to the phi's alloca directly.
    //
    // Note that we also have to create reloads of the required masks' alloca's.
    // During all this, we possibly have to create additional alloca's etc.,
    // which are added to the memInfos vector afterwards.
    SmallVector<MemInfo*, 8> newMemInfos;
    for (auto &MI : memInfos)
    {
        if (!MI->mTargetIsPHI) continue;

        const StoreVecType& stores  = *MI->mStores;
        const LoadVecType&  reloads = *MI->mReloads;
        assert (reloads.size() == 1);
        LoadInst* reload = reloads.front();
        BasicBlock* parentBB = reload->getParent();

        // Ignore loop header phis (can never have overlapping paths).
        if (mLoopInfo->isLoopHeader(parentBB)) continue;

        // Ignore phis in OPTIONAL blocks (overlapping paths are okay if there
        // is a critical edge on the neighboring path).
        // TODO: HERE! This results in no fixup select ever getting introduced (in unit tests)!!
        if (WFV::hasMetadata(parentBB, WFV::WFV_METADATA_OPTIONAL)) continue;

        if(mInfo->mVerbose) {
            outs() << "\nrepair overlapping paths to phi reload: " << *reload << "\n";
            outs() << "  block        ('" << parentBB->getName() << "')\n";
        }

        if(mInfo->mVerbose) {
            std::set < BasicBlock * > predBlocks;
            for (pred_iterator P = pred_begin(parentBB); P != pred_end(parentBB); ++P) {
                predBlocks.insert(*P);
            }
            outs() << "  pred blocks:\n";
            for (const auto &BB : predBlocks) {
                outs() << "   * " << BB->getName() << "\n";
            }
            outs() << "  stores:\n";
            for (const auto &st : stores) {
                outs() << "   * " << st->getParent()->getName() << ": " << *st << "\n";
            }

            // Sanity check: We expect each store of a phi to be in a different block.
            std::set<const BasicBlock *> tmpSet;
            for (const auto &store : stores) {
                if (undefStoreSet.count(store)) continue;
                const BasicBlock *parentBB = store->getParent();
                assert(!tmpSet.count(parentBB) && "expected each store in a different block!");
                tmpSet.insert(parentBB);
            }
        }

        // First, we have to find out which stores are not reachable from others
        // and which ones are overwritten by which others.
        OverwriteMapType overwriteMap;
        findOverwritingStores(stores, undefStoreSet, overwriteMap, parentBB);

        if (overwriteMap.empty())
        {
            if(mInfo->mVerbose) outs() << "  no stores are overwritten.\n";
            continue;
        }

        // Now, create a new alloca for all stores that are overwritten by the same other store.
        // Then, create a reload of that alloca, followed by a select with the mask of this
        // block, the value of the overwriting store, and the incoming mask of this block,
        // and the reload.
        AllocaInst* oldAlloca = MI->mAlloca;
        // TODO: HERE! use oldAlloca?
        Instruction* allocaPos =
            oldAlloca->getParent()->getParent()->getEntryBlock().getFirstNonPHI();
        // TODO: HERE! This never gets used?!?!
        for (auto &pair : overwriteMap)
        {
            StoreSetType& directPreds = *pair.second;

            const bool overwritesOthers = !directPreds.empty();
            if (!overwritesOthers) continue;

            StoreInst* store = pair.first;

            if(mInfo->mVerbose) {
                outs() << "  overwrite    ('" << store->getParent()->getName();
                outs() << "'): " << *store << "\n";
            }

            LoadVecType*  newReloads = new LoadVecType();
            StoreVecType* newStores  = new StoreVecType();

            AllocaInst* newAlloca = new AllocaInst(oldAlloca->getAllocatedType(), 0,
                                                   nullptr,
                                                   "alloca.tmp",
                                                   allocaPos);
            WFV::copyMetadata(newAlloca, *oldAlloca); // Retain metadata.
            if(mInfo->mVerbose) {
                outs() << "  new alloca   ('" << newAlloca->getParent()->getName();
                outs() << "'): " << *newAlloca << "\n";
            }

            const bool isNewMask = WFV::hasMetadata(newAlloca, WFV::WFV_METADATA_MASK);

            // Create new store to new location for each overwritten store.
            // If the old one is overwritten on all paths, it does not have an
            // effect. If it is not overwritten on some path, not removing this
            // store ensures that the value is still available there.
            for (auto ovwStore : directPreds)
            {
                StoreInst* newOvwStore = new StoreInst(ovwStore->getValueOperand(),
                                                       newAlloca,
                                                       ovwStore);

                if(mInfo->mVerbose) {
                    outs() << "  new store    ('" << newOvwStore->getParent()->getName();
                    outs() << "'): " << *newOvwStore << "\n";
                }

                // Save the store in the new meminfo object.
                newStores->push_back(newOvwStore);
            }

            // Reload from this location.
            LoadInst* newReload = new LoadInst(newAlloca, "reload.tmp", store);
            newReloads->push_back(newReload);
            if(mInfo->mVerbose) {
                outs() << "  new reload   ('" << newReload->getParent()->getName();
                outs() << "'): " << *newReload << "\n";
            }

            // Get the mask of the corresponding (now non-existant) edge.
            BasicBlock* storeBB = store->getParent();
            Value* mask = mMaskAnalysis->getExitMask(*storeBB, *parentBB);
            if (Instruction* maskI = dyn_cast<Instruction>(mask))
            {
                const bool inValueMap = maskValueMap.count(maskI);
                if (inValueMap || maskPhiValueMap.count(maskI))
                {
                    Value* maskAlloca = inValueMap ? maskValueMap[maskI] : maskPhiValueMap[maskI];
                    assert (isa<AllocaInst>(maskAlloca));
                    mask = new LoadInst(maskAlloca, "reload.mask", store);

                    // Add this reload to the meminfo object of the mask alloca.
                    bool added = false; WFV_UNUSED(added);
                    for (auto &MI : memInfos)
                    {
                        if (MI->mAlloca != maskAlloca) continue;
                        MI->mReloads->push_back(cast<LoadInst>(mask));
                        added = true;
                        break;
                    }
                    assert (added);
                }
            }

            // Blend the overwriting store's value with the reload.
            SelectInst* select = SelectInst::Create(mask,
                                                    store->getValueOperand(),
                                                    newReload,
                                                    "ssa.repair.phi",
                                                    store);
            WFV::setMetadata(select, WFV::WFV_METADATA_RES_VECTOR);
            WFV::setMetadata(select, WFV::WFV_METADATA_OP_VARYING);
            WFV::setMetadata(select, WFV::WFV_METADATA_INDEX_RANDOM);
            WFV::setMetadata(select, WFV::WFV_METADATA_ALIGNED_FALSE);
            WFV::setMetadataForBlend(select, storeBB, nullptr, true /* isLast */);

            if(mInfo->mVerbose) {
                outs() << "  new select   ('" << select->getParent()->getName();
                outs() << "'): " << *select << "\n";
            }

            if (isNewMask)
            {
                WFV::setMetadata(select, WFV::WFV_METADATA_MASK);
                allocaValueMap[newAlloca] = select;
                maskValueMap[select] = newAlloca;
                maskBlockMap[select] = select->getParent();
            }

            // Finally, update the stored value of the current store.
            assert (store->getValueOperand() == store->getOperand(0));
            store->setOperand(0, select);

            // Delay the push_back into 'memInfos' to not confuse the iteration.
            newMemInfos.push_back(new MemInfo(select, newAlloca, newReloads, newStores));
        }
    }

    for (auto &MI : newMemInfos)
    {
        memInfos.push_back(MI);
    }
}

void
CFGLinearizer::findOverwritingStores(const StoreVecType& stores,
                                     const StoreSetType& undefStoreSet,
                                     OverwriteMapType&   overwriteMap,
                                     const BasicBlock*   doNotTraverse) const
{
    for (const auto &store : stores)
    {
        BasicBlock* storeBB = store->getParent();

        // Ignore undef stores (they are always overwritten by construction).
        if (undefStoreSet.count(store)) continue;

        Loop* ignoreLoop = mLoopInfo->getLoopFor(storeBB);

        // Collect all stores that the current one overwrites (except for undef stores).
        StoreSetType overwrittenStores;
        for (auto &other : stores)
        {
            if (other == store) continue;
            if (undefStoreSet.count(other)) continue;
            BasicBlock* otherBB = other->getParent();
            assert (ignoreLoop == mLoopInfo->getLoopFor(otherBB));
            if (!WFV::isReachable(storeBB, otherBB, doNotTraverse, ignoreLoop, ignoreLoop)) continue;
            overwrittenStores.insert(other);
        }

        if (overwrittenStores.empty()) continue;

        // Exclude those that are overwritten already by a different one.
        StoreSetType* directPreds = new StoreSetType();
        for (auto ov0 : overwrittenStores)
        {
            BasicBlock* bb0 = ov0->getParent();
            bool isOverwrittenByOther = false;
            for (auto ov1 : overwrittenStores)
            {
                if (ov0 == ov1) continue;
                BasicBlock* bb1 = ov1->getParent();
                if (!WFV::isReachable(bb1, bb0, doNotTraverse, ignoreLoop, ignoreLoop)) continue;
                isOverwrittenByOther = true;
                break;
            }
            if (isOverwrittenByOther) continue;
            directPreds->insert(ov0);
        }
        overwriteMap[store] = directPreds;
    }
}

const Loop*
CFGLinearizer::getInnermostLoopForAlloca(const LoadVecType&  reloads,
                                         const StoreVecType& stores) const
{
    assert (!stores.empty());

    // Find the innermost common loop of all defs.
    const Loop* commonLoop = nullptr;
    for (const auto &store : stores)
    {
        const BasicBlock* defBB = store->getParent();
        const Loop* loop = mLoopInfo->getLoopFor(defBB);

        // If the def is not in a loop, we can return immediately because this
        // means that the alloca has to be in the entry block of the function
        // (which it already is).
        if (!loop) return nullptr;

        if (!commonLoop)
        {
            commonLoop = loop;
            continue;
        }

        assert (loop->contains(commonLoop) || commonLoop->contains(loop));

        if (loop->contains(commonLoop)) commonLoop = loop;
        assert (loop->contains(defBB) && commonLoop->contains(defBB));
    }

    assert (commonLoop);

    // Find the outermost loop of all uses.
    for (const auto &reload : reloads)
    {
        const BasicBlock* useBB = reload->getParent();
        const Loop* loop = mLoopInfo->getLoopFor(useBB);

        // If the use is not in a loop, we can return immediately because this
        // means that the alloca has to be in the entry block of the function
        // (which it already is).
        if (!loop) return nullptr;

        assert (loop->contains(commonLoop) || commonLoop->contains(loop));

        if (loop->contains(commonLoop)) commonLoop = loop;
    }

    return commonLoop;
}

void
CFGLinearizer::mem2reg(Function*              f,
                       std::vector<MemInfo*>& memInfos,
                       MaskValueMapType&      maskValueMap,
                       MaskValueMapType&      maskPhiValueMap,
                       MaskValueMapType&      allocaValueMap,
                       MaskBlockMapType&      maskBlockMap)
{
    assert (f);

    // SROA/mem2reg phases do not allow setting metadata to generated phis.
    // LASP
    SmallVector<PHINode*, 4> insertedPhis;
    SSAUpdater ssaUpdater(&insertedPhis);

    SmallVector<Instruction*, 2> insts;
    for (auto &MI : memInfos)
    {
        insertedPhis.clear();
        insts.clear();

        AllocaInst* alloca = MI->mAlloca;

        if(mInfo->mVerbose) {
            outs() << "\nmem2reg: " << *alloca << "\n";
            outs() << "  block   ('" << alloca->getParent()->getName() << "')\n";
        }

        for (auto &store : *MI->mStores)
        {
            insts.push_back(store);
            if(mInfo->mVerbose) outs() << "  store   ('" << store->getParent()->getName() << "'): " << *store << "\n";
        }
        for (auto &reload : *MI->mReloads)
        {
            insts.push_back(reload);
            if(mInfo->mVerbose) outs() << "  reload  ('" << reload->getParent()->getName() << "'): " << *reload << "\n";
        }

        // Run promoter.
        // NOTE: This only works correctly if all phis have incoming values
        //       that correspond to the block's actual incoming edges!
        //       This is because SSAUpdater has code that prefers to look
        //       at the incoming blocks of other phis instead of iterating
        //       over the actual predecessors via pred_begin()/end().
        assert (verifyIncomingEdges(*f));
        LoadAndStorePromoter LASP(insts, ssaUpdater);
        LASP.run(insts);

        // If this alloca originally was a mask, update the mask value map
        // with the new value.
        // NOTE: We must never directly access "origMask" since it may have been
        //       erased. We must only use its pointer address to access the
        //       mapped values in maskValueMap and maskBlockMap.
        if (WFV::hasMetadata(alloca, WFV::WFV_METADATA_MASK))
        {
            assert (allocaValueMap.count(alloca));
            Value* origMask = allocaValueMap[alloca];

            const bool isMaskPhiAlloca = maskPhiValueMap.count(origMask) &&
                maskPhiValueMap[origMask] == alloca;
            MaskValueMapType& maskMap = isMaskPhiAlloca ?
                maskPhiValueMap : maskValueMap;
            assert (maskMap.count(origMask));

            if (insertedPhis.empty())
            {
                // Nothing changed, so the mask graph is still valid.
                maskMap.erase(origMask);
            }
            else
            {
                // Get the value in the target block and set it as the new mask.
                assert (maskMap.count(origMask));
                assert (maskMap[origMask] == alloca);
                assert (maskBlockMap.count(origMask));
                BasicBlock* maskBlock = maskBlockMap[origMask];
                // TODO: Why can it happen that we insert additional phis here?
                // NOTE: In some cases, this produces dead code, e.g. for unit tests
                //       Test063LoopNestedMultiExit07, Test065LoopNestedMultiExit09.
                Value* newMask = ssaUpdater.GetValueInMiddleOfBlock(maskBlock);
                assert (newMask);
                maskMap[origMask] = newMask;
            }
        }

        for (auto &phi : insertedPhis)
        {
            // The alloca temporarily stored the relevant metadata.
            WFV::copyMetadata(phi, *alloca);
            // Phi functions are always OP_UNIFORM unless they have RES_SCALARS operands.
            // Since we copied from a normal instruction, that metadata may be wrong.
            // TODO: There may be more things to consider here...
            bool hasResScalarsOp = false;
            for (auto O=phi->op_begin(), OE=phi->op_end(); O!=OE; ++O)
            {
                if (!isa<Argument>(*O) && !isa<Instruction>(*O)) continue;
                if (!WFV::hasMetadata(*O, WFV::WFV_METADATA_RES_SCALARS)) continue;
                hasResScalarsOp = true;
                break;
            }
            WFV::setMetadata(phi,
                             hasResScalarsOp ?
                                 WFV::WFV_METADATA_OP_SEQUENTIAL :
                                 WFV::WFV_METADATA_OP_UNIFORM);
            if(mInfo->mVerbose) {
                outs() << "  new phi ('" << phi->getParent()->getName();
                outs() << "'): " << *phi << "\n";
            }
        }

        // If this is a new mask phi, incoming values that are "undef" have to
        // be changed to "false". This maintains correct behavior when rewiring
        // edges of uniform control flow (e.g. in test_fastwalshtransform of
        // test suite 3).
        for (auto &phi : insertedPhis)
        {
            if (!WFV::hasMetadata(phi, WFV::WFV_METADATA_MASK)) continue;
            for (unsigned i=0, e=phi->getNumIncomingValues(); i<e; ++i)
            {
                Value* incVal = phi->getIncomingValue(i);
                if (!isa<UndefValue>(incVal)) continue;
                phi->setIncomingValue(i, ConstantInt::getFalse(phi->getContext()));
            }
        }

        assert (alloca->user_empty());
        alloca->eraseFromParent();
    }

    for (auto &MI : memInfos)
    {
        delete MI;
    }
}
