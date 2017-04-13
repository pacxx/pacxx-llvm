/**
 * @file   maskAnalysis.cpp
 * @date   07.06.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */
#include "wfv/analysis/maskAnalysis.h"
#include "wfv/utils/maskGraphUtils.h"
#include "wfv/wfvConfig.h"
#include "wfv/utils/wfvTools.h"
#include "wfv/utils/metadata.h"

#include "llvm/IR/Instructions.h"
#include "llvm/Analysis/CFG.h" // pred_begin() etc.

#include <stdexcept>
#include "wfv/utils/stringUtils.h"

using namespace llvm;
using namespace WFV::MaskGraphUtils;

char MaskAnalysis::ID = 0;
// NOTE: The order of initialized dependencies is important
//       to prevent 'Unable to schedule' errors!
INITIALIZE_PASS_BEGIN(MaskAnalysis, "maskAnalysis", "MaskAnalysis", false, true)
INITIALIZE_PASS_DEPENDENCY(WFVInfo)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_END(MaskAnalysis, "maskAnalysis", "MaskAnalysis", false, true)

// Public interface to the MaskAnalysis pass
FunctionPass*
llvm::createMaskAnalysisPass()
{
	return new MaskAnalysis();
}



MaskAnalysis::MaskAnalysis()
    : FunctionPass(ID), mInfo(nullptr)
{
    initializeMaskAnalysisPass(*PassRegistry::getPassRegistry());
}

MaskAnalysis::~MaskAnalysis()
{
    mInfo = nullptr; // Deleted by PassManager.
    for (auto &it : mBlockMap)
    {
        BlockMaskInfo* bi = it.second;
        delete bi;
    }
    for (auto &it : mLoopExitMap)
    {
        LoopExitMaskInfo* li = it.second;
        delete li;
    }
    for (auto &mask : mMasks)
    {
        mask.reset();
    }
}

void
MaskAnalysis::releaseMemory()
{
}

void
MaskAnalysis::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<WFVInfo>();
    AU.addRequired<LoopInfoWrapperPass>();

    AU.setPreservesAll();
}

bool
MaskAnalysis::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
MaskAnalysis::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
MaskAnalysis::runOnFunction(Function& F)
{
    mInfo     = &getAnalysis<WFVInfo>();
    mLoopInfo = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

    if(mInfo->mVerbose) {
        outs() << "\n#########################################################\n";
        outs() << "## MASK ANALYSIS\n";
        outs() << "#########################################################\n";
    }

    // If an error occurred in one of the previous phases, abort.
    assert (mInfo->mFailure);
    if (*mInfo->mFailure) return false;

    createMaskGraph(&F);

    if(mInfo->mVerbose) print(outs(), NULL);

    return false;
}

void
MaskAnalysis::print(raw_ostream& O, const Module* M) const
{
    O << "Masks:\n";
    for (auto &it : mMasks)
    {
        it->print(O);
        //O << " " << it->mInsertPoint->getParent()->getName();
        O << "\n";
    }

    O << "\nBlock Mask Info:\n";
    for (auto &it : mBlockMap)
    {
        BlockMaskInfo* bi = it.second;
        bi->print(O);
    }

    O << "\nLoop Mask Info:\n";
    for (auto &it : mLoopMaskMap)
    {
        LoopMaskInfo* li = it.second;
        li->print(O);
    }

    O << "\nLoop Exit Mask Info:\n";
    for (auto &it : mLoopExitMap)
    {
        LoopExitMaskInfo* li = it.second;
        li->print(O);
    }

    O << "\n";
}


MaskPtr
MaskAnalysis::createMask(const NodeType type,
                         Instruction*   insertPoint)
{
    assert (insertPoint);

    // Create shared pointer.
    MaskPtr mask = std::make_shared<Mask>(type, insertPoint);

    // Store shared pointer to prevent early deletion.
    mMasks.push_back(mask);

    return mask;
}

void
MaskAnalysis::createMaskGraph(Function* f)
{
    assert (f);

    // Initialize graph starting from return nodes.
    SmallPtrSet<BasicBlock*, 2> returnBlocks;
    WFV::findReturnBlocks(*f, returnBlocks);

    DenseSet<BasicBlock*> markedBlocks;
    for (auto BB : returnBlocks)
    {
        recCreateMaskGraph(BB, markedBlocks);
    }

    // We have to be sure to create loop exit masks for every nested divergent
    // loop. Therefore, we iterate over all those loops that are divergent
    // and "top-level", meaning they are not nested or nested directly
    // inside a non-divergent loop.
    // EXAMPLE: divergent loop inside non-divergent loop.
    // EXAMPLE: divergent loop inside non-divergent loop inside divergent loop.
    for (auto loop : *mLoopInfo)
    {
        createLoopExitMasks(loop);
    }
}

void
MaskAnalysis::recCreateMaskGraph(BasicBlock*            block,
                                 DenseSet<BasicBlock*>& markedBlocks)
{
    assert (block);

    // If we have marked this block already, ignore it.
    if (markedBlocks.find(block) != markedBlocks.end()) return;

    const bool  isLoopHeader = mLoopInfo->isLoopHeader(block);
    const Loop* loop         = mLoopInfo->getLoopFor(block);
    assert (!isLoopHeader || loop);

    // Generate mask information for all predecessors (bottom-up post reversed order).
    // If we are in a loop header, we first have to recurse into the preheader, then
    // create the mask info, then recurse into the latch (see end of this function).
    if (isLoopHeader)
    {
        BasicBlock* preheaderBB = loop->getLoopPreheader();
        recCreateMaskGraph(preheaderBB, markedBlocks);
    }
    else
    {
        for (pred_iterator PI=pred_begin(block),
                PE=pred_end(block); PI!=PE; ++PI)
        {
            BasicBlock* predBB = *PI;
            recCreateMaskGraph(predBB, markedBlocks);
        }
    }

    if(mInfo->mVerbose) outs() << "\ngenerating mask information for block '" << block->getName() << "'... \n";

    // Create entry mask.
    MaskPtr entryMask = createEntryMask(block);

    // Create exit mask(s).
    SmallVector<MaskPtr, 2> exitMasks;
    createExitMasks(block, entryMask, exitMasks);

    // Store information in new graph node.
    BlockMaskInfo* info = new BlockMaskInfo();
    info->mBlock        = block;
    info->mEntryMask    = entryMask;
    for (auto M : exitMasks)
    {
        info->mExitMasks.push_back(M);
    }

    // Insert node into mask graph.
    mBlockMap[block] = info;

    // Mark block as finished.
    markedBlocks.insert(block);

    if(mInfo->mVerbose) {
        outs() << "generated mask information for block '" << block->getName() << "':\n";
        info->print(outs());
    }


    if (!isLoopHeader) return;

    // Recurse into latch.
    BasicBlock* latchBB = loop->getLoopLatch();
    recCreateMaskGraph(latchBB, markedBlocks);

    // If this was no divergent loop, return.
    if (WFV::hasMetadata(loop, WFV::WFV_METADATA_LOOP_DIVERGENT_FALSE)) return;

    // Otherwise, we can now set the incoming value of the loop mask phi
    // from direction of the latch.
    assert (entryMask->mType == LOOPMASKPHI);
    MaskPtr latchMask = getExitMaskPtr(*latchBB, *loop->getHeader());
    entryMask->mOperands.push_back(latchMask);
    entryMask->mIncomingDirs.push_back(latchBB);

    if(mInfo->mVerbose) {
        outs() << "  updated loop mask phi in loop header '" << block->getName() << "': ";
        entryMask->print(outs());
        outs() << "\n";
    }
}

MaskPtr
MaskAnalysis::createEntryMask(BasicBlock* block)
{
    assert (block);

    const bool  isLoopHeader = mLoopInfo->isLoopHeader(block);
    const Loop* loop         = mLoopInfo->getLoopFor(block);
    assert (!isLoopHeader || loop);

    Instruction* insertPoint = &*block->getFirstInsertionPt();

    if (block == &block->getParent()->getEntryBlock())
    {
        if (mInfo->mMaskPosition != -1)
        {
            Function::arg_iterator A = block->getParent()->arg_begin();
            std::advance(A, mInfo->mMaskPosition);
            MaskPtr entryMask = createMask(VALUE, insertPoint);
            entryMask->mValue = &*A;
            return entryMask;
        }

        assert ((mInfo->mDisableControlFlowDivAnalysis ||
                 WFV::hasMetadata(block, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE)) &&
                "entry block of function without mask argument must be ALWAYS_BY_ALL_TRUE");
        MaskPtr entryMask = createMask(CONSTANT, insertPoint);
        entryMask->mValue = mInfo->mConstBoolTrue;

        if(mInfo->mVerbose) {
            outs() << "  entryMask (1): ";
            entryMask->print(outs());
            outs() << "\n";
        }
        return entryMask;
    }

    // NOTE: this is only correct for ABAON blocks if we are sure that they
    //       *never* are executed with anything else than a full mask!
    if (WFV::hasMetadata(block, WFV::WFV_METADATA_ALWAYS_BY_ALL_TRUE) ||
        WFV::hasMetadata(block, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE))
    {
        // If block is executed, *all* instances are active,
        // so the mask is always entirely true.
        assert (mInfo->mMaskPosition == -1 &&
                "function has mask argument, must not have ALWAYS_BY_ALL blocks!");
        MaskPtr entryMask = createMask(CONSTANT, insertPoint);
        entryMask->mValue = mInfo->mConstBoolTrue;

        if(mInfo->mVerbose) {
            outs() << "  entryMask (2): ";
            entryMask->print(outs());
            outs() << "\n";
        }
        return entryMask;
    }

    // If a block has a single predecessor, its mask is equal to the one
    // of its predecessor block.
    if (BasicBlock* predBB = block->getUniquePredecessor())
    {
        MaskPtr entryMask = createMask(REFERENCE, insertPoint);
        entryMask->mIncomingDirs.push_back(predBB);
        entryMask->mIncomingDirs.push_back(block);

        if(mInfo->mVerbose) {
            outs() << "  entryMask (3): ";
            entryMask->print(outs());
            outs() << "\n";
        }
        return entryMask;
    }

    if (isLoopHeader)
    {
        BasicBlock* preheaderBB = loop->getLoopPreheader();
        MaskPtr preheaderOp = getExitMaskPtr(*preheaderBB, *block);

        if (WFV::hasMetadata(loop, WFV::WFV_METADATA_LOOP_DIVERGENT_FALSE))
        {
            // This is the header of a NON_DIVERGENT loop, so we have to make sure
            // that the entry mask is the exit mask of the preheader in all
            // iterations (no loop mask phi).
            MaskPtr entryMask = createMask(REFERENCE, insertPoint);
            entryMask->mIncomingDirs.push_back(preheaderBB);
            entryMask->mIncomingDirs.push_back(block);

            if(mInfo->mVerbose) {
                outs() << "  entryMask (4): ";
                entryMask->print(outs());
                outs() << "\n";
            }
            return entryMask;
        }
        else
        {
            // This is the header of a DIVERGENT loop, so we have to make sure
            // that there is a loop mask phi.
            // Build a phi for the loop-condition (incoming value from
            // preheader and latch).
            // Due to LoopSimplify, we can rely on the block to have
            // exactly these two incoming edges.
            // NOTE: Mask materialization (MaskGenerator.cpp) relies on
            //       the preheader mask being the first operand).
            // NOTE: The update operation is created and added as operand
            //       later during recursion.
            MaskPtr entryMask = createMask(LOOPMASKPHI, block->getFirstNonPHI());
            entryMask->mOperands.push_back(preheaderOp);
            entryMask->mIncomingDirs.push_back(preheaderBB);

            // Store loop mask phi.
            LoopMaskInfo* loopInfo = new LoopMaskInfo();
            loopInfo->mLoop    = loop;
            loopInfo->mMaskPhi = entryMask;
            mLoopMaskMap[loop] = loopInfo;

            if(mInfo->mVerbose) {
                outs() << "  entryMask (5): ";
                entryMask->print(outs());
                outs() << "\n";
            }
            return entryMask;
        }
    }

    // If the block is DIVERGENT_FALSE, its mask is equal to the one of the
    // executed predecessor block. This means we have to introduce a mask phi
    // if the block has multiple predecessors.
    // NOTE: OPTIONAL is not the property to be tested here: Consider a diamond-
    //       shaped CFG with a uniform branch. Both the start and end block will
    //       be MANDATORY but do not require to transform phis into selects.
    //       The "direct successor" criterion for MANDATORY blocks also produces
    //       situations where a phi should remain although the block is a direct
    //       successor of two varying branches (if the "connecting" branch above
    //       is uniform).
    if (WFV::hasMetadata(block, WFV::WFV_METADATA_DIVERGENT_FALSE))
    {
        assert (pred_begin(block) != pred_end(block) &&
                "optional block must have predecessors!");

        MaskPtr entryMask = createMask(PHI, block->getFirstNonPHI());

        for (pred_iterator P=pred_begin(block), PE=pred_end(block); P!=PE; ++P)
        {
            BasicBlock* predBB = *P;
            MaskPtr mask = getExitMaskPtr(*predBB, *block);
            entryMask->mOperands.push_back(mask);
            entryMask->mIncomingDirs.push_back(predBB);
        }

        if(mInfo->mVerbose) {
            outs() << "  entryMask (6): ";
            entryMask->print(outs());
            outs() << "\n";
        }
        return entryMask;
    }

    assert (WFV::hasMetadata(block, WFV::WFV_METADATA_DIVERGENT_TRUE));
    assert (WFV::hasMetadata(block, WFV::WFV_METADATA_MANDATORY));
    assert (!block->getUniquePredecessor());
    assert (WFV::getNumIncomingEdges(*block) > 1);

    // The mask is the disjunction of the exit masks of all predecessors.
    // NOTE: We might have more than two incoming edges if there
    //       are no phis in the block!

    MaskPtr entryMask = createMask(DISJUNCTION, insertPoint);

    for (pred_iterator P=pred_begin(block),
            PE=pred_end(block); P!=PE; ++P)
    {
        const BasicBlock& predBB = **P;

        MaskPtr predOp = getExitMaskPtr(predBB, *block);
        entryMask->mOperands.push_back(predOp);
    }

    if(mInfo->mVerbose) {
        outs() << "  entryMask (7): ";
        entryMask->print(outs());
        outs() << "\n";
    }

    return entryMask;
}

void
MaskAnalysis::createExitMasks(BasicBlock*              block,
                              MaskPtr                  entryMask,
                              SmallVector<MaskPtr, 2>& exitMasks)
{
    assert (block);

    if(mInfo->mVerbose) outs() << "  createExitMasks(" << block->getName() << ")\n";

    TerminatorInst* terminator  = block->getTerminator();
    Instruction*    insertPoint = block->getTerminator();

    if (terminator->getNumSuccessors() == 0)
    {
        return;
    }

    if (terminator->getNumSuccessors() == 1)
    {
        // Create a mask reference to the entry mask of the same block.
        MaskPtr exitMask = entryMask;

        // Add single exit mask to output vector and return.
        exitMasks.push_back(entryMask);
        if(mInfo->mVerbose) {
            outs() << "  exitMask  (1): ";
            exitMask->print(outs());
            outs() << "\n";
        }
        return;
    }

    if (WFV::hasMetadata(terminator, WFV::WFV_METADATA_RES_UNIFORM))
    {
        // For every outgoing edge, create a mask select with the entry mask of
        // the same block and an all-false-mask, selected by the comparison
        // result.
        for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
        {
            BasicBlock* succBB = terminator->getSuccessor(i);

            MaskPtr condition = nullptr;
            MaskPtr trueMask  = nullptr;
            MaskPtr falseMask = nullptr;

            if (const BranchInst* brInst = dyn_cast<BranchInst>(terminator))
            {
                condition = createMask(VALUE, insertPoint);
                condition->mValue = brInst->getCondition();
            }
            else if (SwitchInst* switchInst = dyn_cast<SwitchInst>(terminator))
            {
                condition = createMask(VALUE, insertPoint);

                ConstantInt*       caseDest = switchInst->findCaseDest(succBB);
                SwitchInst::CaseIt caseIt   = switchInst->findCaseValue(caseDest);

                assert (isa<ConstantInt>(caseIt->getCaseValue()) &&
                    "handling of non-constant int values in switch not implemented!");

                ConstantInt* caseConst = cast<ConstantInt>(caseIt->getCaseValue());
                Value*       cond      = switchInst->getCondition();

                // Create comparison
                ICmpInst* cmp = new ICmpInst(insertPoint,
                                             ICmpInst::ICMP_EQ,
                                             cond,
                                             caseConst,
                                             "switchcmp"+str<int>(i));

                condition->mValue = cmp;
            }
            else
            {
                assert (false && "unsupported terminator instruction found!");
                return;
            }

            trueMask = entryMask;

            falseMask = createMask(VALUE, insertPoint);
            falseMask->mValue = mInfo->mConstBoolFalse;

            const bool flipMasks = isa<BranchInst>(terminator) && i != 0;
            if (flipMasks)
            {
                trueMask = falseMask;
                falseMask = entryMask;
            }

            if(mInfo->mVerbose) {

                outs() << "  condition  (2): ";
                condition->print(outs());
                outs() << "\n";
                outs() << "  true mask  (2): ";
                trueMask->print(outs());
                outs() << "\n";
                outs() << "  false mask (2): ";
                falseMask->print(outs());
                outs() << "\n";
            }

            MaskPtr exitMask = createMask(SELECT, insertPoint);
            exitMask->mOperands.push_back(condition);
            exitMask->mOperands.push_back(trueMask);
            exitMask->mOperands.push_back(falseMask);

            exitMasks.push_back(exitMask);

            if(mInfo->mVerbose) {
                outs() << "  exitMask  (2): ";
                exitMask->print(outs());
                outs() << "\n";
            }
        }

        return;
    }

    // For every outgoing edge, create a conjunction of the entry
    // mask and the condition under which the edge is taken (= the
    // exit mask in that direction).
    for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
    {
        BasicBlock* succBB = terminator->getSuccessor(i);

        MaskPtr condition = nullptr;

        if (const BranchInst* brInst = dyn_cast<BranchInst>(terminator))
        {
            if (i == 0)
            {
                condition = createMask(VALUE, insertPoint);
                condition->mValue = brInst->getCondition();
                if(mInfo->mVerbose) {
                    outs() << "  condition (1): ";
                    condition->print(outs());
                    outs() << "\n";
                }
            }
            else
            {
                assert (i == 1);
                MaskPtr cmp = createMask(VALUE, insertPoint);
                cmp->mValue = brInst->getCondition();
                if(mInfo->mVerbose) {
                    outs() << "  cmp      : ";
                    cmp->print(outs());
                    outs() << "\n";
                }

                // We have to negate the condition since this is the 'false' edge.
                condition = createMask(NEGATE, insertPoint);
                condition->mOperands.push_back(cmp);
                if(mInfo->mVerbose) {
                    outs() << "  condition (2): ";
                    condition->print(outs());
                    outs() << "\n";
                }
            }
        }
        else if (SwitchInst* switchInst = dyn_cast<SwitchInst>(terminator))
        {
            condition = createMask(VALUE, insertPoint);

            ConstantInt*       caseDest = switchInst->findCaseDest(succBB);
            SwitchInst::CaseIt caseIt   = switchInst->findCaseValue(caseDest);

            assert (isa<ConstantInt>(caseIt->getCaseValue()) &&
                    "handling of non-constant int values in switch not implemented!");

            ConstantInt* caseConst = caseIt->getCaseValue();
            Value*       cond      = switchInst->getCondition();

            // Create comparison
            ICmpInst* cmp = new ICmpInst(insertPoint,
                                         ICmpInst::ICMP_EQ,
                                         cond,
                                         caseConst,
                                         "switchcmp"+str<int>(i));

            condition->mValue = cmp;
            if(mInfo->mVerbose) {
                outs() << "  condition (3): ";
                condition->print(outs());
                outs() << "\n";
            }
        }
        else
        {
            assert (false && "unsupported terminator instruction found!");
            return;
        }


        MaskPtr exitMask = createMask(CONJUNCTION, insertPoint);
        exitMask->mOperands.push_back(entryMask);
        exitMask->mOperands.push_back(condition);

        exitMasks.push_back(exitMask);

        if(mInfo->mVerbose) {
            outs() << "  exitMask  (3): ";
            exitMask->print(outs());
            outs() << "\n";
        }
    }
}

void
MaskAnalysis::createLoopExitMasks(Loop* loop)
{
    assert (loop);

    // We have to iterate through ALL loops, but only do something
    // for those that are DIVERGENT. Since we have to generate a few
    // things before recursing, this has to be prevented for non-
    // divergent loops.
    const bool isDivergentLoop = WFV::hasMetadata(loop, WFV::WFV_METADATA_LOOP_DIVERGENT_TRUE);

    SmallVector<BasicBlock*, 4> exitingBlocks;
    SmallVector<BasicBlock*, 4> exitBlocks;
    if (isDivergentLoop)
    {
        // Get exiting blocks.
        loop->getExitingBlocks(exitingBlocks);
        loop->getExitBlocks(exitBlocks);

        assert (exitBlocks.size() == exitingBlocks.size());

        // We need information about the parent loops' mask phis during recursion.
        // thus, generate them here already and insert them into the graph.
        for (unsigned i=0, e=exitBlocks.size(); i<e; ++i)
        {
            BasicBlock* exitBlock    = exitBlocks[i];
            BasicBlock* exitingBlock = exitingBlocks[i];

            Instruction* insertPoint = &*loop->getHeader()->getFirstInsertionPt();

            // The exit mask of an OPTIONAL exit is simply the exit mask of
            // the exiting block in direction of the exit block.
            if (WFV::hasMetadata(exitBlock, WFV::WFV_METADATA_OPTIONAL)) continue;

            // If we have some information about this exit already, only add the
            // fact that it is the exit of an additional loop.
            if (mLoopExitMap.count(exitingBlock))
            {
                setLoopExitMaskPtrPhi(*loop, *exitingBlock,
                                      createMask(LOOPEXITPHI, insertPoint));
                continue;
            }

            // Otherwise, create a new entry for the map.
            LoopExitMaskInfo* info = new LoopExitMaskInfo();

            // Derive & store information about this exit.
            // NOTE: There can be a difference between the top level loop
            //       and the outermost loop that this is an exit of.
            //       The current loop can be nested and the current exit can leave
            //       multiple loops, but the exit does not necessarily also leave the
            //       parent of the current loop.
            Loop* innermostLoop = mLoopInfo->getLoopFor(exitingBlock);
            Loop* topLevelLoop  = WFV::findTopLevelLoopOfExit(loop,
                                                              exitingBlock,
                                                              exitBlock,
                                                              *mLoopInfo);

            info->mBlock            = exitingBlock;
            info->mTarget           = exitBlock;
            info->mInnermostLoop    = innermostLoop;
            info->mTopLevelLoop     = topLevelLoop;
            info->mMaskPhiMap[loop] = createMask(LOOPEXITPHI, insertPoint);

            mLoopExitMap[exitingBlock] = info;
        }
    }

    // Recurse into nested loops.
    for (auto subLoop : *loop)
    {
        createLoopExitMasks(subLoop);
    }

    // Ignore non-divergent loops.
    if (!isDivergentLoop)
    {
        if(mInfo->mVerbose) outs() << "Non-divergent sub-loop ignored: " << loop->getHeader()->getName() << "\n";
        return;
    }

    if(mInfo->mVerbose) {
        outs() << "\ngenerating exit mask phis for all exiting blocks of loop "
               << "with header '" << loop->getHeader()->getName() << "'...\n";
        outs() << "  Exiting blocks:\n";
        for (unsigned i = 0, e = exitBlocks.size(); i < e; ++i) {
            BasicBlock *exitBlock = exitBlocks[i];
            BasicBlock *exitingBlock = exitingBlocks[i];
            outs() << "    * " << exitingBlock->getName() << " -> "
                   << exitBlock->getName() << "\n";
        }
    }

    BasicBlock* preheaderBB = loop->getLoopPreheader();
    BasicBlock* latchBB     = loop->getLoopLatch();

    for (unsigned i=0, e=exitBlocks.size(); i<e; ++i)
    {
        BasicBlock* exitBlock    = exitBlocks[i];
        BasicBlock* exitingBlock = exitingBlocks[i];

        if(mInfo->mVerbose) outs() << "  generating exit mask phi for exiting block '"
                                   << exitingBlock->getName() << "'...\n";

        Instruction* insertPoint = exitingBlock->getTerminator();

        // Ignore edge if the target is OPTIONAL.
        if (WFV::hasMetadata(exitBlock, WFV::WFV_METADATA_OPTIONAL))
        {
            assert (WFV::hasMetadata(exitingBlock->getTerminator(), WFV::WFV_METADATA_OP_UNIFORM));
            if(mInfo->mVerbose) outs() << "    target of exit block '"
                                       << exitingBlock->getName() << "' is OPTIONAL, ignored!\n";
            continue;
        }

        LoopExitMaskInfo* exitInfo = mLoopExitMap[exitingBlock];
        assert (exitInfo);

        const bool isInnermostLoopOfExit = exitInfo->mInnermostLoop == loop;
        const bool isTopLevelLoopOfExit  = exitInfo->mTopLevelLoop == loop;
        const bool exitsMultipleLoops    = exitInfo->mMaskPhiMap.size() > 1;
        if(mInfo->mVerbose) {
            if (isInnermostLoopOfExit) outs() << "    is innermost loop of exit!\n";
            if (isTopLevelLoopOfExit) outs() << "    is top level loop of exit!\n";
            if (exitsMultipleLoops) outs() << "    block has multi-loop exit!\n";
        }


        // Find exit direction and corresponding mask.
        assert (mBlockMap.count(exitingBlock) &&
                "there has to be a mask graph node for each block!");
        assert (mBlockMap[exitingBlock]->mExitMasks.size() > 1);

        MaskPtr exitMask = getExitMaskPtr(*exitingBlock,
                                          *exitInfo->mTarget);

        // Get mask phi of current exit (already generated before
        // recursion into child-loops).
        assert (exitInfo->mMaskPhiMap.count(loop));
        assert (exitInfo->mMaskPhiMap[loop]);
        MaskPtr exitMaskPhi = exitInfo->mMaskPhiMap[loop];

        // Incoming value from preheader is a zero-vector (we collect only
        // those instances that left during this iteration of the parent loop.
        // It is updated in the parent loop).
        // NOTE: Mask materialization (MaskGenerator.cpp) relies on
        //       the preheader mask being the first operand).
        MaskPtr boolZeroConstMask = createMask(CONSTANT, insertPoint);
        boolZeroConstMask->mValue = mInfo->mConstBoolFalse;

        exitMaskPhi->mOperands.push_back(boolZeroConstMask);
        exitMaskPhi->mIncomingDirs.push_back(preheaderBB);

        // Incoming value from latch is:
        // - if in innermost loop or non-nested loop:
        //     new 'or' of loop's mask phi and the block's old exit mask
        //     ((negated) branch condition)
        // - if in non-innermost loop of multi-loop exit:
        //     new 'or' of loop's mask phi and the 'or' of the next nested loop.
        MaskPtr maskUpdateOp = createMask(LOOPEXITUPDATE, insertPoint);
        maskUpdateOp->mOperands.push_back(exitMaskPhi);

        if (exitsMultipleLoops && !isInnermostLoopOfExit)
        {
            Loop* nextNestedLoop = WFV::findNextNestedLoopOfExit(loop, exitingBlock);
            assert (nextNestedLoop);
            if(mInfo->mVerbose) outs() << "    next nested loop: " << nextNestedLoop->getHeader()->getName() << "\n";
            assert (exitInfo->mMaskUpdateOpMap.count(nextNestedLoop));

            maskUpdateOp->mOperands.push_back(exitInfo->mMaskUpdateOpMap[nextNestedLoop]);
        }
        else
        {
            maskUpdateOp->mOperands.push_back(exitMask);
        }

        setLoopExitMaskPtrUpdate(*loop, *exitingBlock, maskUpdateOp);
        assert (exitInfo->mMaskUpdateOpMap.count(loop));
        assert (exitInfo->mMaskUpdateOpMap[loop] == maskUpdateOp);

        if(mInfo->mVerbose) {
            outs() << "    generated new loop exit mask update operation: ";
            maskUpdateOp->print(outs());
            outs() << "\n";
        }

        // If this is the top level loop of this exit, set the mask of
        // the edge to this loop's update operation (which stores all
        // instances that left over this edge for *any* iteration of *any*
        // enclosing loop.
        if (isTopLevelLoopOfExit)
        {
            setExitMaskPtr(*exitingBlock, *exitBlock, maskUpdateOp);
        }

        // Add incoming value to phi.
        exitMaskPhi->mOperands.push_back(maskUpdateOp);
        exitMaskPhi->mIncomingDirs.push_back(latchBB);

        if(mInfo->mVerbose) {
            outs() << "    generated loop exit mask phi: ";
            exitMaskPhi->print(outs());
            outs() << "\n";
        }
    }

    if(mInfo->mVerbose) {
        outs() << "finished generation of exit mask phis for loop!\n";
        outs() << "\ngenerating combined exit mask for loop!\n";
    }

    // Create a combined loop exit mask with the exit masks of all MANDATORY loop
    // exits as operands (required for select generation).
    Instruction* insertPoint = latchBB->getTerminator();

    // The combined mask is the "or" of all MANDATORY loop exit masks or a
    // reference to a single exit mask.
    MaskPtr combinedMask = nullptr;
    for (unsigned i=0, e=exitBlocks.size(); i<e; ++i)
    {
        BasicBlock* exitBB    = exitBlocks[i];
        BasicBlock* exitingBB = exitingBlocks[i];

        // Ignore edge if the successor outside the current loop is OPTIONAL.
        if (WFV::hasMetadata(exitBB, WFV::WFV_METADATA_OPTIONAL))
        {
            assert (WFV::hasMetadata(exitingBB->getTerminator(), WFV::WFV_METADATA_OP_UNIFORM));
            if(mInfo->mVerbose) outs() << "  exit branch of block '" << exitingBB->getName()
                                       << "' has OPTIONAL exit successor, ignored!\n";
            continue;
        }

        if(mInfo->mVerbose) outs() << "  combining with exit mask of block '" << exitingBB->getName() << "'\n";

        // Now we have the blended loop exit mask where ALL instances that
        // left the loop over this exit in any previous iteration are 'true'.
        // This mask is only desired for blending if the exit leaves from a
        // nested loop (return/break n) where all iterations of this inner loop
        // correspond to the current iteration of the parent loop.
        // For exits leaving the current loop we only want those instances that
        // left the loop in the CURRENT iteration.
        // Summarized:
        // - for exits leaving from a nested loop:
        //    - use accumulated loop exit mask (of current iteration of parent loop!)
        // - for exits leaving from the current loop:
        //    - use corresponding exit condition
        //    - by construction, the required mask is the left hand operand of
        //      the 'or' (RHO is the exit mask phi)
        MaskPtr loopExitMask;
        {
            LoopExitMaskInfo* exitInfo = mLoopExitMap[exitingBB];
            assert (exitInfo);

            // For multi-loop exits, the edges mask is set to the "allexited" mask,
            // so we first have to get the mask update operation that holds only those
            // of the current iteration.
            loopExitMask = exitInfo->mMaskUpdateOpMap[loop];
            assert (loopExitMask);
            if(mInfo->mVerbose) {
                outs() << "  update operation: ";
                loopExitMask->print(outs());
                outs() << "\n";
            }
            assert (loopExitMask->mType == LOOPEXITUPDATE);
            assert (loopExitMask->mOperands[0].lock()->mType == LOOPEXITPHI);

            // We need only those instances that left the loop in the current iteration
            // of the current loop (which may include multiple iterations of all inner
            // loops of that exit). These instances are given by the update operation
            // of the next nested loop or the exit mask if this is the innermost loop.
            assert (exitInfo->mInnermostLoop == loop ||
                    loopExitMask->mOperands[1].lock()->mType == LOOPEXITUPDATE);
            loopExitMask = loopExitMask->mOperands[1].lock();
        }

        if(mInfo->mVerbose) {
            outs() << "  input mask: ";
            loopExitMask->print(outs());
            outs() << "\n";
        }

        // Combine mask.
        if (!combinedMask)
        {
            combinedMask = loopExitMask;
        }
        else
        {
            MaskPtr newCombinedMask = createMask(DISJUNCTION, insertPoint);
            newCombinedMask->mOperands.push_back(combinedMask);
            newCombinedMask->mOperands.push_back(loopExitMask);
            combinedMask = newCombinedMask;
        }
    }

    mLoopMaskMap[loop]->mCombinedLoopExitMask = combinedMask;

    if(mInfo->mVerbose) {
        outs() << "generated combined loop exit mask: ";
        combinedMask->print(outs());
        outs() << "\n";
    }

    return;
}

void
MaskAnalysis::invalidateInsertPoints()
{
    for (auto &mask : mMasks)
    {
        assert (mask->mInsertPoint);
        mask->mInsertPoint = nullptr;
    }
}

// This function maps mask values according to the given map.
// It is used after reg2mem/mem2reg after CFG linearization.
void
MaskAnalysis::mapMaskValues(MaskValueMapType& valueMap)
{
    for (auto &mask : mMasks)
    {
        if (!mask->mValue) continue; // Mask not used.
        if (!valueMap.count(mask->mValue)) continue; // Mask not mapped.
        mask->mValue = valueMap[mask->mValue];
    }
}

// This function maps the entire mask info structure including blocks.
// It is used during function vectorization after cloning the scalar code into
// the SIMD prototype.
void
MaskAnalysis::mapMaskInformation(ValueToValueMapTy& valueMap)
{
    // We iterate over a map while deleting and inserting elements...
    // Better just create a new map and replace the old one.
    DenseMap<const BasicBlock*, BlockMaskInfo*> newBlockMap;

    for (auto &it : mBlockMap)
    {
        // Map BlockMaskInfo contents.
        BlockMaskInfo* bi = it.second;
        BasicBlock* block = bi->mBlock;

        assert (valueMap.count(block));
        BasicBlock* newBlock = cast<BasicBlock>(valueMap[block]);
        bi->mBlock = newBlock;

        // Map entry of mBlockMap itself.
        newBlockMap[newBlock] = bi;
    }

    mBlockMap.clear();
    std::swap(mBlockMap, newBlockMap);

    for (auto &it : mLoopExitMap)
    {
        LoopExitMaskInfo* li = it.second;
        assert (valueMap.count(li->mBlock));
        assert (valueMap.count(li->mTarget));
        li->mBlock  = cast<BasicBlock>(valueMap[li->mBlock]);
        li->mTarget = cast<BasicBlock>(valueMap[li->mTarget]);
    }
    for (auto &mask : mMasks)
    {
        for (unsigned i=0, e=mask->mIncomingDirs.size(); i<e; ++i)
        {
            assert (valueMap.count(mask->mIncomingDirs[i]));
            mask->mIncomingDirs[i] = cast<BasicBlock>(valueMap[mask->mIncomingDirs[i]]);
        }

        // There are no insert points anymore after CFG linearization
        // (which calls invalidateInsertPoints()).
        if (mask->mInsertPoint)
        {
            assert (valueMap.count(mask->mInsertPoint));
            mask->mInsertPoint = cast<Instruction>(valueMap[mask->mInsertPoint]);
        }

        if (valueMap.count(mask->mValue))
        {
            mask->mValue = valueMap[mask->mValue];
        }
    }
}

Value*
MaskAnalysis::getEntryMask(const BasicBlock& block) const
{
    assert (getEntryMaskPtr(block)->mValue);
    return getEntryMaskPtr(block)->mValue;
}

Value*
MaskAnalysis::getExitMask(const BasicBlock& block,
                          const unsigned    index) const
{
    assert (getExitMaskPtr(block, index)->mValue);
    return getExitMaskPtr(block, index)->mValue;
}

Value*
MaskAnalysis::getExitMask(const BasicBlock& block,
                          const BasicBlock& direction) const
{
    assert (getExitMaskPtr(block, direction)->mValue);
    return getExitMaskPtr(block, direction)->mValue;
}

Value*
MaskAnalysis::getLoopMaskPhi(const Loop& loop) const
{
    assert(getLoopMaskPtrPhi(loop)->mValue);
    return getLoopMaskPtrPhi(loop)->mValue;
}

Value*
MaskAnalysis::getCombinedLoopExitMask(const Loop& loop) const
{
    assert(getCombinedLoopExitMaskPtr(loop)->mValue);
    return getCombinedLoopExitMaskPtr(loop)->mValue;
}

Value*
MaskAnalysis::getLoopExitMaskPhi(const Loop&       loop,
                                 const BasicBlock& exitingBlock) const
{
    assert(getLoopExitMaskPtrPhi(loop, exitingBlock)->mValue);
    return getLoopExitMaskPtrPhi(loop, exitingBlock)->mValue;
}

Value*
MaskAnalysis::getLoopExitMaskUpdate(const Loop&       loop,
                                    const BasicBlock& exitingBlock) const
{
    assert (getLoopExitMaskPtrUpdate(loop, exitingBlock)->mValue);
    return getLoopExitMaskPtrUpdate(loop, exitingBlock)->mValue;
}


MaskPtr
MaskAnalysis::getEntryMaskPtr(const BasicBlock& block) const
{
    assert (mBlockMap.count(&block));
    assert (mBlockMap.find(&block)->second);
    assert (mBlockMap.find(&block)->second->mEntryMask);
    return mBlockMap.find(&block)->second->mEntryMask;
}

MaskPtr
MaskAnalysis::getExitMaskPtr(const BasicBlock& block,
                             const unsigned    index) const
{
    assert (mBlockMap.count(&block));
    assert (mBlockMap.find(&block)->second);
    assert (mBlockMap.find(&block)->second->mExitMasks.size() > index);
    assert (mBlockMap.find(&block)->second->mExitMasks[index]);
    return mBlockMap.find(&block)->second->mExitMasks[index];
}

MaskPtr
MaskAnalysis::getLoopMaskPtrPhi(const Loop& loop) const
{
    assert (mLoopMaskMap.count(&loop));
    return mLoopMaskMap.find(&loop)->second->mMaskPhi;
}

MaskPtr
MaskAnalysis::getCombinedLoopExitMaskPtr(const Loop& loop) const
{
    assert (mLoopMaskMap.count(&loop));
    assert (mLoopMaskMap.find(&loop)->second);
    return mLoopMaskMap.find(&loop)->second->mCombinedLoopExitMask;
}

MaskPtr
MaskAnalysis::getExitMaskPtr(const BasicBlock& block,
                             const BasicBlock& direction) const
{
    assert (mBlockMap.count(&block));

    unsigned index = 0;
    bool found = false; WFV_UNUSED(found);
    const TerminatorInst& terminator = *block.getTerminator();
    for (unsigned e=terminator.getNumSuccessors(); index<e; ++index)
    {
        const BasicBlock* succBB = terminator.getSuccessor(index);
        if (succBB != &direction) continue;

        found = true;
        break;
    }

    assert (found && "block is no valid successor!");

    return getExitMaskPtr(block, index);
}

MaskPtr
MaskAnalysis::getLoopExitMaskPtrPhi(const Loop&       loop,
                                    const BasicBlock& exitingBlock) const
{
    assert (!WFV::hasMetadata(WFV::getExitBlock(&exitingBlock, *mLoopInfo),
                              WFV::WFV_METADATA_OPTIONAL) &&
            "must not query loop exit mask phi of OPTIONAL exit!");
    assert (mLoopExitMap.count(&exitingBlock));
    assert (mLoopExitMap.find(&exitingBlock)->second);
    assert (mLoopExitMap.find(&exitingBlock)->second->mMaskPhiMap.count(&loop));
    LoopExitMaskInfo* info = mLoopExitMap.find(&exitingBlock)->second;
    return info->mMaskPhiMap[&loop];
}

MaskPtr
MaskAnalysis::getLoopExitMaskPtrUpdate(const Loop&       loop,
                                       const BasicBlock& exitingBlock) const
{
    assert (!WFV::hasMetadata(WFV::getExitBlock(&exitingBlock, *mLoopInfo),
                              WFV::WFV_METADATA_OPTIONAL) &&
            "must not query loop exit mask update of OPTIONAL exit!");
    assert (mLoopExitMap.count(&exitingBlock));
    assert (mLoopExitMap.find(&exitingBlock)->second);
    assert (mLoopExitMap.find(&exitingBlock)->second->mMaskUpdateOpMap.count(&loop));
    LoopExitMaskInfo* info = mLoopExitMap.find(&exitingBlock)->second;
    return info->mMaskUpdateOpMap[&loop];
}

void
MaskAnalysis::setEntryMask(const BasicBlock& block,
                           Value*            value)
{
    assert (!getEntryMaskPtr(block)->mValue);
    getEntryMaskPtr(block)->mValue = value;
}

void
MaskAnalysis::setExitMask(const BasicBlock& block,
                          const unsigned    index,
                          Value*            value)
{
    assert (!getExitMaskPtr(block, index)->mValue);
    getExitMaskPtr(block, index)->mValue = value;
}

void
MaskAnalysis::setExitMask(const BasicBlock& block,
                          const BasicBlock& direction,
                           Value*           value)
{
    assert (!getExitMaskPtr(block, direction)->mValue);
    getExitMaskPtr(block, direction)->mValue = value;
}

void
MaskAnalysis::setLoopExitMaskPhi(const Loop&       loop,
                                 const BasicBlock& exitingBlock,
                                 Value*            value)
{
    assert(!getLoopExitMaskPtrPhi(loop, exitingBlock)->mValue);
    getLoopExitMaskPtrPhi(loop, exitingBlock)->mValue = value;
}

void
MaskAnalysis::setLoopExitMaskUpdate(const Loop&       loop,
                                    const BasicBlock& exitingBlock,
                                    Value*            value)
{
    assert (!getLoopExitMaskPtrUpdate(loop, exitingBlock)->mValue);
    getLoopExitMaskPtrUpdate(loop, exitingBlock)->mValue = value;
}

void
MaskAnalysis::copyMaskInfoToNewBlock(BasicBlock*       newBlock,
                                     const BasicBlock& oldBlock)
{
    assert (mBlockMap.count(&oldBlock));
    assert (!mBlockMap.count(newBlock));

    // Store information in new graph node.
    BlockMaskInfo* info = new BlockMaskInfo();
    info->mBlock        = newBlock;

    // Copy entry mask.
    info->mEntryMask    = getEntryMaskPtr(oldBlock);

    // Copy exit mask(s).
    // We must not iterate over the actual successors of the old
    // block since the information may not be accurate anymore
    // (e.g. in cases where the block was split).
    for (unsigned i=0, e=info->mExitMasks.size(); i<e; ++i)
    {
        info->mExitMasks.push_back(getExitMaskPtr(oldBlock, i));
    }

    // Insert node into mask graph.
    mBlockMap[newBlock] = info;
}

void
MaskAnalysis::clearEntryMask(const BasicBlock& block)
{
    assert (mBlockMap.count(&block));
    BlockMaskInfo* info = mBlockMap[&block];
    info->mEntryMask.reset();
}

void
MaskAnalysis::clearExitMasks(const BasicBlock& block)
{
    assert (mBlockMap.count(&block));
    BlockMaskInfo* info = mBlockMap[&block];
    for (auto &mask : info->mExitMasks)
    {
        mask.reset();
    }
    info->mExitMasks.clear();
}

void
MaskAnalysis::updateEntryMask(const BasicBlock& block,
                              Value*            newMask,
                              Instruction*      insertPoint)
{
    assert (newMask && insertPoint);
    clearEntryMask(block);

    BlockMaskInfo* info = mBlockMap[&block];

    MaskPtr mask = createMask(VALUE, insertPoint);
    mask->mValue = newMask;
    info->mEntryMask =mask;
}

void
MaskAnalysis::updateExitMasks(const BasicBlock& block,
                              Value*            newMask0,
                              Value*            newMask1,
                              Instruction*      insertPoint)
{
    assert (newMask0 && insertPoint);
    clearExitMasks(block);

    BlockMaskInfo* info = mBlockMap[&block];

    MaskPtr mask0 = createMask(VALUE, insertPoint);
    mask0->mValue = newMask0;
    info->mExitMasks.push_back(mask0);

    if (!newMask1) return;

    MaskPtr mask1 = createMask(VALUE, insertPoint);
    mask1->mValue = newMask1;
    info->mExitMasks.push_back(mask1);
}

#if 0
void
MaskAnalysis::updateExitMask(const BasicBlock& block,
                             const unsigned    index,
                             Value*            value)
{
    assert (value);
    assert (getExitMaskPtr(block, index)->mValue);
    mBlockMap[&block]->mExitMasks[index]->mValue = value;
}

void
MaskAnalysis::resizeExitMasks(const BasicBlock& block,
                              const unsigned    newSize)
{
    assert (mBlockMap.count(&block));
    mBlockMap[&block]->mExitMasks.resize(newSize);
}
#endif

void
MaskAnalysis::removeExitMask(const BasicBlock& block,
                             const unsigned    index)
{
    assert (mBlockMap.count(&block));
    assert (mBlockMap[&block]->mExitMasks.size() > index);

    BlockMaskInfo* info = mBlockMap[&block];
    if (info->mExitMasks[index]) info->mExitMasks[index].reset();
    SmallVector<MaskPtr, 2>::iterator it = info->mExitMasks.begin();
    std::advance(it, index);
    info->mExitMasks.erase(it);
}

void
MaskAnalysis::removeExitMask(const BasicBlock& block,
                             const BasicBlock& direction)
{
    assert (mBlockMap.count(&block));

    unsigned index = 0;
    bool found = false; WFV_UNUSED(found);
    const TerminatorInst& terminator = *block.getTerminator();
    for (unsigned e=terminator.getNumSuccessors(); index<e; ++index)
    {
        const BasicBlock* succBB = terminator.getSuccessor(index);
        if (succBB != &direction) continue;

        found = true;
        break;
    }

    assert (found && "block is no valid successor!");

    removeExitMask(block, index);
}

void
MaskAnalysis::setEntryMaskPtr(const BasicBlock& block,
                              MaskPtr           mask)
{
    assert (mBlockMap.count(&block));
    assert (mBlockMap.find(&block)->second);

    BlockMaskInfo* info = mBlockMap[&block];

    MaskPtr oldMask = info->mEntryMask;
    if (oldMask) oldMask.reset();

    info->mEntryMask = mask;
}

void
MaskAnalysis::setExitMaskPtr(const BasicBlock& block,
                             const unsigned    index,
                             MaskPtr           mask)
{
    assert (mBlockMap.count(&block));
    assert (mBlockMap.find(&block)->second);
    assert (mBlockMap.find(&block)->second->mExitMasks.size() > index);

    BlockMaskInfo* info = mBlockMap[&block];

    MaskPtr oldMask = info->mExitMasks[index];
    if (oldMask) oldMask.reset();

    info->mExitMasks[index] = mask;
}

void
MaskAnalysis::setExitMaskPtr(const BasicBlock& block,
                             const BasicBlock& direction,
                             MaskPtr           mask)
{
    assert (mBlockMap.count(&block));

    unsigned index = 0;
    bool found = false; WFV_UNUSED(found);
    const TerminatorInst& terminator = *block.getTerminator();
    for (unsigned e=terminator.getNumSuccessors(); index<e; ++index)
    {
        const BasicBlock* succBB = terminator.getSuccessor(index);
        if (succBB != &direction) continue;

        found = true;
        break;
    }

    assert (found && "block is no valid successor!");

    setExitMaskPtr(block, index, mask);
}

void
MaskAnalysis::setLoopExitMaskPtrPhi(const Loop&       loop,
                                    const BasicBlock& exitingBlock,
                                    MaskPtr           mask)
{
    assert (mLoopExitMap.count(&exitingBlock));

    LoopExitMaskInfo* info = mLoopExitMap[&exitingBlock];

    if (info->mMaskPhiMap.count(&loop))
    {
        MaskPtr oldMask = info->mMaskPhiMap[&loop];
        oldMask.reset();
    }

    info->mMaskPhiMap[&loop] = mask;
}

void
MaskAnalysis::setLoopExitMaskPtrUpdate(const Loop&       loop,
                                       const BasicBlock& exitingBlock,
                                       MaskPtr           mask)
{
    assert (mLoopExitMap.count(&exitingBlock));

    LoopExitMaskInfo* info = mLoopExitMap[&exitingBlock];

    if (info->mMaskUpdateOpMap.count(&loop))
    {
        MaskPtr oldMask = info->mMaskUpdateOpMap[&loop];
        oldMask.reset();
    }

    info->mMaskUpdateOpMap[&loop] = mask;
}
