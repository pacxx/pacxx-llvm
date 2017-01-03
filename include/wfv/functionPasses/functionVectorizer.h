/**
 * @file   functionVectorizer.h
 * @date   30.05.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */
#ifndef _FUNCTIONVECTORIZER_H
#define	_FUNCTIONVECTORIZER_H

#include "wfv/wfvInfo.h"
#include "wfv/analysis/maskAnalysis.h"

#include "llvm/Pass.h"
#include "llvm/IR/InstVisitor.h"

#define WFV_FUNCTION_NAME_PACK      "wfv_pack"
#define WFV_FUNCTION_NAME_UNPACK    "wfv_unpack"
#define WFV_FUNCTION_NAME_FORWARD   "wfv_forward"
#define WFV_FUNCTION_NAME_PACK_W    "wfv_Wpack"
#define WFV_FUNCTION_NAME_UNPACK_W  "wfv_Wunpack"

using namespace llvm;

enum TrivialConstant {
	ALL_ZERO = 0,
	ALL_ONE = 1,
	ALL_UNDEF = 2
};


class FunctionVectorizer :
        public FunctionPass,
        public InstVisitor<FunctionVectorizer, bool>
{
public:
    static char ID; // Pass identification, replacement for typeid.

    FunctionVectorizer();
	~FunctionVectorizer();

    virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    virtual bool doInitialization(Module& M);
    virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
    virtual void print           (raw_ostream& O, const Module* M) const;

    // These are only public because the InstVisitor interface requires that.
    bool visitInstruction(Instruction& I);

    bool visitReturnInst(ReturnInst &I);
    bool visitBranchInst(BranchInst &I);
    //bool visitSwitchInst(SwitchInst &I);
    //bool visitIndirectBrInst(IndirectBrInst &I);
    //bool visitResumeInst(ResumeInst &I);
    //bool visitUnreachableInst(UnreachableInst &I);
    //bool visitICmpInst(ICmpInst &I);
    //bool visitFCmpInst(FCmpInst &I);
    bool visitAllocaInst(AllocaInst &I);
    bool visitLoadInst(LoadInst     &I);
    bool visitStoreInst(StoreInst   &I);
    //bool visitAtomicCmpXchgInst(AtomicCmpXchgInst &I);
    //bool visitAtomicRMWInst(AtomicRMWInst &I);
    //bool visitFenceInst(FenceInst   &I);
    bool visitGetElementPtrInst(GetElementPtrInst &I);
    bool visitPHINode(PHINode       &I);
    bool visitTruncInst(TruncInst &I);
    bool visitZExtInst(ZExtInst &I);
    bool visitSExtInst(SExtInst &I);
    bool visitFPTruncInst(FPTruncInst &I);
    bool visitFPExtInst(FPExtInst &I);
    //bool visitFPToUIInst(FPToUIInst &I);
    //bool visitFPToSIInst(FPToSIInst &I);
    //bool visitUIToFPInst(UIToFPInst &I);
    //bool visitSIToFPInst(SIToFPInst &I);
    //bool visitPtrToIntInst(PtrToIntInst &I);
    //bool visitIntToPtrInst(IntToPtrInst &I);
    //bool visitBitCastInst(BitCastInst &I);
    bool visitSelectInst(SelectInst &I);
    //bool visitVAArgInst(VAArgInst   &I);
    //bool visitExtractElementInst(ExtractElementInst &I);
    //bool visitInsertElementInst(InsertElementInst &I);
    //bool visitShuffleVectorInst(ShuffleVectorInst &I);
    bool visitExtractValueInst(ExtractValueInst &I);
    bool visitInsertValueInst(InsertValueInst &I);
    //bool visitLandingPadInst(LandingPadInst &I);

    bool visitCallInst(CallInst &I);
    //bool visitInvokeInst(InvokeInst &I);

    bool visitCastInst(CastInst &I);
    bool visitBinaryOperator(BinaryOperator &I);
    bool visitCmpInst(CmpInst &I);
    //bool visitTerminatorInst(TerminatorInst &I);
    //bool visitUnaryInstruction(UnaryInstruction &I);

    //bool visitCallSite(CallSite CS);

private:

    Constant * getTrivialConstant(Type * ty, TrivialConstant trivConst) const;
    WFVInfo*      mInfo;
    MaskAnalysis* mMaskAnalysis;

    void cloneIntoSIMDPrototype(Function*          f_SIMD,
                                const Function&    f,
                                ValueToValueMapTy& valueMap) const;
    void createPtrArgCasts(Function* f);
    BitCastInst* createBitCastToEquivalentVectorType(Argument*    arg,
                                                     Instruction* insertBefore);
    Type* createEquivalentVectorType(Type* oldType);
    bool isArgCast(const Value& value) const;
    void cleanup(Function* f) const;

    // Vectorization Phases.
    void insertPackUnpackIntrinsics(Function* f);
    void optimizePackUnpack(Function* f);
    void duplicateSplitInstructions(Function* f, const unsigned vectorizationFactor);
    bool generatePackUnpackCode(Function* f, const WFVInfo& info);
    void generateSideEffectGuards(Function* f);
    bool vectorizeInstructions(Function* f);
    bool broadcastUniformOperands(Function* f);

    //
    // Common utility functions for all phases.
    //
    std::string getTypeString(Type* type);
    std::string getMangledFunctionName(const char*     name,
                                       Type*           returnType,
                                       ArrayRef<Type*> paramTypes);
    Function*   getSpecialWFVFunction(const char*     name,
                                      Type*           returnType,
                                      ArrayRef<Type*> paramTypes,
                                      Module*         mod);
    bool        isPackFunctionCall(const Instruction* inst);
    bool        isUnpackFunctionCall(const Instruction* inst);
    bool        isForwardFunctionCall(const Instruction* inst);
    bool        isPackWFunctionCall(const Instruction* inst);
    bool        isUnpackWFunctionCall(const Instruction* inst);

    bool        isEntryMaskUseFnCall(const Instruction* inst);

    Value* generateHorizontalExtract(Value*         V,
                                     Value*         indexVal,
                                     StringRef      name,
                                     Instruction*   allocPos,
                                     Instruction*   insertBefore,
                                     const WFVInfo& info);
    Instruction* generateHorizontalPointerExtract(Value*         V,
                                                  Value*         indexVal,
                                                  Type*          elementType,
                                                  StringRef      name,
                                                  Instruction*   allocPos,
                                                  Instruction*   insertBefore,
                                                  const WFVInfo& info);

    Instruction* generateHorizontalMerge(SmallVector<Value*, 8>& splitVals,
                                         Type*                   targetType,
                                         StringRef               name,
                                         Instruction*            insertBefore,
                                         const WFVInfo&          info);

    bool generateWriteBackOperations(Instruction*   currentPos,
                                     Instruction*   extractedVal,
                                     Value*         originalVal,
                                     Value*         indexVal,
                                     const WFVInfo& info);

    Instruction* generateHorizontalInsert(Value*         scalarVal,
                                          Value*         targetVal,
                                          Value*         indexVal,
                                          Instruction*   insertBefore,
                                          const WFVInfo& info);

    Value* createPointerCast(Value*       pointer,
                             Instruction* insertBefore);

    Instruction* createPTest(Value* cond, Instruction* insertBefore) const;

    // duplicateInstructions.cpp

    void createGuards(Instruction*                     inst,
                      DenseMap<Instruction*, Value**>& duplicateMap);

    void generateIfCascade(Instruction*  inst,
                           Value*        mask,
                           BasicBlock**  ifBlocks,
                           BasicBlock**  targetBlocks,
                           MaskAnalysis* maskAnalysis);

    void generateIf(Instruction*  inst,
                    Value*        mask,
                    MaskAnalysis* maskAnalysis,
                    BasicBlock**  outIfBB,
                    BasicBlock**  outTargetBB);

    //
    // vectorizeInstructions.cpp
    //

    Instruction* generateNativeVectorFunctionCall(CallInst*       oldCall,
                                                  const Function& simdFn,
                                                  Value*          mask,
                                                  const int       maskIndex);

    typedef SmallVector<std::pair<Instruction*, Value*>, 4> DummyVecType;
    Value* getMaskArgument(Value*        mask,
                           Type*         paramType,
                           DummyVecType& dummies,
                           Instruction*  insertBefore);

    template<unsigned T1, unsigned T2>
    void createArgumentsForCall(const SmallVector<Value*, T1>& args,
                                const SmallVector<bool, T1>&   uniformArgs,
                                const unsigned                 callIndex,
                                const unsigned                 vectorizationFactor,
                                const unsigned                 callVecFactor,
                                SmallVector<Value*, T2>&       callArgs,
                                Instruction*                   insertBefore,
                                const WFVInfo&                 info);

    template<unsigned T>
    Instruction* mergeCallResults(SmallVector<CallInst*, T>& calls,
                                  const unsigned             vectorizationFactor,
                                  const unsigned             callVecFactor,
                                  Instruction*               insertBefore,
                                  const WFVInfo&             info);

    //
    // broadcastUniformOperands.cpp
    //

    Value* broadcastValue(Value*         oldVal,
                          Instruction*   insertBefore,
                          const WFVInfo& mInfo) const;

    bool broadcastUniformOperand(Instruction*   inst,
                                 Value*         operand,
                                 const WFVInfo& info) const;

    bool broadcastUniformCallOperands(CallInst*      call,
                                      const WFVInfo& info) const;

    bool broadcastUniformStoreOperands(StoreInst*     store,
                                       const WFVInfo& info) const;

    Constant* createVectorConstant(Constant*      oldC,
                                   const WFVInfo& info) const;
};


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeFunctionVectorizerPass(PassRegistry&);
FunctionPass* createFunctionVectorizerPass();
}


#endif	/* _FUNCTIONVECTORIZER_H */
