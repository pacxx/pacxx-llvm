/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2013-2014
*/

#include <iostream>
#include <vector>
#include <set>
#include <cassert>
#include <sstream>
#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/IR/InstVisitor.h"
#include "../lib/IR/LLVMContextImpl.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/PACXXTransforms.h"

#include "llvm/IR/Dominators.h"
#include <string>

#include "ModuleHelper.h"
#include "CallVisitor.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace
{
    template <typename T>
    Instruction* getFirstInstructionForConstantExpr(T& kernels, ConstantExpr& CE)
    {
        for (auto CEU : CE.users()) {
            if (auto I = dyn_cast<Instruction>(CEU)) {
                if (I->getParent()) {
                    auto F = I->getParent()->getParent();
                    if (find(begin(kernels), end(kernels), F) != end(kernels)) {
                        return I;
                    }
                }
            }
            if (auto nextCE = dyn_cast<ConstantExpr>(CEU)) {
                return getFirstInstructionForConstantExpr(kernels, *nextCE);
            }
        }
        return nullptr;
    }

    template <typename T> Function* getParentKernel(T& kernels, GlobalVariable& GV)
    {
        Function* F = nullptr;
        for (auto U : GV.users()) {
            Instruction* I = nullptr;
            if (isa<Instruction>(U)) {
                I = cast<Instruction>(U);
            }
            if (auto CE = dyn_cast<ConstantExpr>(U)) {
                I = getFirstInstructionForConstantExpr(kernels, *CE);
            }

            if (I && I->getParent()) {
                F = I->getParent()->getParent();
            }
            if (find(begin(kernels), end(kernels), F) != end(kernels)) {
                break;
            }
        }
        return F;
    }

    struct SPIRPass : public ModulePass {
        static char ID;
        SPIRPass() : ModulePass(ID) {}
        virtual ~SPIRPass() {}

        virtual bool runOnModule(Module& M)
        {
            _M = &M;

            bool modified = true;

            unsigned ptrSize = M.getDataLayout().getPointerSizeInBits();
            if (ptrSize == 64) {
                M.setTargetTriple("spir64-unknown-unknown");

                M.setDataLayout("e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:"
                                "64-f32:32:32-f64:64:64-v16:16:16-v24:32:32-v32:32:32-"
                                "v48:64:64-v64:64:64-v96:128:128-v128:128:128-v192:256:"
                                "256-v256:256:256-v512:512:512-v1024:1024:1024");
            } else {
                M.setTargetTriple("spir-unknown-unknown");

                M.setDataLayout("e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:"
                                "64-f32:32:32-f64:64:64-v16:16:16-v24:32:32-v32:32:32-"
                                "v48:64:64-v64:64:64-v96:128:128-v128:128:128-v192:256:"
                                "256-v256:256:256-v512:512:512-v1024:1024:1024");
            }

            kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

            auto barrier = M.getFunction("_Z7barrierj");
            if (barrier) {
                auto tb = M.getOrInsertFunction("tmp_barrier", barrier->getFunctionType());

                barrier->replaceAllUsesWith(tb);
            }

            auto visitor = make_CallVisitor([&](CallInst* I) {
                if (!I)
                    return;

                if (!I->isInlineAsm()) {

                    if (!isa<Function>(I->getCalledValue())) {
                        //__error("Call to a function pointer in kernel code. Calls to "
                        //        "function pointers are not supported!");
                        exit(1);
                    }
                    I->setCallingConv(CallingConv::SPIR_FUNC);
                    I->getCalledFunction()->setCallingConv(CallingConv::SPIR_FUNC);
                }
            });

            for (auto& F : kernels) {
                visitor.visit(F);
            }

            map<Function*, unsigned> SMMapping;
            for (auto& GV : M.getGlobalList()) {
                if (GV.getType()->isPointerTy() && GV.getType()->getAddressSpace() == 3) {
                    auto F = getParentKernel(kernels, GV);
                    if (F) {
                        unsigned i = SMMapping[F];

                        string newName = F->getName().str() + ".sm" + to_string(i);
                        GV.setName(newName);
                        SMMapping[F] = i + 1;
                        break;
                    }
                    //__dump(GV);
                }
            }

            auto called = visitor.get();

            for (Function* F : called) {
                AddrSpaceCastRemover rem;
                rem.visit(F);
                rem.finalize(F);
            }

            for (auto& F : kernels) {
                AddrSpaceCastRemover rem;
                rem.visit(F);
                rem.finalize(F);
                rewriteMetadata(*F);
            }

            if (auto TB = M.getFunction("tmp_barrier")) {
                TB->replaceAllUsesWith(barrier);
            }

            cleanupDeadCode(_M);
            for (auto& G : _M->getGlobalList()) {
                if (auto ptrT = dyn_cast<PointerType>(G.getType())) {
                    if (auto arrTy = dyn_cast<ArrayType>(ptrT->getElementType())) {
                        if (arrTy->getArrayNumElements() != 0)
                            G.setLinkage(llvm::GlobalValue::LinkageTypes::InternalLinkage);
                    }
                }
            }

            for (auto& F : M.getFunctionList()) {

                if (&F == barrier)
                    continue;

                AttributeSet attrs = F.getAttributes();
                AttributeSet new_attrs;
                new_attrs.addAttribute(M.getContext(), 0, Attribute::NoUnwind);
                int idx = 1;
                for (unsigned x = 0; x != attrs.getNumSlots(); ++x)
                    for (auto i = attrs.begin(x), e = attrs.end(x); i != e; ++i) {
                        if (i->isEnumAttribute()) {
                            switch (i->getKindAsEnum()) {
                            case Attribute::AlwaysInline:
                            case Attribute::InlineHint:
                            case Attribute::NoInline:
                            case Attribute::ReadNone:
                            case Attribute::ReadOnly:
                                new_attrs.addAttribute(M.getContext(), idx, i->getKindAsEnum());
                                idx++;
                                break;
                            default:
                                break;
                            }
                        }
                    }

                F.setAlignment(0);
                F.setAttributes(new_attrs);
                F.setLinkage(GlobalValue::LinkageTypes::ExternalLinkage);
                F.setVisibility(GlobalValue::VisibilityTypes::DefaultVisibility);
                F.setCallingConv(CallingConv::SPIR_FUNC);
            }
            VectorInstFixer fixer;
            for (auto& F : kernels) {
                fixer.visit(F);
                F->setCallingConv(CallingConv::SPIR_KERNEL);
            }

            vector<unsigned> inst_types;
            inst_types.push_back(Instruction::Load);
            inst_types.push_back(Instruction::GetElementPtr);
            inst_types.push_back(Instruction::BitCast);
            InstScheduler shed;
            for (auto op : inst_types) {
                for (auto F : kernels) {
                    shed.initialize(F);
                    shed.setValueId(op);
                    shed.visit(F);
                    shed.finalize(F);
                }
            }

            for (auto& F : M.getFunctionList()) {

                CastFixer bcf;
                bcf.visit(F);
                bcf.finalize();

                LoadFixer lf;
                lf.visit(F);
                lf.finalize();

                // PHIFixer pf;
                // pf.visit(F);
                // pf.finalize();

                GEPFixer gepf;
                gepf.visit(F);
                gepf.finalize();

                IntrinsicScheduler isced;
                isced.initialize();
                isced.visit(F);
                isced.finalize();

                bcf.visit(F);
                bcf.finalize();

                if (find(kernels.begin(), kernels.end(), &F) != kernels.end()) {
                    // SelectInstFixer sif;
                    // sif.visit(F);
                    // sif.finalize();
                }

                // FIXMEFixer FIXME;
                // FIXME.visit(F);
                // FIXME.finalize();
            }

            // Dumper d;
            // for (auto K : kernels)
            //{
            //	d.visit(K);
            //	d.finalize();
            //}

            return modified;
        }

      private:
        void rewriteMetadata(Function& F)
        {
            // the function has no body, skip it
            if (F.isDeclaration())
                return;

            // remove adress space casts from the original function
            // clone function with address spaces remapped
            /*  vector<int> ASMap = {0, 1};
              if (isAKernel)
                NF = cloner->clone(F, isAKernel, 1);
              else {
                for (int AS : ASMap) {
                  cloner->clone(F, isAKernel, AS);
                }
                return;
              }

              NF->setAttributes(F->getAttributes());

              assert(NF && "cloning of kernel failed!");
          */
            MDNode* kernelMD = nullptr;

            // update metadata nodes identifing the function as kernel
            auto& MDlist = _M->getNamedMDList();

            for (auto MI = MDlist.begin(), ME = MDlist.end(); MI != ME; ++MI) {
                if (MI->getName().equals("opencl.kernels")) {
                    for (unsigned i = 0; i != MI->getNumOperands(); ++i) {
                        auto* op = MI->getOperand(i);
                        if (dyn_cast<ValueAsMetadata>(op->getOperand(0).get())->getValue() == &F) {
                            kernelMD = op;
                        }
                    }
                }
            }

            if (kernelMD) {

                struct {
                    MDNode* addr_space;
                    MDNode* type;
                    MDNode* name;
                    MDNode* access_qual;
                    MDNode* base_type;
                    MDNode* type_qual;
                } kernel_arg;

                for (unsigned i = 0; i != kernelMD->getNumOperands(); ++i) {
                    if (MDNode* cur = dyn_cast<MDNode>(kernelMD->getOperand(i))) {

                        if (MDString* tag = dyn_cast<MDString>(cur->getOperand(0))) {
                            if (tag->getString().equals("kernel_arg_addr_space"))
                                kernel_arg.addr_space = cur;
                            if (tag->getString().equals("kernel_arg_type"))
                                kernel_arg.type = cur;
                            if (tag->getString().equals("kernel_arg_name"))
                                kernel_arg.name = cur;
                            if (tag->getString().equals("kernel_arg_access_qual"))
                                kernel_arg.access_qual = cur;
                            if (tag->getString().equals("kernel_arg_base_type"))
                                kernel_arg.base_type = cur;
                            if (tag->getString().equals("kernel_arg_type_qual"))
                                kernel_arg.type_qual = cur;
                        }
                    }
                }

                auto& Ctx = F.getContext();
                unsigned arg_c = 1;

                vector<Metadata *> ads, name, type, accqual, basetype, typequal;

                ads.push_back(kernel_arg.addr_space->getOperand(0));
                name.push_back(kernel_arg.name->getOperand(0));
                type.push_back(kernel_arg.type->getOperand(0));
                accqual.push_back(kernel_arg.access_qual->getOperand(0));
                basetype.push_back(kernel_arg.base_type->getOperand(0));
                typequal.push_back(kernel_arg.type_qual->getOperand(0));

                for (auto& arg : F.args()) {
                    ads.push_back(ValueAsMetadata::get(ConstantInt::get(
                        Type::getInt32Ty(Ctx), arg.getType()->getPointerAddressSpace())));
                    name.push_back(MDString::get(Ctx, arg.getName()));

                    string str = "unknown_type";

                    Type* t = arg.getType();
                    unsigned ind = 0;
                    while (t->isPointerTy()) {
                        t = t->getPointerElementType();
                        ++ind;
                    }
                    if (t == Type::getVoidTy(Ctx))
                        str = "void";
                    if (t == Type::getInt8Ty(Ctx))
                        str = "char";
                    if (t == Type::getInt32Ty(Ctx))
                        str = "int";
                    if (t == Type::getInt64Ty(Ctx))
                        str = "long";
                    if (t == Type::getInt128Ty(Ctx))
                        str = "long long";
                    if (t == Type::getFloatTy(Ctx))
                        str = "float";
                    if (t == Type::getDoubleTy(Ctx))
                        str = "double";
                    if (t->isStructTy())
                        str = t->getStructName();

                    for (unsigned i = 0; i < ind; ++i) {
                        str += "*";
                    }

                    type.push_back(MDString::get(Ctx, str));
                    accqual.push_back(MDString::get(Ctx, ""));
                    basetype.push_back(MDString::get(Ctx, ""));
                    typequal.push_back(MDString::get(Ctx, ""));

                    arg_c++;
                }

                kernel_arg.addr_space = MDNode::get(Ctx, ads);
                kernel_arg.name = MDNode::get(Ctx, name);
                kernel_arg.type = MDNode::get(Ctx, type);
                kernel_arg.base_type = MDNode::get(Ctx, basetype);
                kernel_arg.access_qual = MDNode::get(Ctx, accqual);
                kernel_arg.type_qual = MDNode::get(Ctx, typequal);

                for (unsigned i = 0; i != kernelMD->getNumOperands(); ++i) {
                    if (MDNode* cur = dyn_cast<MDNode>(kernelMD->getOperand(i))) {
                        if (cur->getNumOperands() != F.arg_size() + 1) {
                            if (MDString* tag = dyn_cast<MDString>(cur->getOperand(0))) {
                                if (tag->getString().equals("kernel_arg_addr_space"))
                                    kernelMD->replaceOperandWith(i, kernel_arg.addr_space);
                                if (tag->getString().equals("kernel_arg_type"))
                                    kernelMD->replaceOperandWith(i, kernel_arg.type);
                                if (tag->getString().equals("kernel_arg_name"))
                                    kernelMD->replaceOperandWith(i, kernel_arg.name);
                                if (tag->getString().equals("kernel_arg_access_qual"))
                                    kernelMD->replaceOperandWith(i, kernel_arg.access_qual);
                                if (tag->getString().equals("kernel_arg_base_type"))
                                    kernelMD->replaceOperandWith(i, kernel_arg.base_type);
                                if (tag->getString().equals("kernel_arg_type_qual"))
                                    kernelMD->replaceOperandWith(i, kernel_arg.type_qual);
                            }
                        }
                    }
                }
            }
        }

      private:
        class AddrSpaceCastRemover : public InstVisitor<AddrSpaceCastRemover>
        {
          public:
            AddrSpaceCastRemover() {}

            void visitAddrSpaceCastInst(AddrSpaceCastInst& I)
            {
                Instruction* repl = nullptr;
                if (I.getSrcTy()->getPointerAddressSpace() == 1 && I.getDestTy()->getPointerAddressSpace() == 0) {
                    auto newPTI = new PtrToIntInst(I.getOperand(0),
                                                   Type::getInt64Ty(I.getContext()), "", &I);
                    auto c0 = ConstantAsMetadata::get(
                        ConstantInt::get(Type::getInt64Ty(I.getContext()),
                                         I.getDestTy()->getPointerAddressSpace(), false));
                    vector<Metadata*> md;
                    md.push_back(c0);
                    newPTI->setMetadata("pacxx.addrspace", MDNode::get(I.getContext(), md));
                    repl = new IntToPtrInst(newPTI, I.getDestTy(), "", &I);
                }
                for (auto U : I.users()) {

                    if (auto GEP = dyn_cast<GetElementPtrInst>(U)) {

                        bool onlyLS = true;

                        for (auto GU : GEP->users()) {
                            if (!(isa<LoadInst>(GU) || isa<StoreInst>(GU))) {
                                onlyLS = false;
                                //__dump(*GU);
                            }
                        }

                        if (!onlyLS)
                            continue;

                        vector<Value*> idx(GEP->idx_begin(), GEP->idx_end());
                        auto newGEP = GetElementPtrInst::Create(
                            I.getSrcTy()->getPointerElementType(), I.getOperand(0), idx);
                        GEPMap[GEP] = newGEP;

                        for (auto GU : GEP->users()) {
                            if (auto LI = dyn_cast<LoadInst>(GU)) {
                                auto newLoad = new LoadInst(newGEP, "");
                                RMap[LI] = newLoad;
                            }
                            if (auto SI = dyn_cast<StoreInst>(GU)) {
                                auto newStore = new StoreInst(SI->getValueOperand(), newGEP);
                                RMap[SI] = newStore;
                            }
                        }
                    } 
                }
                if (repl) I.replaceAllUsesWith(repl); 
            }

            void visitLoadInst(LoadInst& I)
            {
                if (GEPOperator* GEP = dyn_cast<GEPOperator>(I.getOperand(0))) {
                    if (auto CASC = dyn_cast<ConstantExpr>(GEP->getPointerOperand())) {
                        if (auto IASC = dyn_cast<AddrSpaceCastInst>(CASC->getAsInstruction())) {
                            SmallVector<Value*, 8> Indices(GEP->idx_begin(), GEP->idx_end());

                            auto elemTy = IASC->getOperand(0)->getType()->getPointerElementType();

                            auto newGEP = GetElementPtrInst::Create(elemTy, IASC->getOperand(0),
                                                                    Indices, "", &I);
                            auto newLoad = new LoadInst(newGEP, "", &I);
                            VMap[&I] = newLoad;
                        }
                    }
                } else {
                    removeAddrSpaceCastFromInst(&I, 0);
                }
            }

            void visitStoreInst(StoreInst& I)
            {

                if (GEPOperator* GEP = dyn_cast<GEPOperator>(I.getOperand(1))) {
                    if (auto CASC = dyn_cast<ConstantExpr>(GEP->getPointerOperand())) {
                        if (auto IASC = dyn_cast<AddrSpaceCastInst>(CASC->getAsInstruction())) {
                            SmallVector<Value*, 8> Indices(GEP->idx_begin(), GEP->idx_end());

                            auto elemTy = IASC->getOperand(0)->getType()->getPointerElementType();

                            auto newGEP = GetElementPtrInst::Create(elemTy, IASC->getOperand(0),
                                                                    Indices, "", &I);
                            new StoreInst(I.getOperand(0), newGEP, &I);
                            dead.push_back(&I);
                        }
                    }
                } else {
                    removeAddrSpaceCastFromInst(&I, 1);
                }
            }

            void visitPtrToIntInst(PtrToIntInst& I) { removeAddrSpaceCastFromInst(&I, 0); }

            void visitCallInst(CallInst& I)
            {
                for (unsigned int i = 0; i != I.getNumOperands(); ++i) {
                    if (isa<GetElementPtrInst>(I.getOperand(i)))
                        removeAddrSpaceCastFromInst(&I, i);
                    if (AddrSpaceCastInst* asc = dyn_cast<AddrSpaceCastInst>(I.getOperand(i))) {
                        removeAddrSpaceCastFromInst(cast<Instruction>(I.getOperand(i)), 0);
                        if (asc->getType() ==
                            asc->getOperand(0)->getType()) // ASC is now a noop remove it
                        {
                            asc->replaceAllUsesWith(asc->getOperand(0));
                            asc->eraseFromParent();
                        }
                    }
                }
            }

            bool removeAddrSpaceCastFromGEP(GEPOperator* GEP)
            {
                Operator* Cast = dyn_cast<Operator>(GEP->getPointerOperand());
                if (!Cast)
                    return false;

                if (Cast->getOpcode() != Instruction::AddrSpaceCast)
                    return false;

                SmallVector<Value*, 8> Indices(GEP->idx_begin(), GEP->idx_end());

                if (Instruction* GEPI = dyn_cast<Instruction>(GEP)) {
                    GetElementPtrInst* NewGEPI = GetElementPtrInst::Create(
                        Cast->getOperand(0)->getType(), Cast->getOperand(0), Indices,
                        GEP->getName(), GEPI);
                    NewGEPI->setIsInBounds(GEP->isInBounds());
                    VMap[GEP] = new AddrSpaceCastInst(NewGEPI, GEP->getType(), "", GEPI);
                    dead.push_back(GEPI);
                } else {
                    Constant* NewGEPCE = ConstantExpr::getGetElementPtr(
                        Cast->getOperand(0)->getType(), cast<Constant>(Cast->getOperand(0)),
                        Indices, GEP->isInBounds());
                    VMap[GEP] = ConstantExpr::getAddrSpaceCast(NewGEPCE, GEP->getType());
                }

                return true;
            }

            void removeAddrSpaceCastFromInst(Instruction* I, unsigned Idx)
            {
                if (GEPOperator* GEP = dyn_cast<GEPOperator>(I->getOperand(Idx)))
                    removeAddrSpaceCastFromGEP(GEP);

                if (Operator* Cast = dyn_cast<Operator>(I->getOperand(Idx))) {
                    bool eleminate = false;
                    if (Cast->getOpcode() == Instruction::AddrSpaceCast)
                        eleminate = true;

                    if (eleminate) {
                        Value* Operand = Cast->getOperand(0);
                        PointerType* SrcTy = cast<PointerType>(Operand->getType());
                        PointerType* DestTy = cast<PointerType>(Cast->getType());
                        if (SrcTy->getElementType() != DestTy->getElementType()) {
                            BitCastInst* BitCast = new BitCastInst(
                                Operand,
                                DestTy->getElementType()->getPointerTo(SrcTy->getAddressSpace()),
                                "", I);
                            Operand = BitCast;
                        }

                        eleminate |= SrcTy->getAddressSpace() != DestTy->getAddressSpace();
                        if (eleminate)
                            I->setOperand(Idx, Operand);

                        if (auto* castI = dyn_cast<Instruction>(Cast)) {
                            if (castI->getNumUses() == 0)
                                dead.push_back(castI);
                        }
                    }
                }
            }

            void finalize(Function* F)
            {
                for (auto p : VMap) {
                    p.first->replaceAllUsesWith(p.second);
                    // dead.push_back(cast<Instruction>(p.first));
                }
                for (auto p : GEPMap) {
                    auto oldI = cast<Instruction>(p.first);
                    auto newI = cast<Instruction>(p.second);

                    newI->insertAfter(oldI);
                    oldI->replaceAllUsesWith(newI);
                }

                for (auto p : RMap) {
                    auto oldI = cast<Instruction>(p.first);
                    auto newI = cast<Instruction>(p.second);

                    newI->insertBefore(oldI);
                    if (isa<StoreInst>(oldI))
                        dead.push_back(oldI);
                    else
                        oldI->replaceAllUsesWith(newI);
                }

                for (auto& v : dead)
                    v->eraseFromParent();

                dead.clear();
                VMap.clear();
                RMap.clear();
                GEPMap.clear();
            }

          private:
            map<Value *, Value *> VMap, GEPMap, RMap;
            vector<Instruction*> dead;
        };

        class IntrinsicScheduler : public InstVisitor<IntrinsicScheduler>
        {
          public:
            IntrinsicScheduler() {}

            void initialize() { intrinsicClones.clear(); }

            void finalize()
            {

                for (auto p : repl) {
                    auto M = p.first->getParent()->getParent()->getParent();
                    SmallVector<Type*, 1> args;
                    args.push_back(Type::getFloatTy(M->getContext()));
                    FunctionType* FT =
                        FunctionType::get(Type::getFloatTy(M->getContext()), args, false);
                    auto F = M->getOrInsertFunction("_Z5rsqrtf", FT);

                    SmallVector<Value*, 1> param;
                    param.push_back(p.first->getOperand(0));

                    CallInst* newCall = CallInst::Create(F, param, "", p.first);
                    p.second->replaceAllUsesWith(newCall);
                    p.second->eraseFromParent();
                    p.first->eraseFromParent();
                }

                for (auto I : dead) {
                    I->eraseFromParent();
                }

                std::map<BasicBlock*, vector<CallInst*>> blocks;
                for (auto I : intrinsicClones) {

                    auto& clones = blocks[I.first->getParent()];
                    CallInst* clone = I.second;

                    auto lookup = find_if(clones.begin(), clones.end(), [=](auto cand) {
                        if (cand->getCalledFunction() == clone->getCalledFunction() &&
                            cand->getOperand(0) == clone->getOperand(0))
                            return true;
                        else
                            return false;
                    });

                    if (lookup != clones.end())
                        clone = *lookup;
                    else
                        clone = nullptr;

                    if (clone == nullptr) {
                        Instruction* firstI = nullptr;
                        for (auto& II : *I.first->getParent()) {
                            if (!isa<PHINode>(II)) {
                                firstI = &II;
                                break;
                            }
                        }

                        SmallVector<Value*, 1> args;
                        args.push_back(I.second->getOperand(0));
                        clone = CallInst::Create(I.second->getCalledFunction(), args, "", firstI);
                        clone->setTailCall(I.second->isTailCall());
                        blocks[I.first->getParent()].push_back(clone);
                    }

                    for (unsigned i = 0; i != I.first->getNumOperands(); ++i) {
                        auto op = I.first->getOperand(i);
                        if (op == I.second) {
                            I.first->setOperand(i, clone);
                        }
                    }
                }
            }

            void visitCallInst(CallInst& CI)
            {
                if (CI.isInlineAsm())
                    return;
                if (!isa<Function>(CI.getCalledValue())) {
                    return;
                }
                if (isaSregIntrinsic(CI.getCalledFunction())) {
                    if (CI.hasNUses(0)) {
                        dead.push_back(&CI);
                        return;
                    }
                    for (auto u : CI.users()) {
                        if (Instruction* I = dyn_cast<Instruction>(u)) {
                            // clone them if they are not in the same basic block
                            if (!isa<PHINode>(I) && I->getParent() != CI.getParent()) {
                                intrinsicClones.push_back(make_pair(I, &CI));
                            }
                        }
                    }
                } else if (CI.getCalledFunction() ||
                           CI.getCalledFunction()->getName().find("sqrt") != StringRef::npos) {
                    bool valid = CI.hasNUses(1);
                    // REFACTOR: 1.0f / sqrt -> rsqrt optimiation
                    if (valid) {
                        for (auto u : CI.users()) {
                            if (BinaryOperator* bi = dyn_cast<BinaryOperator>(u)) {
                                if (bi->getOpcode() == BinaryOperator::FDiv) {
                                    if (bi->getOperand(1) == &CI) {
                                        if (ConstantFP* div =
                                                dyn_cast<ConstantFP>(bi->getOperand(0))) {
                                            //__debug(div->getValueAPF().convertToFloat(), " - ",
                                            //        1.0f);
                                            if (div->getValueAPF().convertToFloat() == 1.0f) {
                                                repl.push_back(make_pair(&CI, bi));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    CI.getCalledFunction()->addFnAttr(Attribute::NoUnwind);
                    CI.getCalledFunction()->addFnAttr(Attribute::ReadNone);
                }
            }

            bool isaSregIntrinsic(Function* F)
            {
                if (!F)
                    return false;
                bool found = false;
                auto name = F->getName();
                if (name.find("get_local_id") != StringRef::npos)
                    found = true;
                if (name.find("get_global_id") != StringRef::npos)
                    found = true;
                if (name.find("get_local_size") != StringRef::npos)
                    found = true;
                if (name.find("get_group_id") != StringRef::npos)
                    found = true;
                if (name.find("get_group_size") != StringRef::npos)
                    found = true;

                if (found) {
                    F->addFnAttr(Attribute::NoUnwind);
                    F->addFnAttr(Attribute::ReadNone);
                    return true;
                }
                return false;
            }

          private:
            vector<pair<Instruction*, CallInst*>> intrinsicClones;
            vector<CallInst*> dead;
            vector<pair<CallInst*, BinaryOperator*>> repl;
        };

        class FMAOpt : public InstVisitor<FMAOpt>
        {
          public:
            FMAOpt(Module* module) : M(module) {}

            void visitFMul(BinaryOperator& BI)
            {
                if (BI.hasNUses(1)) {

                    for (auto use : BI.users()) {
                        if (BinaryOperator* UBI = dyn_cast<BinaryOperator>(use)) {
                            if (UBI->getOpcode() == BinaryOperator::FAdd) {

                                if (isa<ExtractElementInst>(UBI->getOperand(0)) ||
                                    isa<ExtractElementInst>(UBI->getOperand(1)))
                                    return;

                                vector<Value*> args;
                                args.push_back(BI.getOperand(0));
                                args.push_back(BI.getOperand(1));

                                if (UBI->getOperand(0) == &BI) {
                                    args.push_back(UBI->getOperand(1));
                                } else {
                                    args.push_back(UBI->getOperand(0));
                                }

                                SmallVector<Type*, 1> targ;
                                targ.push_back(BI.getOperand(0)->getType());

                                Function* fma =
                                    Intrinsic::getDeclaration(M, Intrinsic::fmuladd, targ);

                                CallInst* fc = CallInst::Create(fma, args, "");
                                fc->setTailCall(true);
                                reps.push_back(
                                    make_pair(cast<Instruction>(UBI), cast<Instruction>(fc)));
                                dead.push_back(&BI);
                            }
                        }
                    }
                }
            }

            void finalize()
            {

                for (auto& p : reps) {
                    ReplaceInstWithInst(p.first, p.second);
                }

                for (auto& d : dead) {
                    d->replaceAllUsesWith(UndefValue::get(d->getType()));
                    d->eraseFromParent();
                }
            }

          private:
            vector<pair<Instruction*, Instruction*>> reps;
            vector<Instruction*> dead;
            Module* M;
        };

        class PHIFixer : public InstVisitor<PHIFixer>
        {
          public:
            PHIFixer() {}

            void visitPHINode(PHINode& I)
            {
                bool invalid = false;
                unsigned AS = 0;
                for (unsigned i = 0; i < I.getNumIncomingValues(); ++i) {
                    auto V = I.getIncomingValue(i);
                    if (V->getType() != I.getType()) {
                        invalid = true;

                        if (auto PTy = dyn_cast<PointerType>(V->getType())) {
                            if (AS != PTy->getAddressSpace()) {
                                AS = PTy->getAddressSpace();
                                break;
                            }
                        }
                    }
                }

                if (invalid) {
                    I.mutateType(I.getType()->getPointerElementType()->getPointerTo(AS));
                }
            }

            void finalize()
            {

                for (auto& p : reps) {
                    ReplaceInstWithInst(p.first, p.second);
                }
            }

          private:
            vector<pair<Instruction*, Instruction*>> reps;
        };

        class LoadFixer : public InstVisitor<LoadFixer>
        {
          public:
            LoadFixer() {}

            void visitLoadInst(LoadInst& I)
            {
                auto AS = I.getPointerAddressSpace();
                if (AS > 0) {
                    if (I.getType()->isPointerTy())
                        if (I.getType()->getPointerAddressSpace() !=
                            I.getPointerOperand()->getType()->getPointerAddressSpace()) {
                            auto NI = new LoadInst(
                                I.getPointerOperand()->getType()->getPointerElementType(),
                                I.getPointerOperand(), "", I.isVolatile(), I.getAlignment());
                            reps.push_back(make_pair(&I, NI));
                        }
                }
            }

            void finalize()
            {

                for (auto& p : reps) {
                    ReplaceInstWithInst(p.first, p.second);
                }
            }

          private:
            vector<pair<Instruction*, Instruction*>> reps;
        };

        class GEPFixer : public InstVisitor<GEPFixer>
        {
          public:
            GEPFixer() {}

            void visitGetElementPtrInst(GetElementPtrInst& I)
            {
                if (I.getType()->isPointerTy()) {
                    auto AS = I.getAddressSpace();
                    if (AS != 0) {
                        SmallVector<Value*, 3> args;
                        for (auto it = I.idx_begin(), end = I.idx_end(); it != end; ++it)
                            args.push_back(*it);
                        reps.push_back(
                            make_pair(&I, GetElementPtrInst::Create(
                                              I.getPointerOperandType()->getPointerElementType(),
                                              I.getPointerOperand(), args, "", &I)));
                    }
                }
            }

            void finalize()
            {

                for (auto& p : reps) {
                    ReplaceUnsafe<Instruction>(p.first, p.second);
                }
            }

          private:
            vector<pair<Instruction*, Instruction*>> reps;
        };

        class CastFixer : public InstVisitor<CastFixer>
        {
          public:
            CastFixer() {}

            void visitBitCastInst(BitCastInst& I)
            {
                if (I.hasNUses(0)) {
                    dead.push_back(&I);
                    return;
                }

                if (!CastInst::castIsValid(Instruction::BitCast, I.getOperand(0), I.getType())) {
                    auto nC =
                        CastInst::Create(Instruction::BitCast, I.getOperand(0),
                                         I.getDestTy()->getPointerElementType()->getPointerTo(
                                             I.getOperand(0)->getType()->getPointerAddressSpace()),
                                         "", &I);
                    reps.push_back(make_pair(&I, nC));
                }
            }

            void visitAddrSpaceCastInst(AddrSpaceCastInst& ASCI)
            {
                if (!CastInst::castIsValid(Instruction::AddrSpaceCast, ASCI.getOperand(0),
                                           ASCI.getType())) {
                    ASCI.replaceAllUsesWith(ASCI.getOperand(0));
                    dead.push_back(&ASCI);
                }
            }

            void finalize()
            {

                for (auto& p : reps) {
                    ReplaceUnsafe<Instruction>(p.first, p.second);
                }

                for (auto& I : dead)
                    I->eraseFromParent();

                reps.clear();
                dead.clear();
            }

          private:
            vector<pair<Instruction*, Instruction*>> reps;
            vector<Instruction*> dead;
        };

        class InstScheduler : public InstVisitor<InstScheduler>
        {
          public:
            InstScheduler() : ID(0), direction(0), DT() {}

            void initialize(Function* F)
            {
                candidates.clear();

                if (F->isDeclaration())
                    return;
                DT.recalculate(*F);
            }
            void setDirection(int v) { direction = v; }
            void setValueId(unsigned id) { ID = id; }

            int getDist(Instruction* begin, Instruction* end, Module* M)
            {

                const auto& FList = M->getFunctionList();
                int dist = 0;

                bool found = false;
                for (const auto& F : FList) {
                    const auto& BBList = F.getBasicBlockList();
                    for (const auto& BB : BBList) {
                        const auto& IList = BB.getInstList();
                        for (const auto& I : IList) {
                            if (&I == end) {
                                return dist;
                            }
                            if (found)
                                ++dist;
                            if (&I == begin)
                                found = true;
                        }
                    }
                }
                return -1;
            }

            void checkForReschedule(Instruction* I)
            {

                if (I->getOpcode() != ID)
                    return;

                if (direction == 1) {
                    Function* F = I->getParent()->getParent();

                    for (const auto& arg : F->args())
                        if (I->getOperand(0) == &arg) {
                            candidates.push_back(make_pair(I, nullptr));
                            break;
                        }
                    return;
                }

                int dist = INT32_MAX;
                Instruction* target = nullptr;
                for (auto user : I->users()) {
                    if (isa<PHINode>(user) ||
                        (isa<LoadInst>(I) &&
                         isa<StoreInst>(
                             user))) // discard if we try to move a load directly to a store
                        return;      // discard Insts that have a PHINode as user
                    auto cand = dyn_cast<Instruction>(user);
                    int d = getDist(I, cand, I->getParent()->getParent()->getParent());

                    if (d >= 0 && d < dist) {
                        dist = d;
                        target = cand;
                    }
                }

                if (target) {
                    candidates.push_back(make_pair(I, target));
                }
            }

            void visitInstruction(Instruction& I) { checkForReschedule(&I); }

            void finalize(Function* F)
            {
                for (auto I : candidates) {
                    if (direction == 0) {
                        auto oldSuccessor = I.first->getNextNode();
                        I.first->moveBefore(I.second);
                        DT.recalculate(*F);
                        bool isSave = true;
                        for (auto u : I.first->users()) {

                            isSave &= DT.dominates(I.first, cast<Instruction>(u));
                        }

                        if (!isSave) {
                            I.first->moveBefore(oldSuccessor);
                        }

                        moved.push_back(I.first);
                    } else {
                        BasicBlock* BB = I.first->getParent();
                        I.first->moveBefore(&BB->getInstList().front());
                    }
                }

                for (auto I : bit) {
                    I.first->moveBefore(&I.second->getInstList().front());
                }
                for (auto I : idx) {
                    I.first->removeFromParent();
                    auto term = &I.second->getInstList().back();
                    term->removeFromParent();
                    I.second->getInstList().push_back(I.first);
                    I.second->getInstList().push_back(term);
                }
                for (auto I : gep) {
                    I.first->removeFromParent();
                    auto term = &I.second->getInstList().back();
                    term->removeFromParent();
                    I.second->getInstList().push_back(I.first);
                    I.second->getInstList().push_back(term);
                }
            }

            bool hasCandidates() { return candidates.size() > 0; }

          private:
            vector<pair<Instruction*, Instruction*>> candidates;
            vector<pair<Instruction*, BasicBlock*>> idx;
            vector<pair<Instruction*, BasicBlock*>> gep;
            vector<pair<Instruction*, BasicBlock*>> bit;
            vector<Instruction*> moved;
            unsigned ID;
            int direction;

            DominatorTree DT;
        };

        class VectorInstFixer : public InstVisitor<VectorInstFixer>
        {
          public:
            void visitInsertElementInst(InsertElementInst& IEI)
            {
                auto index = ConstantInt::get(
                    Type::getInt32Ty(IEI.getContext()),
                    (int)*(cast<ConstantInt>(IEI.getOperand(2))->getValue().getRawData()));
                IEI.setOperand(2, index);
            }

            void visitExtractElementInst(ExtractElementInst& EEI)
            {
                auto index = ConstantInt::get(
                    Type::getInt32Ty(EEI.getContext()),
                    (int)*(cast<ConstantInt>(EEI.getOperand(1))->getValue().getRawData()));

                EEI.setOperand(1, index);
            }
        };

        class SelectInstFixer : public InstVisitor<SelectInstFixer>
        {
          public:
            struct CmpDesc {
                CmpInst* ptr;
                bool desc;
            };

            void visitSelectInst(SelectInst& SI)
            {

                auto op1 = SI.getOperand(1);
                auto op2 = SI.getOperand(2);
                if (!op1->getType()->isPointerTy())
                    return;

                /*      if (op1->getType()->getPointerAddressSpace() !=
          op2->getType()->getPointerAddressSpace()) */ {
                    Value* true_address = nullptr;
                    bool desc = false;
                    if (op1->getType()->getPointerAddressSpace() == 1) {
                        true_address = op1;
                        desc = true;
                    } else
                        true_address = op2;

                    for (auto U : SI.users()) {
                        auto cmp = dyn_cast<CmpInst>(SI.getOperand(0));
                        if (auto LI = dyn_cast<LoadInst>(U)) {
                            dead.insert(&SI);
                            loads.push_back(pair<LoadInst*, CmpDesc>(LI, {cmp, desc}));
                        } else if (auto StI = dyn_cast<StoreInst>(U)) {
                            dead.insert(&SI);
                            stores.push_back(pair<StoreInst*, CmpDesc>(StI, {cmp, desc}));
                        } //else {
                            //__dump(*U);
                       // }
                    }

                    SI.replaceAllUsesWith(true_address);
                }
            }

            void finalize()
            {
                for (auto I : dead)
                    I->eraseFromParent();

                for (auto p : stores) {
                    auto oldBB = p.first->getParent();
                    auto SBB = oldBB->splitBasicBlock(p.first);
                    auto I = SBB->getInstList().begin();
                    ++I;
                    auto newBB = SBB->splitBasicBlock(I);

                    auto TI = oldBB->getTerminator();

                    BasicBlock* ifTrue = nullptr;
                    BasicBlock* ifFalse = nullptr;

                    if (p.second.desc) {
                        ifTrue = SBB;
                        ifFalse = newBB;
                    } else {
                        ifTrue = newBB;
                        ifFalse = SBB;
                    }

                    BranchInst::Create(ifTrue, ifFalse, p.second.ptr, TI);
                    TI->eraseFromParent();
                }

                for (auto p : loads) {

                    map<BasicBlock*, PHINode*> PHIMap;

                    auto oldBB = p.first->getParent();
                    auto SBB = oldBB->splitBasicBlock(p.first);
                    auto I = SBB->getInstList().begin();
                    ++I;
                    auto newBB = SBB->splitBasicBlock(I);

                    Value* C = nullptr;
                    if (p.first->getType()->isIntegerTy())
                        C = ConstantInt::get(p.first->getType(), 0);
                    else if (p.first->getType()->isFloatingPointTy())
                        C = ConstantFP::get(p.first->getType(), 0);
                    else
                        C = UndefValue::get(p.first->getType());

                    Instruction* IS = &(*newBB->getInstList().begin());
                    auto phi = PHINode::Create(p.first->getType(), 2, "", IS);
                    phi->addIncoming(p.first, SBB);
                    phi->addIncoming(C, oldBB);

                    PHIMap[newBB] = phi;

                    vector<pair<User*, unsigned>> repls;

                    for (auto U : p.first->users()) {
                        if (U != phi) {
                            for (unsigned i = 0; i < U->getNumOperands(); ++i) {
                                if (U->getOperand(i) == p.first)
                                    repls.push_back(pair<User*, unsigned>(U, i));
                            }
                        }
                    }

                    for (auto p : repls) {
                        p.first->setOperand(p.second, phi);
                    }

                    auto TI = oldBB->getTerminator();
                    BasicBlock* ifTrue = nullptr;
                    BasicBlock* ifFalse = nullptr;

                    if (p.second.desc) {
                        ifTrue = SBB;
                        ifFalse = newBB;
                    } else {
                        ifTrue = newBB;
                        ifFalse = SBB;
                    }

                    BranchInst::Create(ifTrue, ifFalse, p.second.ptr, TI);
                    TI->eraseFromParent();
                }
            }

          private:
            vector<pair<Instruction*, Instruction*>> insert_after;
            set<Instruction*> dead;
            vector<pair<StoreInst*, CmpDesc>> stores;
            vector<pair<LoadInst*, CmpDesc>> loads;
        };

        class MemIntrinsicFixer : public InstVisitor<MemIntrinsicFixer>
        {
          public:
            void visitCallInst(CallInst& CI)
            {
                auto F = CI.getCalledFunction();
                if (F && F->isIntrinsic()) {
                    auto id = F->getIntrinsicID();
                    if (id == Intrinsic::ID::lifetime_start || id == Intrinsic::ID::lifetime_end) {
                        dead.push_back(&CI);
                    }

                    if (id == Intrinsic::ID::memcpy) {
                        // CI.dump();
                    }
                }
            }

            void finalize()
            {
                for (auto I : dead) {
                    I->eraseFromParent();
                }
            }

          private:
            vector<Instruction*> dead;
        };

        class FIXMEFixer : public InstVisitor<FIXMEFixer>
        {
          public:
            void visitStoreInst(StoreInst& I)
            {
                auto PTy = dyn_cast<PointerType>(I.getOperand(1)->getType());
                auto ElTy = PTy->getElementType();
                if (auto VTy = dyn_cast<PointerType>(I.getOperand(0)->getType())) {

                    if (ElTy != VTy) {
                        //__verbose("FIXME: Replacing StoreInst");
                        //__dump(I);
                        auto ASC = AddrSpaceCastInst::CreatePointerBitCastOrAddrSpaceCast(
                            I.getValueOperand(), ElTy);
                        I.setOperand(0, ASC);
                        insert.push_back(pair<Instruction*, Instruction*>(&I, ASC));
                    }
                }
            }

            void visitInsertValueInst(InsertValueInst& I)
            {
                auto ExTy = ExtractValueInst::getIndexedType(I.getAggregateOperand()->getType(),
                                                             I.getIndices());
                if (ExTy != I.getOperand(1)->getType()) {
                    //__verbose("FIXME: Replacing InsertValueInst");
                    //__dump(I);
                    auto ASC = AddrSpaceCastInst::CreatePointerBitCastOrAddrSpaceCast(
                        I.getOperand(1), ExTy);
                    I.setOperand(1, ASC);
                    insert.push_back(pair<Instruction*, Instruction*>(&I, ASC));
                }
            }

            void visitICmpInst(ICmpInst& IC)
            {
                PointerType* Op0Ty = dyn_cast<PointerType>(IC.getOperand(0)->getType());
                PointerType* Op1Ty = dyn_cast<PointerType>(IC.getOperand(1)->getType());
                if (Op0Ty && Op1Ty && Op0Ty != Op1Ty) {
                    //__verbose("FIXME: Replacing ICmpInst Ops with PtrToInt");
                    //__dump(IC);
                    auto PTI0 =
                        PtrToIntInst::Create(Instruction::CastOps::PtrToInt, IC.getOperand(0),
                                             Type::getInt64Ty(IC.getContext()));
                    auto PTI1 =
                        PtrToIntInst::Create(Instruction::CastOps::PtrToInt, IC.getOperand(1),
                                             Type::getInt64Ty(IC.getContext()));
                    insert.push_back(pair<Instruction*, Instruction*>(&IC, PTI0));
                    insert.push_back(pair<Instruction*, Instruction*>(&IC, PTI1));
                    IC.setOperand(0, PTI0);
                    IC.setOperand(1, PTI1);
                }
            }

            void visitPHINode(PHINode& PN)
            {
                bool mutate = false;
                Type* incTy = nullptr;
                for (Value* IncValue : PN.incoming_values()) {

                    if (incTy && incTy != IncValue->getType())
                        return; // FIXME: just fail on different inc types
                    incTy = IncValue->getType();
                    if (PN.getType() != IncValue->getType()) {
                        //__verbose("FIXME: Mutating PHINode Type");
                        //__dump(PN);
                        mutate = true;
                    }
                }
                if (incTy)
                    PN.mutateType(incTy);
            }

            void finalize()
            {
                for (auto p : insert)
                    p.second->insertBefore(p.first);
                insert.clear();
            }

            vector<pair<Instruction*, Instruction*>> insert;
        };

        class Dumper : public InstVisitor<Dumper>
        {
          public:
            // void visitAllocaInst(AllocaInst &I) {
            // return;
            // __verbose("--------------------------------------------------------------------");
            // followUsers(&I, 0);
            // __verbose("--------------------------------------------------------------------");
            //}

            void visitStoreInst(StoreInst& I)
            {
                auto F = I.getParent()->getParent();
                for (const auto& A : F->args()) {
                    if (&A == I.getValueOperand()) {
                        //__dump(I);
                        stores[I.getPointerOperand()] = &I;
                    }
                }
            }

            map<Value*, Value*> stores;

            string toString(GetElementPtrInst& GEP)
            {
                vector<int> id;
                for (auto I = GEP.idx_begin(), E = GEP.idx_end(); I != E; ++I) {
                    if (auto CI = dyn_cast<ConstantInt>(I))
                        id.push_back(*CI->getValue().getRawData());
                    else
                        return string();
                }

                string ids;
                for (auto i : id)
                    ids += to_string(i);

                return ids;
            }

            void visitGetElementPtrInst(GetElementPtrInst& GEP)
            {
                auto ids = toString(GEP);
                if (!ids.empty())
                    geps[ids].push_back(&GEP);
            }

            // void followUsers(Instruction* I, unsigned level)
            //{
            // if (!I)
            //  return;
            // if (isa<PHINode>(I)) return;
            // if (isa<StoreInst>(I) || isa<LoadInst>(I) || isa<GetElementPtrInst>(I) ||
            // level == 1)
            // __dump(*I);

            // if (isa<GetElementPtrInst>(I) && level == 0)
            // {
            //  vector<int> id;
            //  auto GEP = cast<GetElementPtrInst>(I);
            //  for (auto I = GEP->idx_begin(), E = GEP->idx_end(); I != E; ++I)
            //  {
            //	  if (auto CI = dyn_cast<ConstantInt>(I))
            //		  id.push_back(*CI->getValue().getRawData()) ;
            //	  else
            //		  id.push_back(-7);
            //  }

            //  string ids;
            //  for (auto i : id)
            //	  ids += to_string(i);

            //  geps[ids].push_back(GEP);
            // }

            // for (auto U : I->users())
            //  followUsers(dyn_cast<Instruction>(U), level);
            //}

            void finalize()
            {
                for (const auto& id : geps) {
                    //__error(id.first);
                    for (auto gep : id.second) {
                        for (auto U : gep->users()) {

                            if (isa<LoadInst>(U)) {
                                if (stores[gep]) {
                                    //__warning("matching gep");
                                    //__dump(*stores[gep]);
                                } else {
                                    for (auto p : stores) {
                                        if (auto cand = dyn_cast<GetElementPtrInst>(p.first)) {
                                            auto ids = toString(*cand);
                                            for (auto& cand2 : geps[ids]) {
                                                if (cand2 != cand)
                                                    if (cand2->getPointerOperand() ==
                                                        cand->getPointerOperand()) {
                                                        for (auto U : cand2->users()) {
                                                            if (isa<LoadInst>(U)) {
                                                                //__warning("found 2 metching geps");
                                                                //__dump(*cand);
                                                                //__dump(*cand2);

                                                                //__dump(*stores[cand2]);
                                                                //__warning("____");
                                                            }
                                                        }
                                                    }
                                            }
                                        }
                                    }
                                }
                                //__dump(*U);
                                //__dump(*gep);
                            }
                        }
                    }
                }
            }

            map<string, vector<GetElementPtrInst*>> geps;
            vector<vector<int>> idx;
        };

        set<Function*> kernels;
        Module* _M;
    };

    char SPIRPass::ID = 0;
    static RegisterPass<SPIRPass> X("spir", "LLVM to SPIR IR pass", false, false);
}

namespace llvm
{
    Pass* createPACXXSpirPass() { return new SPIRPass(); }
}
