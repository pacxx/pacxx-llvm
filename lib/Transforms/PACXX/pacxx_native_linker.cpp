// Created by Michael Haidl and lars

#define PACXX_PASS_NAME "PACXXNativeLinker"
#include "Log.h"
#include "pacxx_sm_pass.h"
#include "ModuleHelper.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "../../IR/ConstantsContext.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace llvm {

    struct PACXXNativeLinker : public ModulePass {
        static char ID;

        PACXXNativeLinker() : ModulePass(ID) {}

        virtual ~PACXXNativeLinker() {}

        virtual bool runOnModule(Module &M);

    private:
      void createSharedMemoryBuffer(Module &M, Function * wrapper, Value *sm_size);
      void transformConstExprToInst(Function *wrapper);
      void recursiveTransformConstExprToInst(Instruction *inst, Instruction *insertBefore);
      void createInternalSharedMemoryBuffer(Module &M, set<GlobalVariable*> &globals, Function *wrapper,
                                            BasicBlock *sharedMemBB);
      void createExternalSharedMemoryBuffer(Module &M, set<GlobalVariable*> &globals, Function *wrapper,
                                            Value *sm_size, BasicBlock *sharedMemBB);
      void handleExternalPtr(Module &M, Function *wrapper, GlobalVariable *GV,
                             Value *sm_size, BasicBlock *sharedMemBB);
      void handleExternalArray(Module &M, Function *wrapper,
                               GlobalVariable *GV, Value *sm_size, BasicBlock *sharedMemBB);


      set<GlobalVariable *> getSMGlobalsUsedByKernel(Module &M, Function *kernel, bool internal);

    private:
        bool _barrier;
        bool _vectorized;
    };

    bool PACXXNativeLinker::runOnModule(Module &M) {

      auto &ctx = M.getContext();
      auto origFoo = M.getFunction("foo");
      auto foo = origFoo;
      auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

      for (auto &F : kernels) {

        _barrier = false;
        _vectorized = false;

        // if the kernel has been vectorized
        if (F->hasFnAttribute("vectorized")) {
          _vectorized = true;
          foo = M.getFunction("__vectorized__foo__" + F->getName().str());
        }
        // if the kernel has a barrier use a special foo function
        if (F->hasFnAttribute("barrier")) {
          _barrier = true;
          foo = M.getFunction("__barrier__foo__" + F->getName().str());
        }

        SmallVector<Type *, 8> Params;
        //block id
        Params.push_back(IntegerType::getInt32Ty(ctx));
        Params.push_back(IntegerType::getInt32Ty(ctx));
        Params.push_back(IntegerType::getInt32Ty(ctx));

        // max blocks
        Params.push_back(IntegerType::getInt32Ty(ctx));
        Params.push_back(IntegerType::getInt32Ty(ctx));
        Params.push_back(IntegerType::getInt32Ty(ctx));

        // max threads
        Params.push_back(IntegerType::getInt32Ty(ctx));
        Params.push_back(IntegerType::getInt32Ty(ctx));
        Params.push_back(IntegerType::getInt32Ty(ctx));

        //shared memory
        Params.push_back(IntegerType::getInt32Ty(ctx));

        //arguments
        Params.push_back(PointerType::getInt8PtrTy(ctx));

        // we can always use void, because we work with valid cuda kernels and they need to return void
        FunctionType *FTy = FunctionType::get(Type::getVoidTy(ctx), Params, false);
        auto wrappedF =
                Function::Create(FTy, GlobalValue::LinkageTypes::ExternalLinkage,
                                 std::string("__wrapped__") + F->getName().str(), &M);

        auto argIt = wrappedF->arg_begin();
        (argIt++)->setName("bidx");
        (argIt++)->setName("bidy");
        (argIt++)->setName("bidz");

        (argIt++)->setName("maxblockx");
        (argIt++)->setName("maxblocky");
        (argIt++)->setName("maxblockz");

        SmallVector<Value *, 1> args;
        args.push_back(&*argIt);
        (argIt++)->setName("maxidx");
        args.push_back(&*argIt);
        (argIt++)->setName("maxidy");
        args.push_back(&*argIt);
        (argIt++)->setName("maxidz");

        (argIt++)->setName("sm_size");

        (argIt++)->setName("args");

        BasicBlock *entry = BasicBlock::Create(ctx, "entry", wrappedF);

        auto term = ReturnInst::Create(ctx, entry);
        auto CI = CallInst::Create(foo, args, "", term);

        // now that we have a call inst inline it!
        InlineFunctionInfo IFI;
        InlineFunction(CI, IFI, nullptr, false);

        AllocaInst *idx = nullptr, *idy = nullptr, *idz = nullptr;

        for (auto &B : *wrappedF) {
          for (auto &I : B) {
            if (auto *alloca = dyn_cast<AllocaInst>(&I)) {
              if (alloca->getName().startswith_lower("__x") && !idx)
                idx = alloca;
              else if (alloca->getName().startswith_lower("__y") && !idy)
                idy = alloca;
              else if (alloca->getName().startswith_lower("__z") && !idz)
                idz = alloca;
            }
          }
        }

        // find the dummy kernel call and replace it with a call to the actual
        // kernel
        SmallVector<Value *, 8> kernel_args;
        Value *bidx = nullptr, *bidy = nullptr, *bidz = nullptr;
        Value *maxidx = nullptr, *maxidy = nullptr, *maxidz = nullptr;
        Value *maxblockx = nullptr, *maxblocky = nullptr, *maxblockz = nullptr;
        Value *sm_size = nullptr;
        Function *seq_dummy = M.getFunction("__dummy_kernel");
        Function *vec_dummy = M.getFunction("__vectorized__dummy_kernel");
        Function *dummy = seq_dummy;

        CallInst *remove = nullptr;
        for (auto U : dummy->users()) {
          if ((CI = dyn_cast<CallInst>(U))) {
            if (CI->getParent()->getParent() == wrappedF) {
              auto argIt = wrappedF->getArgumentList().begin();
              //blockids
              bidx = &*argIt;
              ++argIt;
              bidy = &*argIt;
              ++argIt;
              bidz = &*argIt;
              ++argIt;
              // max blocks
              maxblockx = &*argIt;
              ++argIt;
              maxblocky = &*argIt;
              ++argIt;
              maxblockz = &*argIt;
              ++argIt;
              //max threads
              maxidx = &*argIt;
              ++argIt;
              maxidy = &*argIt;
              ++argIt;
              maxidz = &*argIt;
              ++argIt;
              //shared memory
              sm_size = &*argIt;
              ++argIt;

              // construct the kernel arguments from the char*
              BasicBlock *constructKernelArgs = BasicBlock::Create(ctx, "constructArgs", wrappedF, entry);


              auto int8ptr_type = Type::getInt8PtrTy(ctx);
              auto int8ptr_align = M.getDataLayout().getPrefTypeAlignment(int8ptr_type);

              auto *alloc_args = new AllocaInst(int8ptr_type, nullptr, int8ptr_align, (&*argIt)->getName(),
                                                constructKernelArgs);
              new StoreInst(&*argIt, alloc_args, false, int8ptr_align, constructKernelArgs);
              auto *args_load = new LoadInst(alloc_args, "args", false, int8ptr_align, constructKernelArgs);

              size_t offset = 0;

              for (auto &A : F->getArgumentList()) {

                if (A.getName() == "idx") {
                  kernel_args.push_back(new LoadInst(idx, "idx", CI));
                  continue;
                } else if (A.getName() == "idy") {
                  kernel_args.push_back(new LoadInst(idy, "idy", CI));
                  continue;
                } else if (A.getName() == "idz") {
                  kernel_args.push_back(new LoadInst(idz, "idz", CI));
                  continue;
                } else if (A.getName() == "native.struct") {
                  //ignore next mem because we already set it in the barrier pass
                  continue;
                }

                auto arg_size = M.getDataLayout().getTypeAllocSize(A.getType());
                auto arg_alignment = M.getDataLayout().getPrefTypeAlignment(A.getType());

                auto arg_offset = (offset + arg_alignment - 1) & ~(arg_alignment - 1);
                // consider offset in char array
                auto *elem_ptr = GetElementPtrInst::CreateInBounds(Type::getInt8Ty(ctx), args_load,
                                                                   ConstantInt::get(Type::getInt64Ty(ctx), arg_offset),
                                                                   "", constructKernelArgs);
                // Cast to apropriate size
                auto *casted = new BitCastInst(elem_ptr, PointerType::getUnqual(A.getType()), "", constructKernelArgs);
                auto *casted_load = new LoadInst(casted, A.getName(), false,
                                                 M.getDataLayout().getPrefTypeAlignment(casted->getType()),
                                                 constructKernelArgs);

                kernel_args.push_back(casted_load);

                offset = arg_offset + arg_size;
              }

              // branch to normal entry block
              llvm::BranchInst::Create(entry, constructKernelArgs);

              remove = CI;
              if (_barrier) {
                CallInst *actual_call = dyn_cast<CallInst>(&*(--remove->getIterator()));
                assert(actual_call && "cant find actual call");
                CI = actual_call;
                for (unsigned i = 0; i < kernel_args.size(); ++i)
                  CI->setArgOperand(i, kernel_args[i]);
              } else if (seq_dummy)
                CI = CallInst::Create(F, kernel_args, "", CI);
              else
                CI = CallInst::Create(M.getFunction("__vectorized__" + F->getName().str()), kernel_args, "", CI);
              break;
            }
          }
        }

        if (remove)
          remove->eraseFromParent();

        // time to inline the original kernel into the wrapper
        __verbose("Inline kernel \n");
        InlineFunction(CI, IFI, nullptr, false);

        //If a vectorized version and a sequential version of the kernel exists
        // the vectorized version also needs to be inlined
        if (_vectorized) {
          CallInst *remove_vec = nullptr;
          Function *vec_kernel = M.getFunction("__vectorized__" + F->getName().str());
          for (auto U : vec_dummy->users()) {
            if ((CI = dyn_cast<CallInst>(U))) {
              if (CI->getParent()->getParent() == wrappedF) {
                remove_vec = CI;

                if (_barrier) {
                  CallInst *actual_call = dyn_cast<CallInst>(&*(--remove_vec->getIterator()));
                  assert(actual_call && "cant find actual call");
                  CI = actual_call;
                  for (unsigned i = 0; i < kernel_args.size(); ++i)
                    CI->setArgOperand(i, kernel_args[i]);
                } else
                  CI = CallInst::Create(vec_kernel, kernel_args, "", CI);

                break;
              }
            }
          }

          if (remove_vec)
            remove_vec->eraseFromParent();

          InlineFunction(CI, IFI, nullptr, false);
        }


        __verbose("replacing dead calls \n");
        vector<CallInst *> dead_calls;
        for (auto &B : *wrappedF) {
          for (auto &I : B) {
            if (auto CI = dyn_cast<CallInst>(&I)) {
              auto called = CI->getCalledFunction();
              if (called && called->isIntrinsic()) {
                auto intrin_id = called->getIntrinsicID();
                if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_tid_x) {
                  auto LI = new LoadInst(idx, "idx", CI);
                  CI->replaceAllUsesWith(LI);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_tid_y) {
                  auto LI = new LoadInst(idy, "idy", CI);
                  CI->replaceAllUsesWith(LI);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_tid_z) {
                  auto LI = new LoadInst(idz, "idz", CI);
                  CI->replaceAllUsesWith(LI);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ctaid_x) {
                  CI->replaceAllUsesWith(bidx);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ctaid_y) {
                  CI->replaceAllUsesWith(bidy);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ctaid_z) {
                  CI->replaceAllUsesWith(bidz);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_nctaid_x) {
                  CI->replaceAllUsesWith(maxblockx);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_nctaid_y) {
                  CI->replaceAllUsesWith(maxblocky);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_nctaid_z) {
                  CI->replaceAllUsesWith(maxblockz);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ntid_x) {
                  CI->replaceAllUsesWith(maxidx);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ntid_y) {
                  CI->replaceAllUsesWith(maxidy);
                  dead_calls.push_back(CI);
                } else if (intrin_id == Intrinsic::nvvm_read_ptx_sreg_ntid_z) {
                  CI->replaceAllUsesWith(maxidz);
                  dead_calls.push_back(CI);
                }
              }
            }
          }
        }

        for (auto I : dead_calls)
          I->eraseFromParent();

        if (foo != origFoo)
          foo->eraseFromParent();

        __verbose("Cleaning up");
        //cleanup
        Function *vec_F = M.getFunction("__vectorized__" + F->getName().str());
        F->eraseFromParent();
        if (vec_F)
          vec_F->eraseFromParent();

        __verbose("creating shared mem \n");
        // At this point we inlined all kernels and can safely replace the shared mem global variable
        //create shared memory buffer
        PACXXNativeSMTransformer smTransformer;
        smTransformer.runOnFunction(*wrappedF);
        //createSharedMemoryBuffer(M, wrappedF, sm_size);

      }

      origFoo->eraseFromParent();

      return true;
    }

    //TODO remove if everything is working
    void PACXXNativeLinker::createSharedMemoryBuffer(Module &M, Function *wrapper, Value *sm_size) {

      // remove constExpr so it is easier to replace the global shared memory
      //TODO do not remove all constExpr of the kernel. remove only those that use the shared memory
      //TODO move to own function pass
      //TODO place instructions of constExpr at the right place
      transformConstExprToInst(wrapper);

      auto internal_sm = getSMGlobalsUsedByKernel(M, wrapper, true);
      auto external_sm = getSMGlobalsUsedByKernel(M, wrapper, false);

      BasicBlock *entry = &wrapper->front();
      BasicBlock *sharedMemBB = BasicBlock::Create(wrapper->getContext(), "shared mem", wrapper, entry);

      if(!internal_sm.empty()) {
        __verbose("internal shared memory found\n");
        createInternalSharedMemoryBuffer(M, internal_sm, wrapper, sharedMemBB);
      }

      if(!external_sm.empty()) {
        __verbose("external shared memory found\n");
        createExternalSharedMemoryBuffer(M, external_sm, wrapper, sm_size, sharedMemBB);
      }

      BranchInst::Create(entry, sharedMemBB);

      __verbose("created shared memory");
    }

    set<GlobalVariable *> PACXXNativeLinker::getSMGlobalsUsedByKernel(Module &M, Function *kernel, bool internal) {
      set<GlobalVariable *> sm;
      for (auto &GV : M.globals()) {
        if (internal ? GV.hasInternalLinkage() : GV.hasExternalLinkage() && GV.getType()->getAddressSpace() == 3) {
          for (User *GVUsers : GV.users()) {
            if (Instruction *Inst = dyn_cast<Instruction>(GVUsers)) {
              if (Inst->getParent()->getParent() == kernel) {
                sm.insert(&GV);
              }
            }
          }
        }
      }
      return sm;
    }

    void PACXXNativeLinker::transformConstExprToInst(Function *wrapper) {
      __verbose("Transforming constExpr to instructions");

      // look at all instructions and check if they use a constexpr
      for (auto I = wrapper->begin(), IE = wrapper->end(); I != IE; ++I) {
        for (auto II = I->begin(), IIE = I->end(); II != IIE; ++II) {
          Instruction *inst = &*II;
          recursiveTransformConstExprToInst(inst, inst);
        }
      }
    }

    void PACXXNativeLinker::recursiveTransformConstExprToInst(Instruction *inst, Instruction *insertBefore) {
      for (auto &op : inst->operands()) {
        if (ConstantExpr *constExpr = dyn_cast<ConstantExpr>(op.get())) {
          Instruction *constInst = constExpr->getAsInstruction();
          constInst->insertBefore(insertBefore);
          for(unsigned i = 0; i < constExpr->getNumOperands(); ++i) {
            constInst->setOperand(i, constExpr->getOperand(i));
          }
          inst->replaceUsesOfWith(constExpr, constInst);
          // check if the newly created instruction uses a constexpr
          recursiveTransformConstExprToInst(constInst, constInst);
        } else continue;
      }
    }

    void PACXXNativeLinker::createInternalSharedMemoryBuffer(Module &M, set<GlobalVariable*> &globals,
                                                             Function *wrapper,
                                                             BasicBlock *sharedMemBB) {

      set<GetElementPtrInst *> gepInstructions;
      for (auto GV : globals) {
        Type *sm_type = GV->getType()->getElementType();
        AllocaInst *sm_alloc = new AllocaInst(sm_type, nullptr,
                                              M.getDataLayout().getABITypeAlignment(sm_type), "internal_sm",
                                              sharedMemBB);
        if (GV->hasInitializer() && !isa<UndefValue>(GV->getInitializer()))
          new StoreInst(GV->getInitializer(), sm_alloc, sharedMemBB);

        for (auto *GVUser : GV->users()) {
          if (Instruction *userInst = dyn_cast<Instruction>(GVUser)) {

            if (!(userInst->getParent()->getParent() == wrapper)) continue;

            // collect GEPs
            if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(userInst))
              gepInstructions.insert(GEP);
            // handle casts because of addrspaces
            else if(CastInst *cast = dyn_cast<CastInst>(userInst)) {
              Type * destType = cast->getDestTy();
              if(PointerType * ptrType = dyn_cast<PointerType>(destType)) {
                destType = PointerType::get(ptrType->getElementType(), 0);
              }
              CastInst *newCast = CastInst::Create(cast->getOpcode(), sm_alloc, destType, "", cast);
              // unsafe, but currently i dont know another way to remove the addrspace
              cast->mutateType(destType);
              cast->replaceAllUsesWith(newCast);
              cast->eraseFromParent();
              // remove addrspace casts, because we are already in addrspace 0
              for(auto user : newCast->users()) {
                if (AddrSpaceCastInst *addrSpaceCast = dyn_cast<AddrSpaceCastInst>(user)) {
                  addrSpaceCast->replaceAllUsesWith(newCast);
                  addrSpaceCast->eraseFromParent();
                }
              }
            }
            // else replace use of global variable
            else
              userInst->replaceUsesOfWith(GV, sm_alloc);
          }
        }

        for (auto GEP : gepInstructions) {
          SmallVector<Value *, 4> ValueOperands(GEP->op_begin(), GEP->op_end());
          ArrayRef<Value *> Ops(ValueOperands);
          GetElementPtrInst *newGEP = GEP->isInBounds() ?
                                      GetElementPtrInst::CreateInBounds(sm_type, sm_alloc, Ops.slice(1), "", GEP) :
                                      GetElementPtrInst::Create(sm_type, sm_alloc, Ops.slice(1), "", GEP);
          for (auto GEPUser : GEP->users()) {
            // not needed anymore because we are already in addrspace 0
            if (AddrSpaceCastInst *cast = dyn_cast<AddrSpaceCastInst>(GEPUser)) {
              cast->replaceAllUsesWith(newGEP);
              cast->eraseFromParent();
            }
            // need to handle other casts because of addrspace
            else if(CastInst *cast = dyn_cast<CastInst>(GEPUser)) {
              Type * destType = cast->getDestTy();
              if(PointerType * ptrType = dyn_cast<PointerType>(destType)) {
                destType = PointerType::get(ptrType->getElementType(), 0);
              }
              CastInst *newCast = CastInst::Create(cast->getOpcode(), newGEP, destType, "", cast);
              // unsafe, but currently i dont know another way to remove the addrspace
              cast->mutateType(destType);
              cast->replaceAllUsesWith(newCast);
              cast->eraseFromParent();
              // remove addrspace casts, because we are already in addrspace 0
              for(auto user : newCast->users()) {
                if (AddrSpaceCastInst *addrSpaceCast = dyn_cast<AddrSpaceCastInst>(user)) {
                  addrSpaceCast->replaceAllUsesWith(newCast);
                  addrSpaceCast->eraseFromParent();
                }
              }
            }
          }
          GEP->eraseFromParent();
        }
      }
    }

    void PACXXNativeLinker::createExternalSharedMemoryBuffer(Module &M, set<GlobalVariable*> &globals, Function *wrapper,
                                                             Value *sm_size, BasicBlock *sharedMemBB) {
      for (auto GV : globals) {
        Type *GVType = GV->getType()->getElementType();

        if (isa<PointerType>(GVType))
          handleExternalPtr(M, wrapper, GV, sm_size, sharedMemBB);
        else
          handleExternalArray(M, wrapper, GV, sm_size, sharedMemBB);
      }
    }

    void PACXXNativeLinker::handleExternalPtr(Module &M, Function *wrapper, GlobalVariable *GV,
                                              Value *sm_size, BasicBlock *sharedMemBB) {

      Type *sm_type = GV->getType()->getElementType()->getPointerElementType();
      Value *typeSize = ConstantInt::get(Type::getInt32Ty(M.getContext()),
                                         M.getDataLayout().getTypeSizeInBits(sm_type) / 8);
      //calc number of elements
      BinaryOperator *div = BinaryOperator::CreateUDiv(sm_size, typeSize, "numElem", sharedMemBB);
      AllocaInst *sm_alloc = new AllocaInst(sm_type, div, M.getDataLayout().getABITypeAlignment(sm_type),
                                            "external_sm", sharedMemBB);
      AllocaInst *ptrTosm = new AllocaInst(PointerType::get(sm_type, 0), nullptr, "ptrToSM", sharedMemBB);
      new StoreInst(sm_alloc, ptrTosm, sharedMemBB);

      for (auto user : GV->users()) {
        if (Instruction *userInst = dyn_cast<Instruction>(user)) {
          if (!(userInst->getParent()->getParent() == wrapper)) continue;
          //addrspace cast are not nessecary
          if (isa<AddrSpaceCastInst>(userInst)) {
            userInst->replaceAllUsesWith(ptrTosm);
            userInst->eraseFromParent();
          }
            // need to handle casts because of addrspace
          else if (CastInst *cast = dyn_cast<CastInst>(userInst)) {
            Type *destType = cast->getDestTy();
            if (PointerType *ptrType = dyn_cast<PointerType>(destType)) {
              destType = PointerType::get(ptrType->getElementType(), 0);
            }
            CastInst *newCast = CastInst::Create(cast->getOpcode(), ptrTosm, destType, "", cast);
            // unsafe, but currently i dont know another way to remove the addrspace
            cast->mutateType(destType);
            cast->replaceAllUsesWith(newCast);
            cast->eraseFromParent();
            // remove addrspace casts, because we are already in addrspace 0
            for (auto user : newCast->users()) {
              if (AddrSpaceCastInst *addrSpaceCast = dyn_cast<AddrSpaceCastInst>(user)) {
                addrSpaceCast->replaceAllUsesWith(newCast);
                addrSpaceCast->eraseFromParent();
              }
            }
          }
          userInst->replaceUsesOfWith(GV, ptrTosm);
        }
      }
    }

    void PACXXNativeLinker::handleExternalArray(Module &M, Function *wrapper,
                                                GlobalVariable *GV, Value *sm_size, BasicBlock *sharedMemBB) {
      set<GetElementPtrInst *> gepInstructions;
      Type *sm_type = GV->getType()->getElementType()->getArrayElementType();
      Value *typeSize = ConstantInt::get(Type::getInt32Ty(M.getContext()),
                                         M.getDataLayout().getTypeSizeInBits(sm_type) / 8);
      BinaryOperator *div = BinaryOperator::CreateUDiv(sm_size, typeSize, "numElem", sharedMemBB);
      AllocaInst *sm_alloc = new AllocaInst(sm_type, div,
                                            M.getDataLayout().getABITypeAlignment(sm_type), "external_sm",
                                            sharedMemBB);

      if (GV->hasInitializer() && !isa<UndefValue>(GV->getInitializer()))
        new StoreInst(GV->getInitializer(), sm_alloc, sharedMemBB);

      for (auto *GVUser : GV->users()) {
        if (Instruction *userInst = dyn_cast<Instruction>(GVUser)) {

          if (!(userInst->getParent()->getParent() == wrapper)) continue;

          // collect GEPs
          if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(userInst))
            gepInstructions.insert(GEP);
            // handle casts because of addrspaces
          else if (CastInst *cast = dyn_cast<CastInst>(userInst)) {
            Type *destType = cast->getDestTy();
            if (PointerType *ptrType = dyn_cast<PointerType>(destType)) {
              destType = PointerType::get(ptrType->getElementType(), 0);
            }
            CastInst *newCast = CastInst::Create(cast->getOpcode(), sm_alloc, destType, "", cast);
            // unsafe, but currently i dont know another way to remove the addrspace
            cast->mutateType(destType);
            cast->replaceAllUsesWith(newCast);
            cast->eraseFromParent();
            // remove addrspace casts, because we are already in addrspace 0
            for (auto user : newCast->users()) {
              if (AddrSpaceCastInst *addrSpaceCast = dyn_cast<AddrSpaceCastInst>(user)) {
                addrSpaceCast->replaceAllUsesWith(newCast);
                addrSpaceCast->eraseFromParent();
              }
            }
          }
            // else replace use of global variable
          else
            userInst->replaceUsesOfWith(GV, sm_alloc);
        }
      }

      for (auto GEP : gepInstructions) {
        SmallVector<Value *, 4> ValueOperands(GEP->op_begin(), GEP->op_end());
        ArrayRef<Value *> Ops(ValueOperands);
        GetElementPtrInst *newGEP = GEP->isInBounds() ?
                                    GetElementPtrInst::CreateInBounds(sm_type, sm_alloc, Ops.slice(2), "", GEP) :
                                    GetElementPtrInst::Create(sm_type, sm_alloc, Ops.slice(2), "", GEP);
        for (auto GEPUser : GEP->users()) {
          // not needed anymore because we are already in addrspace 0
          if (AddrSpaceCastInst *cast = dyn_cast<AddrSpaceCastInst>(GEPUser)) {
            cast->replaceAllUsesWith(newGEP);
            cast->eraseFromParent();
          }
            // need to handle other casts because of addrspace
          else if (CastInst *cast = dyn_cast<CastInst>(GEPUser)) {
            Type *destType = cast->getDestTy();
            if (PointerType *ptrType = dyn_cast<PointerType>(destType)) {
              destType = PointerType::get(ptrType->getElementType(), 0);
            }
            CastInst *newCast = CastInst::Create(cast->getOpcode(), newGEP, destType, "", cast);
            // unsafe, but currently i dont know another way to remove the addrspace
            cast->mutateType(destType);
            cast->replaceAllUsesWith(newCast);
            cast->eraseFromParent();
            // remove addrspace casts, because we are already in addrspace 0
            for (auto user : newCast->users()) {
              if (AddrSpaceCastInst *addrSpaceCast = dyn_cast<AddrSpaceCastInst>(user)) {
                addrSpaceCast->replaceAllUsesWith(newCast);
                addrSpaceCast->eraseFromParent();
              }
            }
          }
        }
        GEP->eraseFromParent();
      }
    }

    char PACXXNativeLinker::ID = 0;
    static RegisterPass<PACXXNativeLinker>
            X("pacxx_native", "Inlines functions into kernels", false, false);

    Pass *createPACXXNativeLinkerPass() { return new PACXXNativeLinker(); }
}
