// Created by Michael Haidl and lars

#define PACXX_PASS_NAME "PACXXNativeLinker"

#include <llvm/IR/CFG.h>
#include "Log.h"
#include "ModuleHelper.h"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace llvm {

struct PACXXNativeLinker : public ModulePass {
  static char ID;

  PACXXNativeLinker() : ModulePass(ID) {}

  virtual ~PACXXNativeLinker() {}

  virtual bool runOnModule(Module &M);

  struct Value3 {

    Value3() {}

    Value3(Value *x, Value *y, Value *z) : _x(x), _y(y), _z(z) {}

    ~Value3() {}

    Value *_x;
    Value *_y;
    Value *_z;

  };

private:

  enum IdType { X, Y, Z };

  Function *getFooFunction(Function *kernel, bool vectorized, bool barrier);
  Function *createWrapper(Function *kernel, TerminatorInst **term, SmallVector<Value *, 8> &wrapperArgs);
  SmallVector<Value *, 8> createKernelArgs(Function *wrapper, Function *kernel,
                                           Value3 &blockId, Value3 &maxBlock, Value3 &maxId);
  void replaceDummyWithKernelIfNeeded(Function *wrapper, Function *kernel, SmallVector<Value *, 8> &kernelArgs,
                                      bool vectorized, bool barrier);
  void removeDeadCalls(Function *wrapper, Value3 &blockId, Value3 &maxBlock, Value3 &maxId);

  AllocaInst* getCorrectAlloca(Instruction *intrinsic, IdType id);

  void recursiveFindAlloca(BasicBlock *BB, SmallSet<BasicBlock*, 8>& visited,
                                   AllocaInst*& alloca, IdType id);

  bool isCorrectId(Instruction *inst, IdType id);

  void markWrapperAsKernel(Module &M, Function *wrapper);

};

bool PACXXNativeLinker::runOnModule(Module &M) {

  auto kernels = pacxx::getTagedFunctions(&M, "nvvm.annotations", "kernel");

  for (auto &F : kernels) {
    bool vectorized = F->hasFnAttribute("vectorized") ? true : false;
    bool barrier = F->hasFnAttribute("barrier") ? true : false;

    SmallVector < Value * , 8 > wrapperArgs;
    Value3 blockId;
    Value3 maxBlock;
    Value3 maxId;

    TerminatorInst *term;

    Function *pacxx_block = getFooFunction(F, vectorized, barrier);

    Function *wrapper = createWrapper(F, &term, wrapperArgs);

    SmallVector < Value * , 8 > kernelArgs = createKernelArgs(wrapper, F, blockId, maxBlock, maxId);

    SmallVector < Value * , 8 > blockArgs;
    blockArgs.insert(blockArgs.end(), wrapperArgs.begin(), wrapperArgs.end());
    blockArgs.insert(blockArgs.end(), kernelArgs.begin(), kernelArgs.end());

    // now that we have a call inst inline it!
    auto CI = CallInst::Create(pacxx_block, blockArgs, "", term);
    InlineFunctionInfo IFI;
    InlineFunction(CI, IFI, nullptr, false);

    replaceDummyWithKernelIfNeeded(wrapper, F, kernelArgs, vectorized, barrier);

    removeDeadCalls(wrapper, blockId, maxBlock, maxId);

    __verbose("Cleaning up");
    if (vectorized) {
      Function *vec_F = M.getFunction("__vectorized__" + F->getName().str());
      vec_F->eraseFromParent();
      // if the kernel has been vectorized and has a barrier, we neeed to remove the vectorized wrapper because we
      // will use the barrier version of the wrapper
      if (barrier) {
        Function *vecFoo = M.getFunction("__vectorized__pacxx_block__" + F->getName().str());
        vecFoo->eraseFromParent();
      }
    }
    F->eraseFromParent();
    pacxx_block->eraseFromParent();

    // finally mark the created wrapper as a kernel
    markWrapperAsKernel(M, wrapper);
  }

  Function *origFoo = M.getFunction("__pacxx_block");
  if (origFoo)
    origFoo->eraseFromParent();

  return true;
}

Function *PACXXNativeLinker::getFooFunction(Function *kernel, bool vectorized, bool barrier) {

  __verbose("Getting correct pacxx block function");

  Module *M = kernel->getParent();
  LLVMContext &ctx = M->getContext();
  Function *origFoo = M->getFunction("__pacxx_block");
  Function *pacxx_block = nullptr;


  // if the kernel has been vectorized
  if (vectorized && !barrier) {
    pacxx_block = M->getFunction("__vectorized__pacxx_block__" + kernel->getName().str());
  }
  // if the kernel has a barrier use a special pacxx block function
  if (barrier) {
    pacxx_block = M->getFunction("__barrier__pacxx_block__" + kernel->getName().str());
  }
  //if the kernel has not been vectorized and contains no barriers
  if (!vectorized && !barrier) {
    SmallVector < Type * , 8 > Params;
    for (auto &arg : origFoo->args()) {
      Params.push_back(arg.getType());
    }
    for (auto &arg : kernel->args()) {
      Params.push_back(arg.getType());
    }

    FunctionType *FTy = FunctionType::get(Type::getVoidTy(ctx), Params, false);
    pacxx_block = Function::Create(FTy, origFoo->getLinkage(), "clonedFoo", M);
    auto DestI = pacxx_block->arg_begin();
    ValueToValueMapTy VMap;
    for (auto I = origFoo->arg_begin(); I != origFoo->arg_end(); ++I) {
      DestI->setName(I->getName());
      VMap[cast<Value>(I)] = cast<Value>(DestI++);
    }
    SmallVector < ReturnInst * , 8 > returns;
    CloneFunctionInto(pacxx_block, origFoo, VMap, true, returns);
  }
  return pacxx_block;
}

Function *PACXXNativeLinker::createWrapper(Function *kernel,
                                           TerminatorInst **term,
                                           SmallVector<Value *, 8> &wrapperArgs) {
  Module *M = kernel->getParent();
  LLVMContext &ctx = M->getContext();

  SmallVector < Type * , 8 > Params;

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
  auto wrappedF = Function::Create(FTy, GlobalValue::LinkageTypes::ExternalLinkage,
                                   std::string("__wrapped__") + kernel->getName().str(), M);

  auto wrappedArgs = wrappedF->arg_begin();
  (wrappedArgs++)->setName("bidx");
  (wrappedArgs++)->setName("bidy");
  (wrappedArgs++)->setName("bidz");

  (wrappedArgs++)->setName("maxblockx");
  (wrappedArgs++)->setName("maxblocky");
  (wrappedArgs++)->setName("maxblockz");

  wrapperArgs.push_back(&*wrappedArgs);
  (wrappedArgs++)->setName("maxidx");
  wrapperArgs.push_back(&*wrappedArgs);
  (wrappedArgs++)->setName("maxidy");
  wrapperArgs.push_back(&*wrappedArgs);
  (wrappedArgs++)->setName("maxidz");

  (wrappedArgs++)->setName("sm_size");

  (wrappedArgs++)->setName("args");

  BasicBlock *entry = BasicBlock::Create(ctx, "entry", wrappedF);

  *term = ReturnInst::Create(ctx, entry);

  return wrappedF;
}

SmallVector<Value *, 8> PACXXNativeLinker::createKernelArgs(Function *wrapper, Function *kernel,
                                                            Value3 &blockId, Value3 &maxBlock, Value3 &maxId) {

  SmallVector < Value * , 8 > kernelArgs;

  Module *M = wrapper->getParent();
  LLVMContext &ctx = M->getContext();

  Value *bidx = nullptr, *bidy = nullptr, *bidz = nullptr;
  Value *maxidx = nullptr, *maxidy = nullptr, *maxidz = nullptr;
  Value *maxblockx = nullptr, *maxblocky = nullptr, *maxblockz = nullptr;

  BasicBlock *entry = &wrapper->front();

  // create the arguments from the char*
  auto argIt = wrapper->arg_begin();

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
  ++argIt;

  blockId = Value3(bidx, bidy, bidz);
  maxBlock = Value3(maxblockx, maxblocky, maxblockz);
  maxId = Value3(maxidx, maxidy, maxidz);

  // construct the kernel arguments from the char*
  BasicBlock *constructKernelArgs = BasicBlock::Create(ctx, "constructArgs", wrapper, entry);

  auto int8ptr_type = Type::getInt8PtrTy(ctx);
  auto int8ptr_align = M->getDataLayout().getPrefTypeAlignment(int8ptr_type);

  auto *alloc_args = new AllocaInst(int8ptr_type, 0, nullptr, int8ptr_align, (&*argIt)->getName(),
                                    constructKernelArgs);
  new StoreInst(&*argIt, alloc_args, false, int8ptr_align, constructKernelArgs);
  auto *args_load = new LoadInst(alloc_args, "args", false, int8ptr_align, constructKernelArgs);

  size_t offset = 0;

  for (auto I = kernel->arg_begin(), E = kernel->arg_end(); I != E; ++I) {

    auto &A = *I;

    auto arg_size = M->getDataLayout().getTypeAllocSize(A.getType());
    auto arg_alignment = M->getDataLayout().getPrefTypeAlignment(A.getType());

    auto arg_offset = (offset + arg_alignment - 1) & ~(arg_alignment - 1);
    // consider offset in char array
    auto *elem_ptr = GetElementPtrInst::CreateInBounds(Type::getInt8Ty(ctx), args_load,
                                                       ConstantInt::get(Type::getInt64Ty(ctx), arg_offset),
                                                       "", constructKernelArgs);
    // Cast to apropriate size
    auto *casted = new BitCastInst(elem_ptr, PointerType::getUnqual(A.getType()), "", constructKernelArgs);
    auto *casted_load = new LoadInst(casted, A.getName(), false,
                                     M->getDataLayout().getPrefTypeAlignment(casted->getType()),
                                     constructKernelArgs);

    kernelArgs.push_back(casted_load);

    offset = arg_offset + arg_size;
  }

  // branch to normal entry block
  llvm::BranchInst::Create(entry, constructKernelArgs);

  return kernelArgs;
}

void PACXXNativeLinker::replaceDummyWithKernelIfNeeded(Function *wrapper, Function *kernel,
                                                       SmallVector<Value *, 8> &kernelArgs,
                                                       bool vectorized, bool barrier) {

  Module *M = wrapper->getParent();
  CallInst *CI = nullptr;

  if (!barrier && !vectorized) {
    Function *dummy = M->getFunction("__dummy_kernel");

    CallInst *remove = nullptr;
    for (auto U : dummy->users()) {
      if ((CI = dyn_cast<CallInst>(U))) {
        if (CI->getParent()->getParent() == wrapper) {
          remove = CI;
          CI = CallInst::Create(kernel, kernelArgs, "", CI);
          break;
        }
      }
    }

    if (remove)
      remove->eraseFromParent();

    // time to inline the original kernel into the wrapper
    __verbose("Inline kernel \n");
    InlineFunctionInfo IFI;
    InlineFunction(CI, IFI, nullptr, false);
  }
}

void PACXXNativeLinker::removeDeadCalls(Function *wrapper, Value3 &blockId, Value3 &maxBlock, Value3 &maxId) {

  __verbose("replacing dead calls \n");
  vector < CallInst * > dead_calls;
  for (auto &B : *wrapper) {
    for (auto &I : B) {
      if (auto CI = dyn_cast<CallInst>(&I)) {
        auto called = CI->getCalledFunction();
        if (called && called->isIntrinsic()) {
          switch (called->getIntrinsicID()) {
          case Intrinsic::pacxx_read_tid_x: {
            auto idx = getCorrectAlloca(CI, IdType::X);
            auto LI = new LoadInst(idx, "idx", CI);
            CI->replaceAllUsesWith(LI);
            dead_calls.push_back(CI);
            break;
          }
          case Intrinsic::pacxx_read_tid_y: {
            auto idy = getCorrectAlloca(CI, IdType::Y);
            auto LI = new LoadInst(idy, "idy", CI);
            CI->replaceAllUsesWith(LI);
            dead_calls.push_back(CI);
            break;
          }
          case Intrinsic::pacxx_read_tid_z: {
            auto idz = getCorrectAlloca(CI, IdType::Z);
            auto LI = new LoadInst(idz, "idz", CI);
            CI->replaceAllUsesWith(LI);
            dead_calls.push_back(CI);
            break;
          }
          case Intrinsic::pacxx_read_ctaid_x: {
            CI->replaceAllUsesWith(blockId._x);
            dead_calls.push_back(CI);
            break;
          }
          case Intrinsic::pacxx_read_ctaid_y: {
            CI->replaceAllUsesWith(blockId._y);
            dead_calls.push_back(CI);
            break;
          }
          case Intrinsic::pacxx_read_ctaid_z: {
            CI->replaceAllUsesWith(blockId._z);
            dead_calls.push_back(CI);
            break;
          }
          case Intrinsic::pacxx_read_nctaid_x: {
            CI->replaceAllUsesWith(maxBlock._x);
            dead_calls.push_back(CI);
            break;
          }
          case Intrinsic::pacxx_read_nctaid_y: {
            CI->replaceAllUsesWith(maxBlock._y);
            dead_calls.push_back(CI);
            break;
          }
          case Intrinsic::pacxx_read_nctaid_z: {
            CI->replaceAllUsesWith(maxBlock._z);
            dead_calls.push_back(CI);
            break;
          }
          case Intrinsic::pacxx_read_ntid_x: {
            CI->replaceAllUsesWith(maxId._x);
            dead_calls.push_back(CI);
            break;
          }
          case Intrinsic::pacxx_read_ntid_y: {
            CI->replaceAllUsesWith(maxId._y);
            dead_calls.push_back(CI);
            break;
          }
          case Intrinsic::pacxx_read_ntid_z: {
            CI->replaceAllUsesWith(maxId._z);
            dead_calls.push_back(CI);
            break;
          }
          default: break;
          }
        }
      }
    }
  }

  __verbose("deleting \n");
  for (auto I : dead_calls)
    I->eraseFromParent();
}

AllocaInst* PACXXNativeLinker::getCorrectAlloca(Instruction *intrinsic, IdType id) {

   AllocaInst *alloca = nullptr;
   SmallSet<BasicBlock*, 8> visited;

   // first check if we find the alloca in the same basic block
   auto BB = intrinsic->getParent();
   for(auto I = intrinsic->getIterator(), IE = BB->begin(); I != IE; --I) {
       if(AllocaInst* current = dyn_cast<AllocaInst>(&*I))
       if(isCorrectId(current, id))
         return current;
   }

   // if we have not found it in the same block move upwards
   recursiveFindAlloca(BB, visited, alloca, id);
   return alloca;
}

void PACXXNativeLinker::recursiveFindAlloca(BasicBlock *BB, SmallSet<BasicBlock*, 8>& visited,
                                            AllocaInst*& alloca, IdType id) {

  if(visited.count(BB) != 0)
    return;

  visited.insert(BB);

  for(auto I = pred_begin(BB), IE = pred_end(BB); I != IE; ++I) {
      BasicBlock *pred = *I;
      for(auto &inst : *pred) {
        if(AllocaInst* current = dyn_cast<AllocaInst>(&inst))
          if (isCorrectId(current, id)) {
            alloca = current;
            return;
          }
      }
      recursiveFindAlloca(pred, visited, alloca, id);
  }
}

bool PACXXNativeLinker::isCorrectId(Instruction *inst, IdType id) {
  switch (id) {
    case X:
      return inst->getMetadata("pacxx_read_tid_x") != nullptr;
    case Y:
      return inst->getMetadata("pacxx_read_tid_y") != nullptr;
    case Z:
      return inst->getMetadata("pacxx_read_tid_z") != nullptr;
    default:
      __verbose("unsupported id specified. doing nothing");
    return false;
  }
}

void PACXXNativeLinker::markWrapperAsKernel(Module &M, Function *wrapper) {
  LLVMContext &ctx = M.getContext();
  NamedMDNode *MD = M.getOrInsertNamedMetadata("nvvm.annotations");
  SmallVector < Metadata * , 3 > MDVals;
  MDVals.push_back(ConstantAsMetadata::get(wrapper));
  MDVals.push_back(MDString::get(ctx, "kernel"));
  MDVals.push_back(ConstantAsMetadata::get(ConstantInt::get(Type::getInt32Ty(ctx), 1)));

  MD->addOperand(MDNode::get(ctx, MDVals));
}

char PACXXNativeLinker::ID = 0;
static RegisterPass <PACXXNativeLinker>
    X("pacxx_native", "Inlines functions into kernels", false, false);

Pass *createPACXXNativeLinkerPass() { return new PACXXNativeLinker(); }
}
