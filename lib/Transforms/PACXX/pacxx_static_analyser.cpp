/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2015
*/

//#include <iostream>
#include <vector>
#include <cassert>
#include <algorithm>

#define DEBUG_TYPE "pacxx_static_eval"

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
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Dominators.h"

#include "ModuleHelper.h"

#define GLOBAL_ID_PATTERN "mad.lo.u32 $0, $1, $2, $3;"

using namespace llvm;
using namespace std;
using namespace pacxx;

namespace {

struct PACXXStaticEvalPass : public ModulePass {
  static char ID;
  PACXXStaticEvalPass(bool runtime = false) : ModulePass(ID) {}
  virtual ~PACXXStaticEvalPass() {}

  virtual bool runOnModule(Module &M) {
    auto kernels = getTagedFunctions(&M, "nvvm.annotations", "kernel");

    StaticEvaluater eva;
    for (auto F : kernels) {
      MDNode *launchConfig = nullptr;
      auto kernelMD =
          M.getNamedMetadata(string("pacxx.kernel.") + F->getName().str());

      for (unsigned i = 0; i < kernelMD->getNumOperands(); ++i) {
        auto MD = kernelMD->getOperand(i);
        if (auto ID = dyn_cast<MDString>(MD->getOperand(0))) {
          if (ID->getString().equals("launch config")) {
            launchConfig = MD;
            break;
          }
        }
      }
      if (launchConfig) {
        eva.setLaunchConfiguration(launchConfig);
      }
      eva.visit(F);
    }
    eva.finalize();

    return true;
  }

  enum EvalResultType { evaluated, unevaluated };

  struct EvalResult {
    EvalResult(EvalResultType r = unevaluated)
        : upper(64, std::numeric_limits<long>::max(), true),
          lower(64, std::numeric_limits<long>::min(), true), res(r) {}
    EvalResult(APInt e, EvalResultType r = unevaluated)
        : upper(e), lower(e), res(r) {}
    EvalResult(APInt u, APInt l, EvalResultType r = unevaluated)
        : upper(u), lower(l), res(r) {}
    APInt upper;
    APInt lower;
    EvalResultType res;
  };

  class StaticEvaluater : public InstVisitor<StaticEvaluater> {
  public:
    StaticEvaluater() {}
    virtual ~StaticEvaluater() {}

    void setLaunchConfiguration(MDNode *LC) {
      for (unsigned i = 1; i < LC->getNumOperands(); ++i) {
        if (auto C = dyn_cast<ConstantInt>(
                dyn_cast<ValueAsMetadata>(LC->getOperand(i).get())
                    ->getValue())) {
          config.push_back(static_cast<int>(*(C->getValue().getRawData())));
        }
      }
      configurated = true;
    }

    void visitICmpInst(ICmpInst &I) {
      EvalResult lhs, rhs;
      lhs = evaluate(*I.getOperand(0));
      rhs = evaluate(*I.getOperand(1));
      bool result_upper = false;
      bool result_lower = false;
      bool result = false;
      if (lhs.res == evaluated && rhs.res == evaluated) {
        switch (I.getPredicate()) {
        case ICmpInst::ICMP_SGT:
          //__debug(*lhs.upper.getRawData(), " > ", *rhs.upper.getRawData(),
          //        " | ", *lhs.lower.getRawData(), " > ", *rhs.lower.getRawData());
		  //__debug(lhs.upper.isNegative(), " <-> ", rhs.upper.isNegative(), " | ", lhs.lower.isNegative(), " <-> ", rhs.lower.isNegative());
          result_upper = lhs.upper.sgt(rhs.upper);
          result_lower = lhs.lower.sgt(rhs.lower);
		  //__debug(result_upper, " == ", result_lower);
          break;
        case ICmpInst::ICMP_UGT:
          //__debug(*lhs.upper.getRawData(), " > ", *rhs.upper.getRawData(),
           //       " | ", *lhs.lower.getRawData(), " > ",
           //       *rhs.lower.getRawData());
          result_upper = lhs.upper.ugt(rhs.upper);
          result_lower = lhs.lower.ugt(rhs.lower);
          break;
        case ICmpInst::ICMP_SGE:
          //__debug(*lhs.upper.getRawData(), " >= ", *rhs.upper.getRawData(),
          //        " | ", *lhs.lower.getRawData(), " >= ",
          //        *rhs.lower.getRawData());
          result_upper = lhs.upper.sge(rhs.upper);
          result_lower = lhs.lower.sge(rhs.lower);
          break;
        case ICmpInst::ICMP_SLT:
          //__debug(*lhs.upper.getRawData(), " < ", *rhs.upper.getRawData(),
          //        " | ", *lhs.lower.getRawData(), " < ",
          //        *rhs.lower.getRawData());
          result_upper = lhs.upper.slt(rhs.upper);
          result_lower = lhs.lower.slt(rhs.lower);
          break;
        case ICmpInst::ICMP_ULT:
          //__debug(*lhs.upper.getRawData(), " < ", *rhs.upper.getRawData(),
          //        " | ", *lhs.lower.getRawData(), " < ",
          //        *rhs.lower.getRawData());
          result_upper = lhs.upper.ult(rhs.upper);
          result_lower = lhs.lower.ult(rhs.lower);
          break;
        case ICmpInst::ICMP_EQ:
          //__debug(*lhs.upper.getRawData(), " == ", *rhs.upper.getRawData(),
          //        " | ", *lhs.lower.getRawData(), " == ",
          //        *rhs.lower.getRawData());
          result_upper = lhs.upper.eq(rhs.upper);
          result_lower = lhs.lower.eq(rhs.lower);
          break;
        case ICmpInst::ICMP_NE:
          //__debug(*lhs.upper.getRawData(), " != ", *rhs.upper.getRawData(),
          //        " | ", *lhs.lower.getRawData(), " != ",
          //        *rhs.lower.getRawData());
          result_upper = lhs.upper.ne(rhs.upper);
          result_lower = lhs.lower.ne(rhs.lower);
          break;
        case ICmpInst::ICMP_UGE:
          //__debug(*lhs.upper.getRawData(), " >= ", *rhs.upper.getRawData(),
          //        " | ", *lhs.lower.getRawData(), " >= ",
          //        *rhs.lower.getRawData());
          result_upper = lhs.upper.uge(rhs.upper);
          result_lower = lhs.lower.uge(rhs.lower);
          break;
        default:
          //__debug("Unevaluated but evaluateable:", *lhs.upper.getRawData(),
          //          " ? ", *rhs.upper.getRawData(), " : ", I.getPredicate(),
          //          " | ", *lhs.lower.getRawData(), " ? ",
          //          *rhs.lower.getRawData(), " : ", I.getPredicate());
          return;
          break;
        }
        result = !(result_lower ^ result_upper);
      } else {
        return;
      }
      if (result)
        for (auto u : I.users()) {
          if (BranchInst *br = dyn_cast<BranchInst>(u)) {
            branches.push_back(make_pair(br, result_upper));
          }
        }
      //else
        //__debug("upper and lower bound evaluation not equal -> discarding");
    }

    void visitBranchInst(BranchInst &I) {
      if (I.isConditional()) {
        auto cond = I.getCondition();
        if (ExtractValueInst *evi = dyn_cast<ExtractValueInst>(cond)) {
          if (CallInst *intri = dyn_cast<CallInst>(evi->getOperand(0))) {
            if (intri->getCalledFunction() &&
                intri->getCalledFunction()->getIntrinsicID() ==
                    Intrinsic::uadd_with_overflow) {
              //__debug("llvm!");
              evi->replaceAllUsesWith(
                  ConstantInt::get(Type::getInt1Ty(I.getContext()), 0));
            }
          }
        }
      }
    }

    EvalResult evaluate(Value &I) {
      if (auto bin = dyn_cast<BinaryOperator>(&I)) {
        return evaluateBinOp(*bin);
      } else if (auto constI = dyn_cast<ConstantInt>(&I)) {
        return evaluateConst(*constI);
      } else if (auto call = dyn_cast<CallInst>(&I)) {
        return evaluateCall(*call);
      } else if (auto cast = dyn_cast<CastInst>(&I)) {
        return evaluateCast(*cast);
      }
      // I.dump();
      return EvalResult();
    }

    EvalResult evaluateBinOp(BinaryOperator &op) {

      EvalResult lhs, rhs;
      lhs = evaluate(*op.getOperand(0));
      rhs = evaluate(*op.getOperand(1));

      EvalResultType restype = evaluated;
      APInt eval_upper, eval_lower;
      if (lhs.res == evaluated && rhs.res == evaluated) {
        switch (op.getOpcode()) {
        case BinaryOperator::Add:
          eval_upper = lhs.upper + rhs.upper;
          eval_lower = lhs.lower + rhs.lower;
          break;
        case BinaryOperator::Sub:
          eval_upper = lhs.upper - rhs.upper;
          eval_lower = lhs.lower - rhs.lower;
          break;
        case BinaryOperator::Mul:
          eval_upper = lhs.upper * rhs.upper;
          eval_lower = lhs.lower * rhs.lower;
          break;
        case BinaryOperator::SRem:
          eval_upper = lhs.upper.srem(rhs.upper);
          eval_lower = lhs.lower.srem(rhs.lower);
          break;
        case BinaryOperator::SDiv:
          if (static_cast<long long>(*rhs.upper.getRawData()) != 0)
            eval_upper = lhs.upper.sdiv(rhs.upper);
          else
            restype = unevaluated;
          if (static_cast<long long>(*rhs.lower.getRawData()) != 0)
            eval_lower = lhs.upper.sdiv(rhs.lower);
          else
            restype = unevaluated;
          break;
        case BinaryOperator::Shl:
          eval_upper = lhs.upper.shl(static_cast<unsigned int>(*rhs.upper.getRawData()));
          eval_lower = lhs.lower.shl(static_cast<unsigned int>(*rhs.lower.getRawData()));
          break;
        case BinaryOperator::LShr:
          eval_upper = lhs.upper.lshr(static_cast<unsigned int>(*rhs.upper.getRawData()));
          eval_lower = lhs.lower.lshr(static_cast<unsigned int>(*rhs.lower.getRawData()));
          break;
        case BinaryOperator::Or:
          eval_upper = lhs.upper | rhs.upper;
          eval_lower = lhs.lower | rhs.lower;
          break;
        case BinaryOperator::And:
          eval_upper = lhs.upper & rhs.upper;
          eval_lower = lhs.lower & rhs.lower;
          break;
        case BinaryOperator::AShr:
          eval_upper = lhs.upper.ashr(static_cast<unsigned int>(*rhs.upper.getRawData()));
          eval_lower = lhs.lower.ashr(static_cast<unsigned int>(*rhs.lower.getRawData()));
          break;
        case BinaryOperator::URem:
          if (static_cast<long long>(*rhs.upper.getRawData()) != 0)
            eval_upper = lhs.upper.urem(rhs.upper);
          else
            restype = unevaluated;
          if (static_cast<long long>(*rhs.lower.getRawData()) != 0)
            eval_lower = lhs.lower.urem(rhs.lower);
          else
            restype = unevaluated;
          break;
        default:
          //__debug("unsuported binop");
	//	  __dump(op);
          restype = unevaluated;
          break;
        }
        return {eval_upper, eval_lower, restype};
      }
      return {eval_upper, eval_lower, unevaluated};
    }

    EvalResult evaluateConst(ConstantInt &I) {
	  APInt val = I.getValue();
	  
      if (val.getBitWidth() < 64) {
        if (val.isNegative()) {
          val = val.sext(64);
        } else {
          val = val.zext(64);
        }
      }

	  EvalResult eval{ val, evaluated };
      return eval;
    }

    EvalResult evaluateCast(CastInst &I) {
      EvalResult rhs = evaluate(*I.getOperand(0));
      switch (I.getOpcode()) {
      case CastInst::Trunc:
        if (I.getDestTy()->getIntegerBitWidth() < rhs.upper.getBitWidth())
          rhs.upper = rhs.upper.trunc(I.getDestTy()->getIntegerBitWidth());
        if (I.getDestTy()->getIntegerBitWidth() < rhs.lower.getBitWidth())
          rhs.lower = rhs.lower.trunc(I.getDestTy()->getIntegerBitWidth());
        break;
      case CastInst::SExt:
        if (rhs.upper.getBitWidth() < I.getDestTy()->getIntegerBitWidth()) {
          rhs.upper = rhs.upper.sext(I.getDestTy()->getIntegerBitWidth());
          rhs.lower = rhs.lower.sext(I.getDestTy()->getIntegerBitWidth());
        }
        break;
      case CastInst::ZExt:
        if (rhs.upper.getBitWidth() < I.getDestTy()->getIntegerBitWidth()) {
          rhs.upper = rhs.upper.zext(I.getDestTy()->getIntegerBitWidth());
          rhs.lower = rhs.lower.zext(I.getDestTy()->getIntegerBitWidth());
        }
        break;
      case CastInst::PtrToInt:
		  //__debug(I);
        rhs.res = unevaluated;
        break;
      default:
        //__debug("unsupported cast");
		//__debug(I);
        rhs.res = unevaluated;
        break;
      }
      return rhs;
    }

    EvalResult evaluateCall(CallInst &call) {
      EvalResult result;

      result.res = unevaluated;

      if (configurated) {
        if (!call.isInlineAsm()) {
          if (call.getCalledFunction()->isIntrinsic()) {
            auto IID = call.getCalledFunction()->getIntrinsicID();
            switch (IID) {
            case Intrinsic::nvvm_read_ptx_sreg_tid_x:
              result.upper = APInt(64, config[0] - 1, true);
              result.lower = APInt(64, 0, true);
              result.res = evaluated;
              break;
            case Intrinsic::nvvm_read_ptx_sreg_tid_y:
              result.upper = APInt(64, config[1] - 1, true);
              result.lower = APInt(64, 0, true);
              result.res = evaluated;
              break;
            case Intrinsic::nvvm_read_ptx_sreg_tid_z:
              result.upper = APInt(64, config[2] - 1, true);
              result.lower = APInt(64, 0, true);
              result.res = evaluated;
              break;
            case Intrinsic::nvvm_read_ptx_sreg_ctaid_x:
              result.upper = APInt(64, config[3] - 1, true);
              result.lower = APInt(64, 0, true);
              result.res = evaluated;
              break;
            case Intrinsic::nvvm_read_ptx_sreg_ctaid_y:
              result.upper = APInt(64, config[4] - 1, true);
              result.lower = APInt(64, 0, true);
              result.res = evaluated;
              break;
            case Intrinsic::nvvm_read_ptx_sreg_ctaid_z:
              result.upper = APInt(64, config[5] - 1, true);
              result.lower = APInt(64, 0, true);
              result.res = evaluated;
              break;
            case Intrinsic::nvvm_read_ptx_sreg_ntid_x:
            case Intrinsic::nvvm_read_ptx_sreg_ntid_y:
            case Intrinsic::nvvm_read_ptx_sreg_ntid_z:
            case Intrinsic::nvvm_read_ptx_sreg_nctaid_x:
            case Intrinsic::nvvm_read_ptx_sreg_nctaid_y:
            case Intrinsic::nvvm_read_ptx_sreg_nctaid_z:
              break;
            default:
              //__debug("unsupported intrinsic");
			  //__debug(call);
              break;
            }
          } else {
            auto F = call.getCalledFunction();

            if (F->getName().find("get_local_size") != StringRef::npos) {
              if (auto CI = dyn_cast<ConstantInt>(call.getOperand(0))) {
                auto v = static_cast<int>(*(CI->getValue().getRawData()));
                result.upper = APInt(64, config[v + 3] - 1, true);
                result.lower = APInt(64, 0, true);
                result.res = evaluated;
              }
            } else if (F->getName().find("get_local_id") != StringRef::npos) {
				//__debug(*call.getOperand(0));
              if (auto CI = dyn_cast<ConstantInt>(call.getOperand(0))) {
                auto v = static_cast<int>(*(CI->getValue().getRawData()));
                result.upper = APInt(64, config[v] - 1, true);
                result.lower = APInt(64, 0, true);
                result.res = evaluated;
              }
            } else if (F->getName().find("get_global_id") != StringRef::npos) {
				//__debug(*call.getOperand(0));
              if (auto CI = dyn_cast<ConstantInt>(call.getOperand(0))) {
                auto v = static_cast<int>(*(CI->getValue().getRawData()));
                result.upper = APInt(64, config[v] * config[v + 3] - 1, true);
                result.lower = APInt(64, 0, true);
                result.res = evaluated;
              }
            } else {
			  //__debug(call);
              //__debug("unsupported function call ", F->getName().str());
            }
          }
        }
      }
      return result;
    }

    void finalize() {
      for (auto br : branches) {
        BasicBlock *succ, *dead, *curr;

        if (br.second) {
          succ = br.first->getSuccessor(0);
          dead = br.first->getSuccessor(1);
        } else {
          dead = br.first->getSuccessor(0);
          succ = br.first->getSuccessor(1);
        }
        curr = br.first->getParent();

        dead->removePredecessor(curr);
        br.first->eraseFromParent();
        BranchInst::Create(succ, curr);
      }
    }

  private:
    vector<pair<BranchInst *, bool>> branches;
    vector<int> config;
    bool configurated = false;
  };
};

char PACXXStaticEvalPass::ID = 0;
static RegisterPass<PACXXStaticEvalPass>
    X("pacxx_static", "PACXX: static analyser for CFG optimization", false,
      false);
}

namespace llvm {
Pass *createPACXXStaticEvalPass() { return new PACXXStaticEvalPass(); }
}
