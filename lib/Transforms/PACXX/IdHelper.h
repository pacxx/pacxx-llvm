//
// Created by lars on 07/12/16.
//

#ifndef LLVM_IDHELPER_H
#define LLVM_IDHELPER_H

namespace pacxx {
    namespace IdHelper {

        enum IdType {
            X, Y, Z
        };

        inline bool multiplyingIds(Instruction *inst, IdType id) {
            if (!(inst->getOpcode() == Instruction::Mul))
                return false;

            Intrinsic::ID ntidID, ctaidID;

            switch (id) {
                case X:
                    ntidID = Intrinsic::nvvm_read_ptx_sreg_ntid_x;
                    ctaidID = Intrinsic::nvvm_read_ptx_sreg_ctaid_x;
                    break;
                case Y:
                    ntidID = Intrinsic::nvvm_read_ptx_sreg_ntid_y;
                    ctaidID = Intrinsic::nvvm_read_ptx_sreg_ctaid_y;
                    break;
                case Z:
                    ntidID = Intrinsic::nvvm_read_ptx_sreg_ntid_z;
                    ctaidID = Intrinsic::nvvm_read_ptx_sreg_ctaid_z;
                    break;
                default:
                    __verbose("unsupported id specified. doing nothing");
                    return false;
            }

            if (IntrinsicInst * firstOperand = dyn_cast<IntrinsicInst>(inst->getOperand(0))) {
                if (IntrinsicInst * secondOperand = dyn_cast<IntrinsicInst>(inst->getOperand(1))) {
                    if ((firstOperand->getIntrinsicID() == ntidID &&
                         secondOperand->getIntrinsicID() == ctaidID)
                        || (firstOperand->getIntrinsicID() == ctaidID &&
                            secondOperand->getIntrinsicID() == ntidID))
                        return true;
                }
            }
            return false;
        }

        inline bool addingTid(BinaryOperator *binOp, IdType id) {
            if (!(binOp->getOpcode() == Instruction::Add))
                return false;

            Intrinsic::ID tidID;

            switch (id) {
                case X:
                    tidID = Intrinsic::nvvm_read_ptx_sreg_tid_x; // initialization
                    break;
                case Y:
                    tidID = Intrinsic::nvvm_read_ptx_sreg_tid_y;
                    break;
                case Z:
                    tidID = Intrinsic::nvvm_read_ptx_sreg_tid_z;
                    break;
                default:
                    __verbose("unsupported id specified. doing nothing");
                    return false;
            }

            if (Instruction * firstOperand = dyn_cast<Instruction>(binOp->getOperand(0))) {
                if (Instruction * secondOperand = dyn_cast<Instruction>(binOp->getOperand(1))) {
                    if (IntrinsicInst * tid = dyn_cast<IntrinsicInst>(firstOperand))
                        if (tid->getIntrinsicID() == tidID
                            && secondOperand->getOpcode() == Instruction::Mul)
                            return multiplyingIds(secondOperand, id);
                    if (IntrinsicInst * tid = dyn_cast<IntrinsicInst>(secondOperand))
                        if (tid->getIntrinsicID() == tidID
                            && firstOperand->getOpcode() == Instruction::Mul)
                            return multiplyingIds(firstOperand, id);
                }
            }
            return false;
        }

        inline bool isBaseId(Value *value, IdType id) {
            if (Instruction * inst = dyn_cast<Instruction>(value)) {
                if (BinaryOperator * binOp = dyn_cast<BinaryOperator>(inst)) {
                    return addingTid(binOp, id);
                }
            }
            return false;
        }
    }
}

#endif //LLVM_IDHELPER_H
