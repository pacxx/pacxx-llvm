//===-- llvm/CodeGen/GlobalISel/LegalizationArtifactCombiner.h --===========//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This file contains some helper functions which try to cleanup artifacts
// such as G_TRUNCs/G_[ZSA]EXTENDS that were created during legalization to make
// the types match. This file also contains some combines of merges that happens
// at the end of the legalization.
//===----------------------------------------------------------------------===//

#include "llvm/CodeGen/GlobalISel/Legalizer.h"
#include "llvm/CodeGen/GlobalISel/LegalizerInfo.h"
#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"
#include "llvm/CodeGen/GlobalISel/Utils.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "legalizer"

namespace llvm {
class LegalizationArtifactCombiner {
  MachineIRBuilder &Builder;
  MachineRegisterInfo &MRI;
  const LegalizerInfo &LI;

public:
  LegalizationArtifactCombiner(MachineIRBuilder &B, MachineRegisterInfo &MRI,
                    const LegalizerInfo &LI)
      : Builder(B), MRI(MRI), LI(LI) {}

  bool tryCombineAnyExt(MachineInstr &MI,
                        SmallVectorImpl<MachineInstr *> &DeadInsts) {
    if (MI.getOpcode() != TargetOpcode::G_ANYEXT)
      return false;
    if (MachineInstr *DefMI =
            getOpcodeDef(TargetOpcode::G_TRUNC, MI.getOperand(1).getReg())) {
      DEBUG(dbgs() << ".. Combine MI: " << MI;);
      unsigned DstReg = MI.getOperand(0).getReg();
      unsigned SrcReg = DefMI->getOperand(1).getReg();
      Builder.setInstr(MI);
      // We get a copy/trunc/extend depending on the sizes
      Builder.buildAnyExtOrTrunc(DstReg, SrcReg);
      markInstAndDefDead(MI, *DefMI, DeadInsts);
      return true;
    }
    return tryFoldImplicitDef(MI, DeadInsts);
  }

  bool tryCombineZExt(MachineInstr &MI,
                      SmallVectorImpl<MachineInstr *> &DeadInsts) {

    if (MI.getOpcode() != TargetOpcode::G_ZEXT)
      return false;
    if (MachineInstr *DefMI =
            getOpcodeDef(TargetOpcode::G_TRUNC, MI.getOperand(1).getReg())) {
      unsigned DstReg = MI.getOperand(0).getReg();
      LLT DstTy = MRI.getType(DstReg);
      if (isInstUnsupported(TargetOpcode::G_AND, DstTy) ||
          isInstUnsupported(TargetOpcode::G_CONSTANT, DstTy))
        return false;
      DEBUG(dbgs() << ".. Combine MI: " << MI;);
      Builder.setInstr(MI);
      unsigned ZExtSrc = MI.getOperand(1).getReg();
      LLT ZExtSrcTy = MRI.getType(ZExtSrc);
      APInt Mask = APInt::getAllOnesValue(ZExtSrcTy.getSizeInBits());
      auto MaskCstMIB = Builder.buildConstant(DstTy, Mask.getZExtValue());
      unsigned TruncSrc = DefMI->getOperand(1).getReg();
      // We get a copy/trunc/extend depending on the sizes
      auto SrcCopyOrTrunc = Builder.buildAnyExtOrTrunc(DstTy, TruncSrc);
      Builder.buildAnd(DstReg, SrcCopyOrTrunc, MaskCstMIB);
      markInstAndDefDead(MI, *DefMI, DeadInsts);
      return true;
    }
    return tryFoldImplicitDef(MI, DeadInsts);
  }

  bool tryCombineSExt(MachineInstr &MI,
                      SmallVectorImpl<MachineInstr *> &DeadInsts) {

    if (MI.getOpcode() != TargetOpcode::G_SEXT)
      return false;
    if (MachineInstr *DefMI =
            getOpcodeDef(TargetOpcode::G_TRUNC, MI.getOperand(1).getReg())) {
      unsigned DstReg = MI.getOperand(0).getReg();
      LLT DstTy = MRI.getType(DstReg);
      if (isInstUnsupported(TargetOpcode::G_SHL, DstTy) ||
          isInstUnsupported(TargetOpcode::G_ASHR, DstTy) ||
          isInstUnsupported(TargetOpcode::G_CONSTANT, DstTy))
        return false;
      DEBUG(dbgs() << ".. Combine MI: " << MI;);
      Builder.setInstr(MI);
      unsigned SExtSrc = MI.getOperand(1).getReg();
      LLT SExtSrcTy = MRI.getType(SExtSrc);
      unsigned SizeDiff = DstTy.getSizeInBits() - SExtSrcTy.getSizeInBits();
      auto SizeDiffMIB = Builder.buildConstant(DstTy, SizeDiff);
      unsigned TruncSrcReg = DefMI->getOperand(1).getReg();
      // We get a copy/trunc/extend depending on the sizes
      auto SrcCopyExtOrTrunc = Builder.buildAnyExtOrTrunc(DstTy, TruncSrcReg);
      auto ShlMIB = Builder.buildInstr(TargetOpcode::G_SHL, DstTy,
                                       SrcCopyExtOrTrunc, SizeDiffMIB);
      Builder.buildInstr(TargetOpcode::G_ASHR, DstReg, ShlMIB, SizeDiffMIB);
      markInstAndDefDead(MI, *DefMI, DeadInsts);
      return true;
    }
    return tryFoldImplicitDef(MI, DeadInsts);
  }

  /// Try to fold sb = EXTEND (G_IMPLICIT_DEF sa) -> sb = G_IMPLICIT_DEF
  bool tryFoldImplicitDef(MachineInstr &MI,
                          SmallVectorImpl<MachineInstr *> &DeadInsts) {
    unsigned Opcode = MI.getOpcode();
    if (Opcode != TargetOpcode::G_ANYEXT && Opcode != TargetOpcode::G_ZEXT &&
        Opcode != TargetOpcode::G_SEXT)
      return false;

    if (MachineInstr *DefMI = getOpcodeDef(TargetOpcode::G_IMPLICIT_DEF,
                                           MI.getOperand(1).getReg())) {
      unsigned DstReg = MI.getOperand(0).getReg();
      LLT DstTy = MRI.getType(DstReg);
      if (isInstUnsupported(TargetOpcode::G_IMPLICIT_DEF, DstTy))
        return false;
      DEBUG(dbgs() << ".. Combine EXT(IMPLICIT_DEF) " << MI;);
      Builder.setInstr(MI);
      Builder.buildInstr(TargetOpcode::G_IMPLICIT_DEF, DstReg);
      markInstAndDefDead(MI, *DefMI, DeadInsts);
      return true;
    }
    return false;
  }

  bool tryCombineMerges(MachineInstr &MI,
                        SmallVectorImpl<MachineInstr *> &DeadInsts) {

    if (MI.getOpcode() != TargetOpcode::G_UNMERGE_VALUES)
      return false;

    unsigned NumDefs = MI.getNumOperands() - 1;
    unsigned SrcReg = MI.getOperand(NumDefs).getReg();
    MachineInstr *MergeI = MRI.getVRegDef(SrcReg);
    if (!MergeI || (MergeI->getOpcode() != TargetOpcode::G_MERGE_VALUES))
      return false;

    const unsigned NumMergeRegs = MergeI->getNumOperands() - 1;

    if (NumMergeRegs < NumDefs) {
      if (NumDefs % NumMergeRegs != 0)
        return false;

      Builder.setInstr(MI);
      // Transform to UNMERGEs, for example
      //   %1 = G_MERGE_VALUES %4, %5
      //   %9, %10, %11, %12 = G_UNMERGE_VALUES %1
      // to
      //   %9, %10 = G_UNMERGE_VALUES %4
      //   %11, %12 = G_UNMERGE_VALUES %5

      const unsigned NewNumDefs = NumDefs / NumMergeRegs;
      for (unsigned Idx = 0; Idx < NumMergeRegs; ++Idx) {
        SmallVector<unsigned, 2> DstRegs;
        for (unsigned j = 0, DefIdx = Idx * NewNumDefs; j < NewNumDefs;
             ++j, ++DefIdx)
          DstRegs.push_back(MI.getOperand(DefIdx).getReg());

        Builder.buildUnmerge(DstRegs, MergeI->getOperand(Idx + 1).getReg());
      }

    } else if (NumMergeRegs > NumDefs) {
      if (NumMergeRegs % NumDefs != 0)
        return false;

      Builder.setInstr(MI);
      // Transform to MERGEs
      //   %6 = G_MERGE_VALUES %17, %18, %19, %20
      //   %7, %8 = G_UNMERGE_VALUES %6
      // to
      //   %7 = G_MERGE_VALUES %17, %18
      //   %8 = G_MERGE_VALUES %19, %20

      const unsigned NumRegs = NumMergeRegs / NumDefs;
      for (unsigned DefIdx = 0; DefIdx < NumDefs; ++DefIdx) {
        SmallVector<unsigned, 2> Regs;
        for (unsigned j = 0, Idx = NumRegs * DefIdx + 1; j < NumRegs;
             ++j, ++Idx)
          Regs.push_back(MergeI->getOperand(Idx).getReg());

        Builder.buildMerge(MI.getOperand(DefIdx).getReg(), Regs);
      }

    } else {
      // FIXME: is a COPY appropriate if the types mismatch? We know both
      // registers are allocatable by now.
      if (MRI.getType(MI.getOperand(0).getReg()) !=
          MRI.getType(MergeI->getOperand(1).getReg()))
        return false;

      for (unsigned Idx = 0; Idx < NumDefs; ++Idx)
        MRI.replaceRegWith(MI.getOperand(Idx).getReg(),
                           MergeI->getOperand(Idx + 1).getReg());
    }

    markInstAndDefDead(MI, *MergeI, DeadInsts);
    return true;
  }

  /// Try to combine away MI.
  /// Returns true if it combined away the MI.
  /// Adds instructions that are dead as a result of the combine
  /// into DeadInsts, which can include MI.
  bool tryCombineInstruction(MachineInstr &MI,
                             SmallVectorImpl<MachineInstr *> &DeadInsts) {
    switch (MI.getOpcode()) {
    default:
      return false;
    case TargetOpcode::G_ANYEXT:
      return tryCombineAnyExt(MI, DeadInsts);
    case TargetOpcode::G_ZEXT:
      return tryCombineZExt(MI, DeadInsts);
    case TargetOpcode::G_SEXT:
      return tryCombineSExt(MI, DeadInsts);
    case TargetOpcode::G_UNMERGE_VALUES:
      return tryCombineMerges(MI, DeadInsts);
    case TargetOpcode::G_TRUNC: {
      bool Changed = false;
      for (auto &Use : MRI.use_instructions(MI.getOperand(0).getReg()))
        Changed |= tryCombineInstruction(Use, DeadInsts);
      return Changed;
    }
    }
  }

private:
  /// Mark MI as dead. If a def of one of MI's operands, DefMI, would also be
  /// dead due to MI being killed, then mark DefMI as dead too.
  void markInstAndDefDead(MachineInstr &MI, MachineInstr &DefMI,
                          SmallVectorImpl<MachineInstr *> &DeadInsts) {
    DeadInsts.push_back(&MI);
    if (MRI.hasOneUse(DefMI.getOperand(0).getReg()))
      DeadInsts.push_back(&DefMI);
  }
  /// Checks if the target legalizer info has specified anything about the
  /// instruction, or if unsupported.
  bool isInstUnsupported(unsigned Opcode, const LLT &DstTy) const {
    auto Action = LI.getAction({Opcode, 0, DstTy});
    return Action.first == LegalizerInfo::LegalizeAction::Unsupported ||
           Action.first == LegalizerInfo::LegalizeAction::NotFound;
  }
  /// See if Reg is defined by an single def instruction that is
  /// Opcode. Also try to do trivial folding if it's a COPY with
  /// same types. Returns null otherwise.
  MachineInstr *getOpcodeDef(unsigned Opcode, unsigned Reg) {
    auto *DefMI = MRI.getVRegDef(Reg);
    auto DstTy = MRI.getType(DefMI->getOperand(0).getReg());
    if (!DstTy.isValid())
      return nullptr;
    while (DefMI->getOpcode() == TargetOpcode::COPY) {
      unsigned SrcReg = DefMI->getOperand(1).getReg();
      auto SrcTy = MRI.getType(SrcReg);
      if (!SrcTy.isValid() || SrcTy != DstTy)
        break;
      DefMI = MRI.getVRegDef(SrcReg);
    }
    return DefMI->getOpcode() == Opcode ? DefMI : nullptr;
  }
};

} // namespace llvm
