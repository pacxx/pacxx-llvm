add_llvm_library(LLVMRV

  config.cpp
  init.cpp
  PlatformInfo.cpp
  registerPasses.cpp
  report.cpp
  rvConfig.cpp
  rv.cpp
  sleefLibrary.cpp
  vectorizationInfo.cpp
  VectorizationInfoProxyPass.cpp
  vectorMapping.cpp
  vectorShape.cpp

  analysis/BranchDependenceAnalysis.cpp
  analysis/DFG.cpp
  analysis/DivPathDecider.cpp
  analysis/reductionAnalysis.cpp
  analysis/VectorizationAnalysis.cpp

  native/MemoryAccessGrouper.cpp
  native/NatBuilder.cpp
  native/ShuffleBuilder.cpp
  native/Utils.cpp

  region/LoopRegion.cpp
  region/Region.cpp

  transform/bosccTransform.cpp
  transform/CNSPass.cpp
  transform/divLoopTrans.cpp
  transform/irPolisher.cpp
  transform/Linearizer.cpp
  transform/loopExitCanonicalizer.cpp
  transform/LoopVectorizer.cpp
  transform/maskExpander.cpp
  transform/redOpt.cpp
  transform/redTools.cpp
  transform/remTransform.cpp
  transform/srovTransform.cpp
  transform/structOpt.cpp

  transform/cns/BlockGraph.cpp
  transform/cns/CNS.cpp
  transform/cns/CNSScoring.cpp
  transform/cns/llvmDomination.cpp
  transform/cns/llvmDuplication.cpp
  transform/cns/SplitTree.cpp

  utils/maskGraphUtils.cpp
  utils/rvTools.cpp
)

add_dependencies(LLVMRV intrinsics_gen)
