add_llvm_library(LLVMRV

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

  analysis/ABAAnalysis.cpp
  analysis/BranchDependenceAnalysis.cpp
  analysis/DFG.cpp
  analysis/MandatoryAnalysis.cpp
  analysis/maskAnalysis.cpp
  analysis/reductionAnalysis.cpp
  analysis/VectorizationAnalysis.cpp

  native/MemoryAccessGrouper.cpp
  native/NatBuilder.cpp
  native/ShuffleBuilder.cpp
  native/Utils.cpp

  region/LoopRegion.cpp
  region/Region.cpp

  transform/irPolisher.cpp
  transform/Linearizer.cpp
  transform/loopExitCanonicalizer.cpp
  transform/LoopVectorizer.cpp
  transform/maskGenerator.cpp
  transform/srovTransform.cpp
  transform/structOpt.cpp

  utils/maskGraphUtils.cpp
  utils/rvTools.cpp
)

add_dependencies(LLVMRV intrinsics_gen)
