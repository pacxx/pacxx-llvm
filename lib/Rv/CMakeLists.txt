set(CMAKE_MODULE_PATH
  ${CMAKE_MODULE_PATH}
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake"
)

include("rv_macros")

OPTION(RV_ENABLE_SLEEF "Generate RV's builtin vector-math library" ON)
OPTION(RV_ENABLE_CRT "Implement complex math functions using BC-compiler compiler-rt" OFF)
OPTION(RV_DEBUG "Enable verbose debug output and expensive internal checks." OFF)


# check enabled targets
IF (X86 IN_LIST LLVM_TARGETS_TO_BUILD)
  SET(RV_ENABLE_X86 On)
ELSE()
  SET(RV_ENABLE_X86 Off)
ENDIF()

IF (AArch64 IN_LIST LLVM_TARGETS_TO_BUILD)
  SET(RV_ENABLE_ADVSIMD On)
ELSE()
  SET(RV_ENABLE_ADVSIMD Off)
ENDIF()

if (RV_ENABLE_X86)
  add_definitions( "-DRV_ENABLE_X86" )
ENDIF()

if (RV_ENABLE_ADVSIMD)
  add_definitions( "-DRV_ENABLE_ADVSIMD" )
endif()

if (RV_ENABLE_SLEEF) 
  add_definitions( "-DRV_ENABLE_BUILTINS" )
  if (RV_ENABLE_CRT)
    add_definitions( "-DRV_ENABLE_CRT" )
  endif()
endif()

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

  utils/rvTools.cpp
)

add_dependencies(LLVMRV intrinsics_gen)

