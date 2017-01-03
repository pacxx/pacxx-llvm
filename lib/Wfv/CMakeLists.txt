add_llvm_library(LLVMWFV

    wfvInfo.cpp
    wfvInterface.cpp
    wfvRegionBased.cpp

    utils/functionInfoMap.cpp
    utils/maskGraphUtils.cpp
    utils/metadata.cpp
    utils/valueInfoMap.cpp
    utils/wfvTools.cpp

    analysis/linearizationVerifier.cpp
    analysis/loopLiveValueAnalysis.cpp
    analysis/maskAnalysis.cpp
    analysis/PathFinder.cpp
    analysis/sccAnalyzer.cpp
    analysis/SketchGraph.cpp
    analysis/vectorizationAnalysis.cpp
    analysis/vectorizationPrereqVerifier.cpp
    analysis/vectorizationVerifier.cpp

    variantRegions/utils.cpp
    variantRegions/disableVectorizationRegion.cpp
    variantRegions/sequentializeRegion.cpp
    variantRegions/variantRegion.cpp
    variantRegions/variantRegionFactory.cpp

    functionPasses/cfgLinearizer.cpp
    functionPasses/dummy.cpp
    functionPasses/functionVectorizer.cpp
    functionPasses/loopExitCanonicalizer.cpp
    functionPasses/maskGenerator.cpp
    functionPasses/selectGenerator.cpp

    functionPasses/functionVectorization/broadcastUniformOperands.cpp
    functionPasses/functionVectorization/duplicateSplitInstructions.cpp
    functionPasses/functionVectorization/generatePackUnpackCode.cpp
    functionPasses/functionVectorization/generateSideEffectGuards.cpp
    functionPasses/functionVectorization/insertPackUnpackIntrinsics.cpp
    functionPasses/functionVectorization/optimizePackUnpackOperations.cpp
    functionPasses/functionVectorization/utils.cpp
    functionPasses/functionVectorization/vectorizeInstructions.cpp
)

add_dependencies(LLVMWFV intrinsics_gen)
