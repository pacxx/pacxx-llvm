add_llvm_library(LLVMPACXXTransforms
  pacxx_static_analyser.cpp
  pacxx_reflection.cpp
  pacxx_reflection_cleaner.cpp
  pacxx_verifier.cpp
  pacxx_inliner.cpp
  pacxx_dce.cpp
  Log.cpp
  Common.cpp
  )

add_dependencies(LLVMPACXXTransforms intrinsics_gen)
