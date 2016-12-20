add_llvm_library(LLVMPACXXTransforms
  pacxx_native_linker.cpp
  pacxx_static_analyser.cpp
  pacxx_reflection.cpp
  pacxx_reflection_cleaner.cpp
  pacxx_classify.cpp
  pacxx_verifier.cpp
  pacxx_inliner.cpp
  pacxx_dce.cpp
  spir.cpp
  nvvm.cpp
  nvvm_reg.cpp
  spir_vector_fixer.cpp
  )

add_dependencies(LLVMPACXXTransforms intrinsics_gen)
