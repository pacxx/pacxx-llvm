#pragma once

#include "../Pass.h"
#include "../IR/InstVisitor.h"
#include "../IR/Function.h"
#include "../IR/Module.h"
#include "../IR/Constants.h"
#include <vector>
#include <set>
#include <map>

namespace llvm {
Pass *createPACXXReflectionPass();
Pass *createPACXXReflectionCleanerPass();
Pass *createPACXXReflectionRemoverPass();
Pass *createPACXXSpirPass();
Pass *createPACXXNvvmPass();
Pass *createPACXXNvvmRegPass(bool runtime = false);
Pass *createPACXXSPIRVectorFixerPass(size_t offset = 0);
Pass *createPACXXStaticEvalPass();
Pass *createPACXXVerifier();
Pass *createPACXXInlinerPass();
Pass *createPACXXDeadCodeElimPass();
Pass *createPACXXTargetSelectPass(const SmallVector<std::string, 2>&);
Pass *createPACXXGEPPass();

// native backend passes
Pass *createPACXXAddrSpaceTransformPass();
Pass *createPACXXIdRemoverPass();
Pass *createSPMDVectorizerPass();
Pass *createPACXXLivenessAnalyzerPass();
Pass *createPACXXNativeBarrierPass();
Pass *createPACXXNativeSMPass();
Pass *createPACXXNativeLinkerPass();
Pass *createPACXXIntrinsicSchedulerPass();
Pass *createPACXXSelectEmitterPass();
Pass *createPACXXIntrinsicMapperPass();
}

namespace pacxx {
using namespace llvm;
using namespace std;



template <typename PTy = std::pair<Function *, int>>
PTy getTagedFunction(MDNode *MD, StringRef desc) {

  if (MD->getNumOperands() == 3) // Function*, "descriptor", i32 value
    if (auto *str = dyn_cast<MDString>(MD->getOperand(1)))
      if (MD->getOperand(0) != nullptr &&
          ((desc == "") || (str->getString() == desc)))
        if (auto *F = dyn_cast<Function>( dyn_cast<ValueAsMetadata>(MD->getOperand(0).get())->getValue()) ) {
          if (auto *Val = dyn_cast<ConstantInt>( dyn_cast<ValueAsMetadata>(MD->getOperand(2).get())->getValue()) )
            return PTy(F, *(Val->getValue().getRawData()));

          assert(false && "third operand on MDNode is not i32Ty");
        }

  return PTy(nullptr, -1);
}




template <typename CTy = std::set<Function *>>
CTy getTagedFunctions(Module *M, StringRef twine, StringRef desc) {
  CTy functions;
  if (auto MD = M->getNamedMetadata(twine)) {
    for (unsigned i = 0; i != MD->getNumOperands(); ++i) {
      auto p = getTagedFunction(MD->getOperand(i), desc);
      if (p.first)
        functions.insert(p.first);
    }
  }
  return functions;
}

template <typename CTy = std::set<std::pair<Function *, int>>>
CTy getTagedFunctionsWithTag(Module *M, StringRef twine, StringRef desc) {
  CTy functions;
  if (auto MD = M->getNamedMetadata(twine)) {
    for (unsigned i = 0; i != MD->getNumOperands(); ++i) {
      auto p = getTagedFunction(MD->getOperand(i), desc);
      if (p.first)
        functions.insert(p);
    }
  }
  return functions;
}

template <typename CTy = std::set<Function *>>
CTy getKernels(Module *M) {
  auto functions = getTagedFunctions(M, "nvvm.anntations", "kernel");
  for (auto& F : *M)
    if (F.getMetadata("kernel_arg_addr_space") != nullptr)
      functions.insert(&F);
  return functions;
}

}
