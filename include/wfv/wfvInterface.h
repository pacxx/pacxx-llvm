/**
 * @file   wfvInterface.h
 * @date   28.03.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */
#ifndef _WFVINTERFACE_H
#define	_WFVINTERFACE_H

//----------------------------------------------------------------------------//
// Necessary definitions for library compilation.
//----------------------------------------------------------------------------//

#if defined(_WIN32)

    #if !defined(WFV_STATIC_LIBS)
        #define WFV_DLL_EXPORT __declspec(dllexport)
        #define WFV_DLL_IMPORT __declspec(dllimport)
    #else
        #define WFV_DLL_EXPORT
        #define WFV_DLL_IMPORT
    #endif

#else

    // POSIX, etc.
    #define WFV_DLL_EXPORT
    #define WFV_DLL_IMPORT

#endif

#if defined(WFV_LIB)
    #define WFV_API WFV_DLL_EXPORT
#else
    #define WFV_API WFV_DLL_IMPORT
#endif

//----------------------------------------------------------------------------//
//----------------------------------------------------------------------------//

// LLVM forward declarations.
namespace llvm {
class Module;
class Argument;
class GlobalVariable;
class LLVMContext;
class Function;
class BasicBlock;
class Instruction;
class TargetTransformInfo;
class Value;
class Type;
class TimerGroup;
}

// Forward declaration of internal implementation.
class WFVInfo;

using namespace llvm;


namespace WFVInterface {

WFV_API class WFVInterface {
public:
    WFV_API WFVInterface(Module*         module,
                         LLVMContext*    context,
                         Function* scalarFunction,
                         Function*       simdFunction,
                         TargetTransformInfo* TTI,
                         const unsigned  vectorizationFactor,
                         const int       maskPosition=-1,
                         const bool      disableMemAccessAnalysis=false,
                         const bool      disableControlFlowDivAnalysis=false,
                         const bool      disableAllAnalyses=false,
                         const bool      verbose=false);

    WFV_API ~WFVInterface();

    // Parameter 'maskPosition' is expected to start at 0 for first argument.
    WFV_API bool addSIMDMapping(const Function& scalarFunction,
                                const Function& simdFunction,
                                const int       maskPosition,
                                const bool      mayHaveSideEffects);

    WFV_API bool addCommonMappings(const bool useSSE,
                                   const bool useSSE41,
                                   const bool useSSE42,
                                   const bool useAVX,
                                   const bool useNEON);

    WFV_API bool addSIMDSemantics(const Function& f,
                                  const bool      isOpUniform,
                                  const bool      isOpVarying,
                                  const bool      isOpSequential,
                                  const bool      isOpSequentialGuarded,
                                  const bool      isResultUniform,
                                  const bool      isResultVector,
                                  const bool      isResultScalars,
                                  const bool      isAligned,
                                  const bool      isIndexSame,
                                  const bool      isIndexConsecutive);

    WFV_API bool addSIMDSemantics(const Argument& arg,
                                  const bool      isResultUniform,
                                  const bool      isResultVector,
                                  const bool      isResultScalars,
                                  const bool      isAligned,
                                  const bool      isIndexSame,
                                  const bool      isIndexConsecutive);

    WFV_API bool addSIMDSemantics(const GlobalVariable& var,
                                  const bool         isOpUniform,
                                  const bool         isOpVarying,
                                  const bool         isOpSequential,
                                  const bool         isOpSequentialGuarded,
                                  const bool         isResultUniform,
                                  const bool         isResultVector,
                                  const bool         isResultScalars,
                                  const bool         isAligned,
                                  const bool         isIndexSame,
                                  const bool         isIndexConsecutive);


    WFV_API bool addSIMDSemantics(const Instruction& inst,
                                  const bool         isOpUniform,
                                  const bool         isOpVarying,
                                  const bool         isOpSequential,
                                  const bool         isOpSequentialGuarded,
                                  const bool         isResultUniform,
                                  const bool         isResultVector,
                                  const bool         isResultScalars,
                                  const bool         isAligned,
                                  const bool         isIndexSame,
                                  const bool         isIndexConsecutive);

    WFV_API bool analyze();

    WFV_API bool run();

    WFV_API bool clearAnalysisMetadata(Function* f);

private:
    TimerGroup* mTimerGroup;
    WFVInfo*    mInfo;

    // Should never be called directly but only through run() and analyze().
    bool analyzeFunction(Function* scalarFn, Function* simdFn);

    // Should never be called directly but only through run() (for exception handling).
    bool vectorizeFunction();

    bool verifyFunctionSignaturesMatch(const Function& f, const Function& f_SIMD);
    bool verifyVectorizedType(Type* scalarType, Type* vecType);
};


} // namespace WFVInterface


#endif	/* _WFVINTERFACE_H */
