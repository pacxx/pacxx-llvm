/**
 * @file   wfvConfig.h
 * @date   30.03.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */
#ifndef _WFVCONFIG_H
#define	_WFVCONFIG_H

// variables defined by corresponding compiler
// gcc     = __GNUC__
// icc     = __INTEL_COMPILER
// msvc    = _MSC_VER
// llvm    = __llvm__
// borland = __BORLANDC__

// variables defined by corresponding operating system
// windows (general) = _WIN32
// windows 64bit     = _WIN64
// linux             = __linux
// mac os            = __APPLE__ & __MACH__ (icc & gcc)

// http://predef.sourceforge.net/


//----------------------------------------------------------------------------//
// WholeFunctionVectorizer configuration defines
// These should be set by the build script
//----------------------------------------------------------------------------//

// Enable silent mode (no output at all)
// (default: deactivated)
//#define WFV_SILENT_MODE

// Ignore alignment analysis and only generate aligned loads/stores.
// This may lead to segmentation faults.
// (default: deactivated)
//#define WFV_FORCE_ALIGNED_MEMOPS

// Ignore alignment analysis and only generate unaligned loads/stores.
// (default: deactivated)
//#define WFV_FORCE_UNALIGNED_MEMOPS

//----------------------------------------------------------------------------//
// debug flags
//----------------------------------------------------------------------------//

// DEBUG flag has to be set by compiler
#if defined(DEBUG) || defined(_DEBUG)
#   define DEBUG_WHOLEFUNCTIONVECTORIZATION
#endif

//if NDEBUG is defined, always ignore debug information
#ifdef NDEBUG
#   undef DEBUG_WHOLEFUNCTIONVECTORIZATION
#endif


// debug macros
// do while and ((void)0) are used to enforce semicolon
// DEBUG_WFV_VISIBLE allows arbitrary code that does not have its own scope
// NOTE: a boolean 'mVerbose' has to be in scope in order to use this ;)
#ifdef DEBUG_WHOLEFUNCTIONVECTORIZATION
#   define DEBUG_WFV_NO_VERBOSE(x) do { x } while (0)
#   define DEBUG_WFV(x) \
    do { \
        assert (mInfo && "don't use DEBUG_WFV makro after WFVInfo was destroyed!"); \
        if (mInfo->mVerbose) { x } \
    } while (0)
#   define DEBUG_WFV_VISIBLE(x) x ((void)0)
#else
#   define DEBUG_WFV_NO_VERBOSE(x) ((void)0)
#   define DEBUG_WFV(x) ((void)0)
#   define DEBUG_WFV_VISIBLE(x) ((void)0)
#endif

// VectorizationAnalysis should remain independent of WFVInfo, so the verbose flag is
// stored separately
#ifdef DEBUG_WHOLEFUNCTIONVECTORIZATION
#   define DEBUG_VA_NO_VERBOSE(x) do { x } while (0)
#   define DEBUG_VA(x) \
    do { \
        if (mVerbose) { x } \
    } while (0)
#   define DEBUG_VA_VISIBLE(x) x ((void)0)
#else
#   define DEBUG_VA_NO_VERBOSE(x) ((void)0)
#   define DEBUG_VA(x) ((void)0)
#   define DEBUG_VA_VISIBLE(x) ((void)0)
#endif

//----------------------------------------------------------------------------//
// Misc
//----------------------------------------------------------------------------//

// Use this macro to silence warnings for variables that are
// only used in assertions.
#define WFV_UNUSED(x) ((void)(x))


// use ShuffleInsts to broadcast operands
// #define WFV_USE_SHUFFLE_BROADCAST


#endif // _WFVCONFIG_H
