/*
 * analysisCfg.h
 *
 *  Created on: Feb 16, 2015
 *      Author: simon
 */

#ifndef SRC_ANALYSIS_ANALYSISCFG_H_
#define SRC_ANALYSIS_ANALYSISCFG_H_

// #define PF_VERBOSE

// #define VA_VERIFY
// Debug output. Generate paths in new disjoint paths test.
// Enables the legacy(== reference) code paths based on path enumeration and runs the new algorithm against them

#ifdef VA_VERIFY
#define WFV_ENABLE_LEGACY_API
// enables the path enumeration API
// do not use this API for any new code!
#endif
#endif /* SRC_ANALYSIS_ANALYSISCFG_H_ */
