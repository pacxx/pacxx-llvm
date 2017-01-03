/**
 * @file   valueInfoMap.h
 * @date   19.07.2012
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2012 Saarland University
 *
 */
#ifndef _VALUEINFOMAP_H
#define	_VALUEINFOMAP_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/ValueMapper.h"

using namespace llvm;

namespace WFV {

class ValueInfo {
public:
    const bool mIsOpUniform;
    const bool mIsOpVarying;
    const bool mIsOpSequential;
    const bool mIsOpSequentialGuarded;
    const bool mIsResultUniform;
    const bool mIsResultVector;
    const bool mIsResultScalars;
    const bool mIsAligned;
    const bool mIsIndexSame;
    const bool mIsIndexConsecutive;

    explicit ValueInfo(const ValueInfo& other);
    ValueInfo(const bool isOpUniform,
              const bool isOpVarying,
              const bool isOpSequential,
              const bool isOpSequentialGuarded,
              const bool isResultUniform,
              const bool isResultVector,
              const bool isResultScalars,
              const bool isAligned,
              const bool isIndexSame,
              const bool isIndexConsecutive);

    ~ValueInfo();
};

class ValueInfoMap {
private:
    typedef DenseMap<const Value*, const ValueInfo*> ValueMapType;
    ValueMapType mValueMap;

public:
    ValueInfoMap();
    explicit ValueInfoMap(const ValueInfoMap& other);
    ~ValueInfoMap();

    typedef ValueMapType::iterator       iterator;
    typedef ValueMapType::const_iterator const_iterator;

    iterator       begin();
    const_iterator begin() const;
    iterator       end();
    const_iterator end() const;

    bool empty() const;

    bool add(const Value& value,
             const bool   isOpUniform,
             const bool   isOpVarying,
             const bool   isOpSequential,
             const bool   isOpSequentialGuarded,
             const bool   isResultUniform,
             const bool   isResultVector,
             const bool   isResultScalars,
             const bool   isAligned,
             const bool   isIndexSame,
             const bool   isIndexConsecutive);

    bool hasMapping(const Value& value) const;
    const ValueInfo& get(const Value& value) const;

    void mapValueInformation(ValueToValueMapTy& valueMap);

    void print(raw_ostream& o) const;
};

}

#endif	/* _VALUEINFOMAP_H */

