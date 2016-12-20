/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2013-2014
*/

#ifndef LLVM_TRANSFORM_PACXX_CALLVISITOR_H
#define LLVM_TRANSFORM_PACXX_CALLVISITOR_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <vector>
#include <set>

using namespace llvm;
using namespace std;


namespace pacxx
{
    template <typename L>
    class CallVisitor : public InstVisitor < CallVisitor<L> >
    {
    public:
        CallVisitor(L transform) : _transform(transform) {}

        void visitCallInst(CallInst& I)
        {
            if (Function* F = I.getCalledFunction())
            {
				if (find(begin(visited), end(visited), F) == end(visited)) // prevent endless recursion
				{
					visited.insert(F);
					this->visit(F);
				}
            }

            _transform(&I);
        }

        set<Function*> get() { return visited; }
        void clear() { visited.clear(); }

    private:
        set<Function*> visited;
        L _transform;
    };

    template<typename L>
    auto make_CallVisitor(L func)
    {
        return CallVisitor<L>(func);
    }
}

#endif
