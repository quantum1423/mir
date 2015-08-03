#pragma once
// Implements closure stuff.

// A closure is represented as a function pointer, and an environment.
// The environment is simply a list of "mirObject"s.
// The function pointer is called with 4 arguments:
//    - pointer to dynamic context
//    - the environment
//    - the number of arguments
//    - an array of all the arguments 
#include "mirContext.h"
#include <assert.h>

#define mirFunCall(fun, n, arr) __mirFunCall(fun, _Internal_dynamicContext, n, arr)

static inline mirObject __mirFunCall(mirObject fun, mirContext *ctx,
                                     int argc, mirObject *argv) {
    assert(fun.tag == MIRT_FUN);
    mirObject (*foo)(mirContext*, mirObject*, int, mirObject*) = fun.value.asFunction->code;
    return foo(ctx, fun.value.asFunction->env, argc, argv);
}
