#pragma once
// Implements closure stuff.

// A closure is represented as a function pointer, and an environment.
// The environment is simply a list of "mirObject"s.
// The function pointer is called with 4 arguments:
//    - pointer to dynamic context
//    - the environment
//    - the number of arguments
//    - an array of all the arguments 
#include <assert.h>

typedef mirObject (*mirRawFun)(mirContext*, struct _mirObject*, int, struct _mirObject*);

#define mirFunCall(fun, n, arr) __mirFunCall(fun, _Internal_dynamicContext, n, arr)

static inline mirObject __mirFunCall(mirObject fun, mirContext *ctx,
                                     int argc, mirObject *argv) {
    assert(fun.tag == MIRT_FUN);
    mirRawFun foo = fun.value.asFunction->code;
    return foo(ctx, fun.value.asFunction->env, argc, argv);
}

mirObject mirFunTo(mirRawFun code) {
    mirObject toret;
    toret.tag = MIRT_FUN;
    toret.value.asFunction = GC_MALLOC(sizeof(*toret.value.asFunction));
    toret.value.asFunction->code = code;
    return toret;
}
