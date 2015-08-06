#include <assert.h>

static inline mirObject mirIntTo(int x) {
    return (mirObject){MIRT_INT, {.asInteger = x}};
}

static inline mirObject mirIntAdd(mirObject x, mirObject y) {
    assert(x.tag == MIRT_INT && y.tag == MIRT_INT);
    return (mirObject){MIRT_INT, {.asInteger = x.value.asInteger+y.value.asInteger}};
}

static inline mirObject mirIntMult(mirObject x, mirObject y) {
    assert(x.tag == MIRT_INT && y.tag == MIRT_INT);
    return (mirObject){MIRT_INT, {.asInteger = x.value.asInteger*y.value.asInteger}};
}

static inline mirObject mirIntDiv(mirObject x, mirObject y) {
    assert(x.tag == MIRT_INT && y.tag == MIRT_INT);
    return (mirObject){MIRT_INT, {.asInteger = x.value.asInteger/y.value.asInteger}};
}

