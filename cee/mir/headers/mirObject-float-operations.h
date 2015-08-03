static inline mirObject mirFloTo(double x) {
    return (mirObject){MIRT_FLO, {.asFloat = x}};
}

static inline mirObject mirFloAdd(mirObject x, mirObject y) {
    return (mirObject){MIRT_FLO, {.asFloat = x.value.asFloat+y.value.asFloat}};
}
