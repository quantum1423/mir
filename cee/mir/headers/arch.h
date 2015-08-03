#include <stdint.h>
#if UINTPTR_MAX == 0xffffffff
typedef float flonum
#elif UINTPTR_MAX == 0xffffffffffffffff
typedef double flonum
#else
/* wtf */
#endif
