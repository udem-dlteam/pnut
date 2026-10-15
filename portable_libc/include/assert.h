#ifndef _ASSERT_H
#define _ASSERT_H 1

// Minimal assert.h for the pnut libc. Unlike the standard one, the assertion
// can't be disabled by defining NDEBUG, because TCC's x86-64 backend is
// compiled with the pnut libc and depends on assert() being defined.
#include <stdlib.h>

#define assert(cond) if (!(cond)) abort()

#endif
