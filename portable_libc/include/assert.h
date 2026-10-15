#ifndef _ASSERT_H
#define _ASSERT_H 1

// Minimal assert.h for the pnut libc. Unlike the standard one, the assertion
// can't be disabled by defining NDEBUG, because TCC's x86-64 backend is
// compiled with the pnut libc and depends on assert() being defined.
//
// The statement expression is wrapped in `do { } while (0)` so that assert()
// behaves as a single statement: without it, `if (c) assert(x); else ...`
// binds the `else` to the `if` inside the macro (TCC's aarch64 backend hits
// that in arm64_gen_opic(), which silently drops the shift instruction).
#include <stdlib.h>

#define assert(cond) do { if (!(cond)) abort(); } while (0)

#endif
