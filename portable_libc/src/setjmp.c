#include <pnut_lib.h>
#include <setjmp.h>

int setjmp(jmp_buf env) {
  return 0; // TCC tests this value and doesn't call longjmp if it's 0
}

#ifdef ADD_LIBC_STUB

void longjmp(jmp_buf env, int val) {
  pnut_abort("longjmp not implemented");
}

#endif
