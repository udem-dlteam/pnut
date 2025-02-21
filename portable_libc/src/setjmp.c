#include "../include/setjmp.h"

int setjmp(jmp_buf env) {
  return 0; // TCC tests this value and doesn't call longjmp if it's 0
}

// longjmp should never be called, so we comment it out so pnut stubs it with a
// function that produces an error.
// void longjmp(jmp_buf env, int val) {
//
// }
