#include "include/setjmp.h"

int setjmp(jmp_buf env) {
  return 0;
}

void longjmp(jmp_buf env, int val) {
}
