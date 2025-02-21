#ifndef _SETJMP_H
#define _SETJMP_H

#define jmp_buf int

int setjmp(jmp_buf env);
void longjmp(jmp_buf env, int val);

#include "../src/setjmp.c"

#endif
