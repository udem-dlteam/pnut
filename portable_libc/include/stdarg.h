#ifndef _STDARG_H
#define _STDARG_H

#ifdef PNUT_CC

#define VAR_ARGS , VA_LIST

#define va_list int

#define va_start(ap,last) ap = VA_LIST
#define va_arg(ap,type) ap
#define va_end(ap) ap=ap

#else

#define VAR_ARGS , ...

#ifdef TCC

typedef char *va_list;

#define va_ptr_size      (sizeof(void *))
#define va_word_align(x) (((x)+va_ptr_size-1)&~(va_ptr_size-1))

#define va_start(ap,last) ap = ((char *)&(last)) + va_word_align(sizeof(last))
#define va_arg(ap,type) (ap += va_word_align(sizeof(type)), *(type *)(ap - va_word_align(sizeof(type))))
#define va_end(ap)

#else

#undef _STDARG_H
#include <stdarg.h>

#endif

#endif

#endif
