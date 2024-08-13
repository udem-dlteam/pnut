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

#define va_start(ap,last) ap = ((char *)&(last)) + ((sizeof(last)+3)&~3)
#define va_arg(ap,type) (ap += (sizeof(type)+3)&~3, *(type *)(ap - ((sizeof(type)+3)&~3)))
#define va_end(ap)

#else

#undef _STDARG_H
#include <stdarg.h>

#endif

#endif

#endif
