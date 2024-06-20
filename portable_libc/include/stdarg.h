#ifndef _STDARG_H
#define _STDARG_H

#ifdef TODO

typedef char *va_list;

#define va_start(ap,last) ap = ((char *)&(last)) + ((sizeof(last)+3)&~3)
#define va_arg(ap,type) (ap += (sizeof(type)+3)&~3, *(type *)(ap - ((sizeof(type)+3)&~3)))
#define va_end(ap)

#else

#undef _STDARG_H
#include <stdarg.h>

#define VAR_ARGS ,...

#endif

#endif
