#ifndef _STDARG_H
#define _STDARG_H

#ifdef _PNUT_CC
#define VAR_ARGS
#else
#define VAR_ARGS ,...
#endif

typedef char *va_list;

#define va_start(ap,last) ap = ((char *)&(last)) + ((sizeof(last)+3)&~3)
#define va_arg(ap,type) (ap += (sizeof(type)+3)&~3, *(type *)(ap - ((sizeof(type)+3)&~3)))
#define va_end(ap)

#endif
