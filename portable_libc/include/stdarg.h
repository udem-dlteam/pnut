#ifndef _STDARG_H
#define _STDARG_H

#define VAR_ARGS , ...

typedef char *va_list;

#define va_start(ap,last) ap = ((char *)&(last)) + ((sizeof(last)+3)&~3)
#define va_arg(ap,type) (ap += (sizeof(type)+3)&~3, *(type *)(ap - ((sizeof(type)+3)&~3)))
#define va_end(ap)
#define va_copy(d,s) ((d) = (s))

#endif
