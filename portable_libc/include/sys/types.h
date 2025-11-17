#ifndef _SYS_TYPES_H
#define _SYS_TYPES_H

#if defined(PNUT_EXE_32) || defined(PNUT_SH)

typedef int ssize_t;
typedef int size_t;
typedef int off_t;

#else

typedef long ssize_t;
typedef unsigned long size_t;
typedef long off_t;

#endif

#endif
