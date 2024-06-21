#ifndef _SYS_TYPES_H
#define _SYS_TYPES_H

#ifdef PNUT_CC

#define ssize_t int
#define size_t int
#define off_t int

#else

typedef long ssize_t;
typedef unsigned long size_t;
typedef long long off_t;

#endif

#endif
