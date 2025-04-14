#ifndef _INTTYPES_H
#define _INTTYPES_H 1

typedef unsigned char uint8_t;
typedef signed char int8_t;
typedef unsigned short uint16_t;
typedef short int16_t;
typedef unsigned uint32_t;
typedef int int32_t;

typedef unsigned long long uint64_t;
typedef long long int64_t;

#if __x86_64__
typedef unsigned long long uintptr_t;
#elif __i386__
typedef unsigned int uintptr_t;
#endif

#endif
