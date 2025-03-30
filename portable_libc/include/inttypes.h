#ifndef _INTTYPES_H
#define _INTTYPES_H 1

typedef unsigned char uint8_t;
typedef signed char int8_t;
typedef unsigned short uint16_t;
typedef short int16_t;
typedef unsigned uint32_t;
typedef int int32_t;
#ifdef PNUT_EXE_64
typedef unsigned long long uint64_t;
typedef long long int64_t;
#endif // PNUT_EXE_64

#endif
