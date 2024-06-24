#ifndef _STDLIB_H
#define _STDLIB_H

#include "include/sys/types.h"

void *malloc(size_t size);
void free(void *ptr);
void *realloc(void *ptr, size_t size);

#ifndef PNUT_CC
double strtod(const char *str, char **endptr);
float strtof(const char *str, char **endptr);
long double strtold(const char *str, char **endptr);
#endif

long int strtol(const char *str, char **endptr, int base);
int atoi(const char *str);

#endif
