#ifndef _STDLIB_H
#define _STDLIB_H

#include "sys/types.h"

void *malloc(size_t size);
void free(void *ptr);
void *realloc(void *ptr, size_t size);

double strtod(const char *str, char **endptr);
float strtof(const char *str, char **endptr);
long double strtold(const char *str, char **endptr);
unsigned long long strtoull(const char *nptr, char **endptr, int base);
long long strtoll(const char *nptr, char **endptr, int base);
unsigned long strtoul(const char *nptr, char **endptr, int base);

long int strtol(const char *str, char **endptr, int base);
int atoi(const char *str);
char *getenv(const char *name);

void qsort(void *base, size_t nmemb, size_t size, int (*compar)(void *, void *));

char *realpath(const char * path, char * resolved_path);

#include "../src/stdlib.c"

#endif
