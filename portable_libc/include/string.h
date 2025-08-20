#ifndef _STRING_H

#ifdef FLAT_INCLUDES
#include <types.h>
#else
#include <sys/types.h>
#endif

void *memset(void *dest, int c, size_t n);
void *memcpy(void *dest, const void *src, size_t n);
void *memmove(void *dest, const void *src, size_t n);
int memcmp(const void *vl, const void *vr, size_t n);

size_t strlen(const char *s);
char *strcpy(char *dest, const char *src);
char *strncpy(char *dest, const char *src, size_t n);
char *strcat(char *dest, const char *src);
char *strchr(const char *s, int c);
char *strrchr(const char *s, int c);
int strcmp(const char *l, const char *r);

char *strerror(int errnum);
int strncmp(const char *s1, const char *s2, size_t n);
char *strpbrk(const char *dest, const char *breakset);
char *strstr(char const *haystack, char const *needle);
void *memmem(void const *haystack, size_t haystack_len, void const *needle, size_t needle_len);

#endif
