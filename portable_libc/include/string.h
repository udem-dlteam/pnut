#include "sys/types.h"

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
size_t fread(void *buffer, size_t size, size_t count, FILE *stream);
int fseek( FILE* stream, long offset, int origin );
long ftell( FILE* stream );
int remove(const char *_Filename);
char *strstr (char *__haystack, char *__needle);

#include "../src/string.c"
