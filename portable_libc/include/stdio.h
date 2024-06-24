#ifndef _STDIO_H
#define _STDIO_H

#include "include/sys/types.h"
#include "include/stdarg.h"

#ifdef USE_STRUCT

typedef struct {
  int fd;
  char buf[1];
  char *string_output_buf;
  size_t string_output_buf_size;
  size_t string_output_len;
} FILE;

#else

typedef int FILE;

#endif

#ifdef PNUT_CC

FILE *stdin;
FILE *stdout;
FILE *stderr;

#else

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

#endif

FILE *fopen(const char *pathname, const char *mode);
FILE *fdopen(int fd, const char *mode);
int fclose(FILE *stream);

int fputc(int c, FILE *stream);
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
int fputs(const char *s, FILE *stream);
int puts(const char *s);

int vfprintf(FILE *stream, const char *format, va_list ap);
int fprintf(FILE *stream, const char *format VAR_ARGS);
int printf(const char *format VAR_ARGS);

int vsnprintf(char *str, size_t size, const char *format, va_list ap);
int snprintf(char *str, size_t size, const char *format VAR_ARGS);
int sprintf(char *str, const char *format VAR_ARGS);

#endif
