#ifndef _STDIO_H
#define _STDIO_H

#include "sys/types.h"
#include "stdarg.h"

#ifndef EOF
#define EOF (-1)
#endif

#ifndef NULL
#define NULL 0
#endif

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

int getchar(void);
int putchar(int);

int fgetc(FILE *stream);
FILE *fopen(const char *pathname, const char *mode);
FILE *fdopen(int fd, const char *mode);
int fclose(FILE *stream);

int fputc(int c, FILE *stream);
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
int fputs(const char *s, FILE *stream);
int puts(const char *s);

size_t fread(void *buffer, size_t size, size_t count, FILE *stream);
int fseek( FILE* stream, long offset, int origin );
long ftell( FILE* stream );
int remove(const char *_Filename);

int vfprintf(FILE *stream, const char *format, va_list ap);
int fprintf(FILE *stream, const char *format VAR_ARGS);
int printf(const char *format VAR_ARGS);
int fflush(FILE *stream);
int sscanf(const char *_Src, const char *_Format, ...);

int vsnprintf(char *str, size_t size, const char *format, va_list ap);
int snprintf(char *str, size_t size, const char *format VAR_ARGS);
int sprintf(char *str, const char *format VAR_ARGS);

#include "../src/stdio.c"

#endif
