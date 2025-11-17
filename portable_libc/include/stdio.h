#ifndef _STDIO_H
#define _STDIO_H

#ifdef FLAT_INCLUDES
#include <types.h>
#else
#include <sys/types.h>
#endif

#include <stdarg.h>

#ifndef EOF
#define EOF (-1)
#endif

#ifndef NULL
#define NULL 0
#endif

typedef int FILE;

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

FILE *fopen(const char *pathname, const char *mode);
FILE *fdopen(int fd, const char *mode);
int fclose(FILE *stream);

int fgetc(FILE *stream);
int fputc(int c, FILE *stream);
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
int fputs(const char *s, FILE *stream);
int puts(const char *s);

size_t fread(void *buffer, size_t size, size_t count, FILE *stream);
int fseek( FILE* stream, long offset, int origin );
long ftell( FILE* stream );
int remove(const char *_Filename);

int vfprintf(FILE *stream, const char *format, va_list ap);
int fprintf(FILE *stream, const char *format, ...);
int printf(const char *format, ...);
int fflush(FILE *stream);
int sscanf(const char *str, const char *format, ...);

int vsnprintf(char *str, size_t size, const char *format, va_list ap);
int snprintf(char *str, size_t size, const char *format, ...);
int sprintf(char *str, const char *format, ...);

#endif
