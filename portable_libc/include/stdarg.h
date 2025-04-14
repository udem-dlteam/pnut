#ifndef _STDARG_H
#define _STDARG_H

typedef char *va_list;

#define va_ptr_size      (sizeof(void *))
#define va_word_align(x) (((x)+va_ptr_size-1)&~(va_ptr_size-1))

#define va_start(ap,last) ap = ((char *)&(last)) + va_word_align(sizeof(last))
#define va_arg(ap,type) (ap += va_word_align(sizeof(type)), *(type *)(ap - va_word_align(sizeof(type))))
#define va_end(ap)
#define va_copy(d,s) ((d) = (s))

// int vprintf(const char *format, va_list ap);
// int vfprintf(FILE *stream, const char *format, va_list ap);
// int vsnprintf(char *str, size_t size, const char *format, va_list ap);

// int vscanf(const char *format, va_list ap);
// int vsscanf(const char *str, const char *format, va_list ap);
// int vfscanf(FILE *stream, const char *format, va_list ap);

#endif
