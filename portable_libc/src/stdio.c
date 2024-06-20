#include "include/stdio.h"

#include "include/stdlib.h"
#include "include/unistd.h"

FILE _stdin_FILE;
FILE _stdout_FILE;
FILE _stderr_FILE;
FILE _string_FILE;

FILE *stdin = &_stdin_FILE;
FILE *stdout = &_stdout_FILE;
FILE *stderr = &_stderr_FILE;

int _get_fd(FILE *f) {
  if (f == &_stdin_FILE) {
    return 0;
  } else if (f == &_stdout_FILE) {
    return 1;
  } else if (f == &_stderr_FILE) {
    return 2;
  } else {
    return f->fd;
  }
}

FILE *fopen(const char *pathname, const char *mode) {
  return 0; /*TODO*/
}

FILE *fdopen(int fd, const char *mode) {
  FILE *result = malloc(sizeof(FILE));
  if (result) {
    result->fd = fd;
  }
  return result;
}

int fclose(FILE *stream) {
  return 0; /*TODO*/
}

int fputc(int c, FILE *stream) {
  int fd = _get_fd(stream);
  if (fd == -1) {
    if (stream->string_output_len+1 < stream->string_output_buf_size) {
      stream->string_output_buf[stream->string_output_len] = c;
      ++stream->string_output_len;
    }
  } else {
    stream->buf[0] = c;
    write(fd, stream->buf, 1);
  }
  return c;
}

size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
  const char *p = ptr;
  size_t n = size * nmemb;
  while (n) {
    fputc(*p, stream);
    ++p;
    --n;
  }
  return nmemb;
}

int _fputstr(const char *str, FILE *stream) {
  int result = 0;
  while (*str) {
    fputc(*str, stream);
    ++str;
    ++result;
  }
  return result;
}

int fputs(const char *s, FILE *stream) {
  _fputstr(s, stream);
  fputc('\n', stream);
  return 0;
}

int puts(const char *s) {
  return fputs(s, stdout);
}

#define SIZEOF_NUM_BUF 100
char _num_buf[SIZEOF_NUM_BUF];
char *_digits = "0123456789abcdef";

char *_int_to_str(int n, int base, int width, int force_0, int force_plus) {

  char *out = _num_buf + SIZEOF_NUM_BUF;
  int neg = n < 0;
  int has_sign = neg || force_plus;

  if (!neg) n = -n;

  *--out = 0;

  while (n <= -base) {
    *--out = _digits[-(n % base)];
    n /= base;
  }

  *--out = _digits[-(n % base)];

  if (width >= SIZEOF_NUM_BUF) width = SIZEOF_NUM_BUF-1;

  if (force_0) {
    while (_num_buf + SIZEOF_NUM_BUF - out <= width - has_sign) {
      *--out = force_0 ? '0' : ' ';
    }
  }

  if (has_sign) *--out = neg ? '-' : '+';

  if (!force_0) {
    while (_num_buf + SIZEOF_NUM_BUF - out <= width) {
      *--out = ' ';
    }
  }

  return out;
}

int vfprintf(FILE *stream, const char *format, va_list ap) {

  char c;
  int result = 0;
  int base;
  int width;
  int force_0;

  while ((c = *format++)) {
    if (c == '%' && (c = *format++) != '%') {
      width = 0;
      force_0 = 0;
      if (c == '0') {
        force_0 = 1;
        c = *format++;
      }
      while (c >= '0' && c <= '9') {
        width = width*10 + (c - '0');
        c = *format++;
      }
      while (c == 'l' || c == 'L') {
        c = *format++;
      }
      switch (c) {
      case 'c':
        fputc(va_arg(ap, int), stream);
        result += 1;
        break;
      case 'd':
      case 'u':
      case 'o':
      case 'x':
        base = c == 'x' ? 16 : c == 'o' ? 8 : 10;
        result += _fputstr(_int_to_str(va_arg(ap, int), base, width, force_0, 0), stream);
        break;
      case 's':
        result += _fputstr(va_arg(ap, char*), stream);
        break;
      default:
        fputc('?', stream);
        fputc('?', stream);
        fputc('?', stream);
        result += 3;
      }
    } else {
      fputc(c, stream);
      result += 1;
    }
  }

  return result;
}

int fprintf(FILE *stream, const char *format VAR_ARGS) {

  va_list ap;
  int result;

  va_start (ap, format);
  result = vfprintf(stream, format, ap);
  va_end(ap);

  return result;
}

int printf(const char *format VAR_ARGS) {

  va_list ap;
  int result;

  va_start (ap, format);
  result = vfprintf(stdout, format, ap);
  va_end(ap);

  return result;
}

int vsnprintf(char *str, size_t size, const char *format, va_list ap) {

  int result;

  _string_FILE.fd = -1;
  _string_FILE.string_output_buf = str;
  _string_FILE.string_output_buf_size = size;
  _string_FILE.string_output_len = 0;
  result = vfprintf(&_string_FILE, format, ap);
  _string_FILE.string_output_buf[_string_FILE.string_output_len] = 0; /* null terminate string */

  return result;
}

int snprintf(char *str, size_t size, const char *format VAR_ARGS) {

  va_list ap;
  int result;

  va_start (ap, format);
  result = vsnprintf(str, size, format, ap);
  va_end(ap);

  return result;
}

int sprintf(char *str, const char *format VAR_ARGS) {

  va_list ap;
  int result;

  va_start (ap, format);
  result = vsnprintf(str, 999999999, format, ap);
  va_end(ap);

  return result;
}
