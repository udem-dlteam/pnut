#include "../include/stdio.h"
#include "../include/stdlib.h"
#include "../include/unistd.h"
#include "../include/fcntl.h"

FILE _standard_files[4];

FILE *stdin = _standard_files+0;
FILE *stdout = _standard_files+1;
FILE *stderr = _standard_files+2;

char *_string_output_buf;
size_t _string_output_buf_size;
size_t _string_output_len;

int _get_fd(FILE *f) {
  if (f == _standard_files+0) {
    return 0;
  } else if (f == _standard_files+1) {
    return 1;
  } else if (f == _standard_files+2) {
    return 2;
  } else {
    return *f;
  }
}

int fopen_flags(const char *mode) {
  int res = 0;
  while (*mode == 'b') ++mode; // Ignore binary mode

  if (*mode == 'r') {
    res = O_RDONLY;
  } else if (*mode == 'w') {
    res = O_WRONLY | O_CREAT | O_TRUNC;
  } else if (*mode == 'a') {
    res = O_WRONLY | O_CREAT | O_APPEND;
  } else {
    return 0;
  }

  mode += 1;
  while (*mode == 'b') ++mode; // Ignore binary mode (it can appear after r, w, a)

  if (*mode == '+') {
    res = res & ~(O_RDONLY | O_WRONLY) | O_RDWR;
  }

  return res;
}

FILE *fdopen(int fd, const char *mode) {
  FILE *result = malloc(sizeof(FILE));
  if (result) {
    *result = fd;
  }
  return result;
}

FILE *fopen(const char *pathname, const char *mode) {
  int fd = open(pathname, fopen_flags(mode), 0666); // 0666 is the default mode
  if (fd == -1) {
    return 0;
  } else {
    return fdopen(fd, mode);
  }
}

int fclose(FILE *stream) {
  int fd = _get_fd(stream);
  free(stream);
  if (close(fd) == 0) {
    return 0;
  } else {
    return EOF;
  }
}

int fseek(FILE* stream, long offset, int origin) {
  return lseek(_get_fd(stream), offset, origin);
}

off_t ftell(FILE * stream) {
  return lseek (_get_fd(stream), 0, SEEK_CUR);
}

size_t fread(void *data, size_t size, size_t count, FILE *stream) {
  return read(_get_fd(stream), data, size * count) / size;
}

int remove(const char *filename) {
  return unlink(filename);
}

char _output_buf[1];

int fputc(int c, FILE *stream) {
  int fd = _get_fd(stream);
  if (fd == -1) {
    if (_string_output_len+1 < _string_output_buf_size) {
      _string_output_buf[_string_output_len] = c;
      ++_string_output_len;
    }
  } else {
    _output_buf[0] = c;
    write(fd, _output_buf, 1);
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
  // fputc('\n', stream);
  return 0;
}

int puts(const char *s) {
  return fputs(s, stdout);
}

int fflush(FILE *stream) {
  return 0; // no buffering so nothing to do
}

#define SIZEOF_NUM_BUF 100
char _num_buf[SIZEOF_NUM_BUF];

char *_int_to_str(int n, int base, int width, int force_0, int force_plus, int left_justify) {

  char *out = _num_buf + SIZEOF_NUM_BUF;
  int neg = n < 0;
  int has_sign = neg || force_plus;
  char *digits = "0123456789abcdef";

  if (!neg) n = -n;

  *--out = 0;

  while (n <= -base) {
    *--out = digits[-(n % base)];
    n /= base;
  }

  *--out = digits[-(n % base)];

  if (width >= SIZEOF_NUM_BUF) width = SIZEOF_NUM_BUF-1;

  if (left_justify) {
    if (has_sign) *--out = neg ? '-' : '+';

    // We need to add padding with spaces to the right of the number, but the
    // number is already to the right of the buffer, so we need to move it to
    // the left first.
    // Here, the number occupies (_num_buf + SIZEOF_NUM_BUF - out) bytes, and
    // is moved width - (_num_buf + SIZEOF_NUM_BUF - out) bytes to the left.
    // The number is then padded with spaces to the right.
    int num_len = _num_buf + SIZEOF_NUM_BUF - out - 1;
    int move_len = width - num_len;
    char *dst = _num_buf + SIZEOF_NUM_BUF - width;
    char *src = out;
    // Move the number to the left
    while (src < _num_buf + SIZEOF_NUM_BUF - 1) {
      *dst = *src;
      ++dst;
      ++src;
    }

    out = _num_buf + SIZEOF_NUM_BUF - width;
    while (dst < _num_buf + SIZEOF_NUM_BUF) {
      *dst = ' ';
      ++dst;
    }
  } else {
    // When padding with 0, the sign is to the left of the padding
  if (force_0) {
    while (_num_buf + SIZEOF_NUM_BUF - out <= width - has_sign) {
        *--out = '0';
    }
  }

  if (has_sign) *--out = neg ? '-' : '+';

    // When padding with spaces, the sign is to the right of the padding
  if (!force_0) {
    while (_num_buf + SIZEOF_NUM_BUF - out <= width) {
      *--out = ' ';
      }
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
  int force_sign;
  int left_justify;

  while ((c = *format++)) {
    if (c == '%' && (c = *format++) != '%') {
      width = 0;
      force_0 = 0;
      force_sign = 0;
      left_justify = 0;
      if (c == '0') {
        force_0 = 1;
        c = *format++;
      }
      if (c == '+') {
        force_sign = 1;
        c = *format++;
      }
      if (c == '-') {
        left_justify = 1;
        c = *format++;
      }
      while (c >= '0' && c <= '9') {
        width = width*10 + (c - '0');
        c = *format++;
      }
      while (c == 'l' || c == 'L') {
        c = *format++;
      }
      if (c == 'c') {
        fputc(va_arg(ap, int), stream);
        result += 1;
      } else if (c == 'd' || c == 'u' || c == 'o' || c == 'x') {
        base = c == 'x' ? 16 : c == 'o' ? 8 : 10;
        result += _fputstr(_int_to_str(va_arg(ap, int), base, width, force_0, force_sign, left_justify), stream);
      } else if (c == 's') {
        result += _fputstr(va_arg(ap, char*), stream);
      } else {
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

  va_start(ap, format);
  result = vfprintf(stream, format, ap);
  va_end(ap);

  return result;
}

int printf(const char *format VAR_ARGS) {

  va_list ap;
  int result;

  va_start(ap, format);
  result = vfprintf(stdout, format, ap);
  va_end(ap);

  return result;
}

int vsnprintf(char *str, size_t size, const char *format, va_list ap) {

  int result;

  _standard_files[3] = -1; /* mark as string FILE */
  _string_output_buf = str;
  _string_output_buf_size = size;
  _string_output_len = 0;
  result = vfprintf(_standard_files+3, format, ap);
  _string_output_buf[_string_output_len] = 0; /* null terminate string */

  return result;
}

int snprintf(char *str, size_t size, const char *format VAR_ARGS) {

  va_list ap;
  int result;

  va_start(ap, format);
  result = vsnprintf(str, size, format, ap);
  va_end(ap);

  return result;
}

int sprintf(char *str, const char *format VAR_ARGS) {

  va_list ap;
  int result;

  va_start(ap, format);
  result = vsnprintf(str, 999999999, format, ap);
  va_end(ap);

  return result;
}
