// Test that varargs functions work

#include <stdio.h>

#ifdef PNUT_CC

typedef char *va_list;

#define va_start(ap,last) ap = ((char *)&(last)) + ((sizeof last+3)&~3)
#define va_arg(ap,type) (ap += (sizeof(type)+3)&~3, *(type *)(ap - ((sizeof(type)+3)&~3)))
#define va_end(ap)

#else

#include <stdarg.h>

#endif

void putint_aux(int n, int base) {
  int d = n % base;
  int top = n / base;
  if (n == 0) return;
  putint_aux(top, base);
  putchar("0123456789abcdef"[d & 15]);
}

void putint(int n, int base) {
  if (n < 0) {
    putchar('-');
    putint_aux(-n, base);
  } else {
    putint_aux(n, base);
  }
}

void putstr(char *s) {
  while (*s) {
    putchar(*s);
    s++;
  }
}

// A simple version of printf that supports %d, %c, %x, %s
void simple_printf(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  while (*fmt) {
    if (*fmt == '%') {
      fmt++;
      switch (*fmt) {
        case 'd':
          putint(va_arg(ap, int), 10);
          break;
        case 'c':
          putchar(va_arg(ap, int));
          break;
        case 'x':
          putint(va_arg(ap, int), 16);
          break;
        case 's':
          putstr(va_arg(ap, char *));
          break;
        default:
          putchar(*fmt);
          break;
      }
    } else {
      putchar(*fmt);
    }
    fmt++;
  }
  va_end(ap);
}


int main() {
  simple_printf("Hello %s! %d %c %x\n", "world", 42, 'A', 255);
  return 0;
}
