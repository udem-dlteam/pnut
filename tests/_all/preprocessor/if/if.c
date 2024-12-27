// tests for #if* preprocessor directive
#include <stdio.h>

void putdigit(int n) {
  putchar('0' + n);
  putchar('\n');
}

int main() {

// Basic if works
#if 1
  putdigit(1);
#endif

// else works
#if 0
  putdigit(0);
#else
  putdigit(2);
#endif

// else doesn't execute if any block before did
#if 0
  putdigit(0);
#elif 1
  putdigit(3);
#else
  putdigit(0);
#endif

// Only first elif block that's true executes
#if 0
  putdigit(0);
#elif 1
  putdigit(4);
#elif 1
  putdigit(0);
#endif

// defined operator works
#define FOO
#if defined(FOO)
  putdigit(5);
#else
  putdigit(0);
#endif

#undef FOO
#if defined(FOO)
  putdigit(0);
#else
  putdigit(6);
#endif

// if and ifdef can be used together
#ifdef FOO
  putdigit(0);
#elif defined(FOO)
  putdigit(0);
#else
  putdigit(7);
#endif

// Test operator precedence
#if 1 + 2 * 3 == 7
  putdigit(8);
#else
  putdigit(0);
#endif

#if 1 + 2 * 3 != 7
  putdigit(0);
#else
  putdigit(9);
#endif

#if 1 + 2 * 3 < 7
  putdigit(0);
#else
  putdigit(1);
#endif

#if ~(0) + 1 == 0
  putdigit(1);
#else
  putdigit(0);
#endif

#define BUFSIZE 10000
#if defined BUFSIZE && BUFSIZE >= 1024
  putdigit(2);
#else
  putdigit(0);
#endif

#if 'A' == 65
  putdigit(3);
#else
  putdigit(0);
#endif

#if NOT_DEF == 0
  putdigit(4);
#else
  putdigit(0);
#endif

#define ARCH_i386 24
#if ARCH_i386
  putdigit(5);
#else
  putdigit(0);
#endif

#define TCC_ARM_EABI 1
#if defined(TCC_ARM_EABI) && !defined(CONFIG_TCC_ELFINTERP)
  putdigit(6);
#else
  putdigit(0);
#endif

#define __FreeBSD__
#if !defined(TCC_TARGET_PE) && (defined(__FreeBSD__) || defined(__FreeBSD_kernel__))
  putdigit(7);
#else
  putdigit(0);
#endif

  return 0;
}
