// tests for #if* preprocessor directive

void putdigit(int n) {
  putchar('0' + n);
  putchar('\n');
}

void main() {

// Basic if works
#if 1
  putdigit(1);
#endif

// else works
#if 0
  putdigit(0);
#else
  putdigit(1);
#endif

// else doesn't execute if any block before did
#if 0
  putdigit(0);
#elif 1
  putdigit(1);
#else
  putdigit(0);
#endif

// Only first elif block that's true executes
#if 0
  putdigit(0);
#elif 1
  putdigit(1);
#elif 1
  putdigit(0);
#endif

// defined operator works
#define FOO
#if defined(FOO)
  putdigit(1);
#else
  putdigit(0);
#endif

#undef FOO
#if defined(FOO)
  putdigit(0);
#else
  putdigit(1);
#endif

// if and ifdef can be used together
#ifdef FOO
  putdigit(0);
#elif defined(FOO)
  putdigit(0);
#else
  putdigit(1);
#endif

// Test operator precedence
#if 1 + 2 * 3 == 7
  putdigit(1);
#else
  putdigit(0);
#endif

#if 1 + 2 * 3 != 7
  putdigit(0);
#else
  putdigit(1);
#endif

#if 1 + 2 * 3 < 7
  putdigit(0);
#else
  putdigit(1);
#endif

#define BUFSIZE 10000
#if defined BUFSIZE && BUFSIZE >= 1024
  putdigit(1);
#else
  putdigit(0);
#endif

#if 'A' == 65
  putdigit(1);
#else
  putdigit(0);
#endif

#if NOT_DEF == 0
  putdigit(1);
#else
  putdigit(0);
#endif


#define TCC_ARM_EABI 1
#if defined(TCC_ARM_EABI) && !defined(CONFIG_TCC_ELFINTERP)
  putdigit(1);
#else
  putdigit(0);
#endif

#define __FreeBSD__
#if !defined(TCC_TARGET_PE) && (defined(__FreeBSD__) || defined(__FreeBSD_kernel__))
  putdigit(1);
#else
  putdigit(0);
#endif
}
