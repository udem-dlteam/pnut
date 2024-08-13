// tests for #if* preprocessor directive

void putdigit(int n) {
  putchar('0' + n);
  putchar('\n');
}

void main() {
#define FOO
#define BAR

#ifdef FOO
  putdigit(1);
#endif

#ifndef FOO
  putdigit(0);
#endif

#ifdef FOO_not
  putdigit(0);
#endif

#ifndef FOO_not
  putdigit(1);
#endif

#ifdef FOO
  putdigit(1);
#ifndef BAR
  putdigit(0);
#else
  putdigit(1);
#endif
#else
  putdigit(0);
#endif

// ifdef directive
#ifdef FOO
#endif

#ifdef FOO
#else
#endif

#ifdef FOO
#ifdef FOO
#ifdef FOO
#ifndef FOO
  putdigit(0);
#else
  putdigit(1);
#endif
#ifdef FOO
#ifdef FOO
  putdigit(1);
#endif
#endif
#endif
#endif
#endif
}
