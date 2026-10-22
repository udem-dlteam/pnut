// The operands of the `#` and `##` operators are *not* macro-expanded: when a
// macro argument is used as an operand of `#` or `##`, the raw tokens of the
// argument are used, while the same argument used anywhere else in the
// replacement list is expanded.  An argument that is only used as an operand of
// `#` or `##` (or not used at all) is never expanded.
//
// "Raw" means raw at the place where the macro is invoked: when a macro
// forwards an argument to another macro, the tokens it forwards are the ones
// its own argument expanded to, and the inner macro pastes or stringifies those.

// putchar
#include <stdio.h>

int str2 = 7;
int str22 = 2;
int ab = 5;
int X_ab = 3;
int num = 4;
int num1 = 6;

#define str str2
#define DEEP 99

// `X` is pasted with `Y` before either operand is expanded
#define T(X, Y) X ## Y

// `x` is used both as a `##` operand (raw) and as a regular token (expanded)
#define BOTH(x) ((x) + (X ## _ ## x))

#define STR(x) #x

// An unused argument is never expanded: NEEDS_2_ARGS is not invoked with too
// few arguments here.
#define NEEDS_2_ARGS(a, b) 0
#define EAT(x) 1

// `str` is expanded by PASSTHRU/Q, so the operand that INNER and STR see is `str2`
#define INNER(y) T(y, 2)
#define PASSTHRU(x) INNER(x)
#define Q(x) STR(x)

void putdigit(int n) {
  putchar('0' + n);
  putchar('\n');
}

void putstring(char * s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
  putchar('\n');
}

void main() {
  putdigit(T(str, 2));          // str ## 2     = str2  = 7
  putdigit(T(num, 1));          // num ## 1     = num1  = 6
  putdigit(BOTH(ab));           // ab + X_ab    = 8
  putdigit(EAT(NEEDS_2_ARGS())); // 1
  putdigit(PASSTHRU(str));      // str2 ## 2    = str22 = 2
  // Note: pnut only stringifies arguments that are a single token
  putstring(STR(DEEP));         // "DEEP", not "99"
  putstring(STR(str));          // "str", not "str2"
  putstring(Q(str));            // "str2", the argument that STR is given
}
