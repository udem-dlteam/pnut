// tests for object-like macro

// putchar
#include <stdio.h>

#define EMPTY
#define FOO 1

// Nested macros
#define BAR FOO
#define BAZ BAR
#define QUX BAZ
#define QUUX QUX
#define CORGE QUUX
#define GRULT CORGE
#define GARPLY GRULT

#define ADD (1 + 1)
#define MUL (ADD * ADD)
#define DIV (MUL / ADD)
#define SUB (DIV - ADD)

// An object-like macro that looks like a function-like macro.
// The syntactical difference between PARENS_PARENS_EXPR2 being a function-like
// macro and an object-like macro is the space between the macro name and the
// opening parenthesis.
// We define the macros in reverse order so that the nature of PARENS_EXPR is
// not known when PARENS_PARENS_EXPR2 is defined.
#define PARENS_PARENS_EXPR2 (PARENS_EXPR, PARENS_EXPR)
#define PARENS_PARENS_EXPR (PARENS_EXPR)
#define PARENS_EXPR (1 + 1)

#define float int // We can redefine keywords

void putdigit(int n) {
  putchar('0' + n);
  putchar('\n');
}

void main() {
  float foo_val; // not a float, but an int

  putdigit(EMPTY + 8); // Will expand to + 8
  putdigit(FOO);
  putdigit(GARPLY);
// Changing the value of FOO will change the expansion of any macros using it
#define FOO 2
  putdigit(FOO);
  putdigit(GARPLY);

  putdigit(ADD);
  putdigit(MUL);
  putdigit(DIV);
  putdigit(SUB);

  putdigit(PARENS_EXPR);
  putdigit(PARENS_PARENS_EXPR);
  putdigit(PARENS_PARENS_EXPR2);
  foo_val = FOO
  #define FOO 3 // This will not change the value of foo_val
  ;

  putdigit(foo_val);
}
