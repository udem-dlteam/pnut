// tests for function-like macro

// putchar
#include <stdio.h>

// Macro with a comma in the argument
#define FST(X, Y) X
#define SND(X, Y) Y
#define ADD_PAIR(X, Y) FST(X, Y) + SND(X, Y)

// Empty function-like macro
#define EMPTY(X, Y)
// Function like macro with 0 arguments
#define THUNK() 1

int THUNK = 8;

// Higher order macro
#define HIGHER_ORDER2(F, X, Y) F(X, Y)
#define CONST(X, Y) X
#define ADD(X, Y) X + Y

// Macro arguments can unpacked by calling an auxiliary function (SETBIT)
#define SETBIT2(ADDRESS,BIT,N) ((N) ? (ADDRESS & ~(1<<BIT)) : (ADDRESS | (1<<BIT)))
#define SETBIT(F, PARAMS) F(PARAMS)
#define ADDR 5
#define CONTROL 7
#define ARGS ADDR,3,CONTROL

#define DEF(id, str, val) ,id

enum {
  ABC = 42
  DEF(def, "def",)
  DEF(ghi, "ghi",)
  DEF(jkl, "jlk",)
// Tests that the expansion of the preceding DEF uses the old DEF macro
#define DEF(id, str, val) ,id = val
  DEF(mno, "mno", 1)
  DEF(pqr, "pqr", 2)
};

#define MULTI_LINE_MACRO(X, Y) \
  FST(X, Y) + \
  SND(X, Y)
  // + SETBIT(SETBIT2, ARGS) Disabled for now

void putdigit(int n) {
  putchar('0' + n);
  putchar('\n');
}

void function_with_3_args(int a, int b, int c) {
  putdigit(a);
  putdigit(b);
  putdigit(c);
}

void main() {
  putdigit(EMPTY(1, 5) + 3);
  putdigit(EMPTY(ARGS, 5) + 3);
  putdigit(HIGHER_ORDER2(CONST, 1, 5));
  putdigit(HIGHER_ORDER2(ADD, 1, 5));
  function_with_3_args(ARGS);
  putdigit(THUNK()); // 1
  putdigit(THUNK);   // THUNK is also a variable containing 42
  putdigit(SETBIT(SETBIT2, ARGS));
  putdigit(MULTI_LINE_MACRO(1, 2));
  putdigit(def % 10);
  putdigit(ghi % 10);
  putdigit(jkl % 10);
  putdigit(mno % 10);
  putdigit(pqr % 10);
}
