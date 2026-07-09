// 64-bit compound assignment and pre-increment/decrement. The lvalue must be
// evaluated exactly once, so a side-effecting lvalue (`*p++`, `a[i++]`) runs its
// side effects a single time -- a regression test for the read-modify-write
// lowering (a naive `lhs = lhs op rhs` rewrite would evaluate the lvalue twice).
#include "int64_util.h"

int main() {
  long long x = 100;

  // Plain compound assignment, and its value as a sub-expression.
  x += 5;            show64(x);            // 105
  x -= 200;          show64(x);            // ffffffff ffffffa1  (-95)
  x = 3;
  show64(x *= 7);    show64(x);            // 21 ; 21  (value of `x *= 7` is 21)
  x = -64;
  show64(x >>= 2);                         // ffffffff fffffff0  (-16, arithmetic)
  x = 1;
  x <<= 40;          show64(x);            // 10000000000

  // Pre-increment / decrement.
  x = 0; ++x; ++x;   show64(x);            // 2
  --x;               show64(x);            // 1

  // Side-effecting lvalue evaluated once: pointer with post-increment.
  long long arr[3];
  arr[0] = 10; arr[1] = 20; arr[2] = 30;
  long long *p = arr;
  *p++ += 5;                               // arr[0] = 15, p advances once
  show64(arr[0]);                          // 15
  show64(arr[1]);                          // 20  (untouched)
  showint((int)(p - arr));                 // 1   (advanced exactly once)

  // Side-effecting lvalue evaluated once: array index with post-increment.
  int i = 0;
  arr[i++] <<= 4;                          // arr[0] = 15 << 4 = 240, i = 1
  show64(arr[0]);                          // f0  (240)
  showint(i);                              // 1
  return 0;
}
