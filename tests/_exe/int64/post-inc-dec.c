// Post-increment/decrement on long long values: the expression's value is
// the value *before* the update, unlike pre-inc/dec.
#include "int64_util.h"

long long identity64(long long x) { return x; }

int main() {
  long long a = 5;
  show64(a++); // 5  (old value)
  show64(a);   // 6  (updated)
  show64(a--); // 6
  show64(a);   // 5

  // crossing the 32-bit boundary
  long long b = 0xFFFFFFFFLL;
  show64(b++); // ffffffff
  show64(b);   // 100000000
  show64(b--); // 100000000
  show64(b);   // ffffffff

  // used by reference (call argument), not just by value
  long long c = 10;
  show64(identity64(c++)); // a
  show64(c);               // b

  // side effects of the lvalue run once
  long long arr[2];
  arr[0] = 1; arr[1] = 2;
  int i = 0;
  arr[i++]++; // increments arr[0], i becomes 1 (only one evaluation of i)
  showint(i);      // 1
  show64(arr[0]);  // 2
  show64(arr[1]);  // 2

  // combined in an expression
  long long s = 0;
  long long x = 3;
  s = x++ + x++; // 3 + 4 = 7, x ends at 5
  show64(s);     // 7
  show64(x);     // 5

  return 0;
}
