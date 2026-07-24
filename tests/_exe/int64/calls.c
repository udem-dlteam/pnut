// Passing and returning long long, and nested calls whose temporaries must
// coexist (f(x) + f(y)).
#include "int64_util.h"

long long add64(long long a, long long b) { return a + b; }
long long dbl(long long x) { return x + x; }
long long fact(int n) {                 // returns long long, int param
  long long r = 1;
  while (n > 1) { r = r * n; n -= 1; }
  return r;
}
long long sumto(long long n) {          // recursion with long long
  if (n <= 0) return 0;
  return n + sumto(n - 1);
}

int main() {
  long long a = 0xFFFFFFFFLL, b = 1LL;
  show64(add64(a, b));                   // 100000000
  show64(dbl(a) + dbl(b));               // two live temporaries: 1fffffffe + 2 = 200000000
  show64(add64(dbl(a), dbl(b)));         // nested: 1fffffffe + 2 = 200000000
  show64(fact(21));                      // 21! = 14b3b4ca85a86c47a098a224000... low64
  show64(sumto(2000LL));                 // 2001000 = 1e8b28
  // chain of nested calls
  show64(add64(add64(1LL, 2LL), add64(3LL, 4LL)));
  return 0;
}
