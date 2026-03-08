// libtcc contains functions that implements arithmetic operations not supported
// natively by some architectures. This is TCC's equivalent of GCC's libgcc.
// A lot of the operations here are not implemented correctly, or only work for
// a certain subset of inputs, but that's good enough to bootstrap TCC.

long long
__divdi3 (long long a, long long b)
{
  return (long) a / (long) b;
}

unsigned long long
__udivdi3 (unsigned long long a, unsigned long long b)
{
  return (unsigned long) a / (unsigned long) b;
}

unsigned long long
__umoddi3 (unsigned long long a, unsigned long long b)
{
  return (unsigned long) a % (unsigned long) b;
}

unsigned long long
__lshrdi3 (unsigned long long a, long b)
{
  return a;
}

long long
__ashldi3 (long long a, long b)
{
  for (int i = 0; i < b; i++)
    a += a;
  return a;
}

long double
__floatundixf (unsigned long long a)
{
  return 0;
}

unsigned long long
__fixunsxfdi (double a1)
{
  return 0;
}

long
__fixdfdi (double a1)
{
  return 0;
}

// long long
// __moddi3 (long long a, long long b)
// {
//   return (long) a % (long) b;
// }
//
// long long
// __ashrdi3 (long long a, long b)
// {
//   return a;
// }
//
// int
// __fixunsdfsi (int a, int b)
// {
//   return 0;
// }
