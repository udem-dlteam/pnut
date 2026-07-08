// 64-bit arithmetic routines for 32-bit hosts.
// Each 64-bit value is represented as a struct with two 32-bit halves.
// These functions are called by the compiler when it encounters 64-bit
// arithmetic operations on a 32-bit host (WORD_SIZE == 4).

struct Int64 {
  unsigned int lo;
  unsigned int hi;
};

// Conversions between 64-bit values and narrower scalars (widen, narrow,
// truthiness) have no runtime functions here: the compiler emits them inline.

// ---------------------------------------------------------------------------
// Addition and subtraction (same for signed and unsigned in two's complement)
// ---------------------------------------------------------------------------

struct Int64 add_i64(struct Int64 a, struct Int64 b) {
  struct Int64 r;
  r.lo = a.lo + b.lo;
  r.hi = a.hi + b.hi + (r.lo < a.lo); // carry
  return r;
}

struct Int64 add_u64(struct Int64 a, struct Int64 b) {
  return add_i64(a, b);
}

struct Int64 sub_i64(struct Int64 a, struct Int64 b) {
  struct Int64 r;
  r.lo = a.lo - b.lo;
  r.hi = a.hi - b.hi - (a.lo < b.lo); // borrow
  return r;
}

struct Int64 sub_u64(struct Int64 a, struct Int64 b) {
  return sub_i64(a, b);
}

// ---------------------------------------------------------------------------
// Negation and bitwise NOT
// ---------------------------------------------------------------------------

struct Int64 neg_i64(struct Int64 a) {
  struct Int64 zero;
  zero.lo = 0;
  zero.hi = 0;
  return sub_i64(zero, a);
}

struct Int64 not_i64(struct Int64 a) {
  struct Int64 r;
  r.lo = ~a.lo;
  r.hi = ~a.hi;
  return r;
}

// ---------------------------------------------------------------------------
// Multiplication (lower 64 bits are the same for signed and unsigned)
// ---------------------------------------------------------------------------

struct Int64 mul_i64(struct Int64 a, struct Int64 b) {
  // We need only the lower 64 bits of a * b.
  // Decompose: a = a.hi * 2^32 + a.lo,  b = b.hi * 2^32 + b.lo
  // Lower 64 bits = (a.hi*b.lo + a.lo*b.hi)*2^32 + a.lo*b.lo  (mod 2^64)
  //
  // Compute a.lo * b.lo using 16-bit halves to stay within 32-bit arithmetic.
  unsigned int a_l = a.lo & 0xFFFF;
  unsigned int a_h = a.lo >> 16;
  unsigned int b_l = b.lo & 0xFFFF;
  unsigned int b_h = b.lo >> 16;

  unsigned int p0 = a_l * b_l; // bits [0..31]
  unsigned int p1 = a_l * b_h; // bits [16..47], need lower 32
  unsigned int p2 = a_h * b_l; // bits [16..47], need lower 32
  unsigned int p3 = a_h * b_h; // bits [32..63], need lower 32

  // mid = p1 + p2 + (p0 >> 16), tracking carry into bit 32
  unsigned int p0_hi = p0 >> 16;
  unsigned int mid   = p1 + p2;
  unsigned int carry = (mid < p1);
  mid   = mid + p0_hi;
  carry = carry + (mid < p0_hi);

  struct Int64 r;
  r.lo = (p0 & 0xFFFF) | (mid << 16);
  // hi accumulates p3, the carry from mid, the upper 16 bits of mid, and the
  // cross terms a.hi*b.lo and a.lo*b.hi (all mod 2^32).
  r.hi = p3 + (carry << 16) + (mid >> 16) + a.hi * b.lo + a.lo * b.hi;
  return r;
}

struct Int64 mul_u64(struct Int64 a, struct Int64 b) {
  return mul_i64(a, b);
}

// ---------------------------------------------------------------------------
// Bitwise operations (sign-independent)
// ---------------------------------------------------------------------------

struct Int64 and_i64(struct Int64 a, struct Int64 b) {
  struct Int64 r;
  r.lo = a.lo & b.lo;
  r.hi = a.hi & b.hi;
  return r;
}

struct Int64 and_u64(struct Int64 a, struct Int64 b) {
  return and_i64(a, b);
}

struct Int64 or_i64(struct Int64 a, struct Int64 b) {
  struct Int64 r;
  r.lo = a.lo | b.lo;
  r.hi = a.hi | b.hi;
  return r;
}

struct Int64 or_u64(struct Int64 a, struct Int64 b) {
  return or_i64(a, b);
}

struct Int64 xor_i64(struct Int64 a, struct Int64 b) {
  struct Int64 r;
  r.lo = a.lo ^ b.lo;
  r.hi = a.hi ^ b.hi;
  return r;
}

struct Int64 xor_u64(struct Int64 a, struct Int64 b) {
  return xor_i64(a, b);
}

// ---------------------------------------------------------------------------
// Shifts
// ---------------------------------------------------------------------------

struct Int64 shl_i64(struct Int64 a, struct Int64 b) {
  unsigned int n = b.lo & 63;
  struct Int64 r;
  if (n == 0) {
    r = a;
  } else if (n < 32) {
    r.lo = a.lo << n;
    r.hi = (a.hi << n) | (a.lo >> (32 - n));
  } else {
    r.lo = 0;
    r.hi = a.lo << (n - 32);
  }
  return r;
}

struct Int64 shl_u64(struct Int64 a, struct Int64 b) {
  return shl_i64(a, b);
}

// Logical (unsigned) right shift
struct Int64 shr_u64(struct Int64 a, struct Int64 b) {
  unsigned int n = b.lo & 63;
  struct Int64 r;
  if (n == 0) {
    r = a;
  } else if (n < 32) {
    r.lo = (a.lo >> n) | (a.hi << (32 - n));
    r.hi = a.hi >> n;
  } else {
    r.lo = a.hi >> (n - 32);
    r.hi = 0;
  }
  return r;
}

// Arithmetic (signed) right shift
struct Int64 shr_i64(struct Int64 a, struct Int64 b) {
  // The shift count must be a *signed* int: pnut-exe lowers `x >> n` to an
  // arithmetic shift only when both operands are signed, so an unsigned count
  // would turn the `shi >> n` below into a logical shift. (n is 0..63, so its
  // value is unaffected by the signed type.)
  int n = b.lo & 63;
  struct Int64 r;
  int shi = (int)a.hi; // treat hi as signed
  if (n == 0) {
    r = a;
  } else if (n < 32) {
    r.lo = (a.lo >> n) | ((unsigned int)shi << (32 - n));
    r.hi = (unsigned int)(shi >> n);
  } else {
    r.lo = (unsigned int)(shi >> (n - 32));
    r.hi = (unsigned int)(shi >> 31); // sign fill
  }
  return r;
}

// ---------------------------------------------------------------------------
// Unsigned division and remainder (using long division on 32-bit halves)
// ---------------------------------------------------------------------------

// Returns q such that a == q * b + r (unsigned).
// Uses a simple bit-by-bit long division.
struct Int64 div_u64(struct Int64 a, struct Int64 b) {
  struct Int64 q;
  struct Int64 r;
  struct Int64 one;
  int i = 63;
  q.lo = 0; q.hi = 0;
  r.lo = 0; r.hi = 0;
  one.lo = 1; one.hi = 0;

  while (i >= 0) {
    // r = r << 1
    r = shl_i64(r, one);
    // r.lo |= bit i of a
    if (i >= 32) {
      r.lo = r.lo | ((a.hi >> (i - 32)) & 1);
    } else {
      r.lo = r.lo | ((a.lo >> i) & 1);
    }
    // if r >= b, r -= b, q |= 1 << i
    if (r.hi > b.hi || (r.hi == b.hi && r.lo >= b.lo)) {
      r = sub_i64(r, b);
      if (i >= 32) {
        q.hi = q.hi | (1u << (i - 32));
      } else {
        q.lo = q.lo | (1u << i);
      }
    }
    i = i - 1;
  }
  return q;
}

struct Int64 rem_u64(struct Int64 a, struct Int64 b) {
  struct Int64 q;
  struct Int64 r;
  struct Int64 one;
  int i = 63;
  q.lo = 0; q.hi = 0;
  r.lo = 0; r.hi = 0;
  one.lo = 1; one.hi = 0;

  while (i >= 0) {
    r = shl_i64(r, one);
    if (i >= 32) {
      r.lo = r.lo | ((a.hi >> (i - 32)) & 1);
    } else {
      r.lo = r.lo | ((a.lo >> i) & 1);
    }
    if (r.hi > b.hi || (r.hi == b.hi && r.lo >= b.lo)) {
      r = sub_i64(r, b);
      if (i >= 32) {
        q.hi = q.hi | (1u << (i - 32));
      } else {
        q.lo = q.lo | (1u << i);
      }
    }
    i = i - 1;
  }
  return r;
}

// ---------------------------------------------------------------------------
// Signed division and remainder
// ---------------------------------------------------------------------------

struct Int64 div_i64(struct Int64 a, struct Int64 b) {
  int neg_a = (int)a.hi < 0;
  int neg_b = (int)b.hi < 0;
  struct Int64 ua;
  struct Int64 ub;
  if (neg_a) ua = neg_i64(a); else ua = a;
  if (neg_b) ub = neg_i64(b); else ub = b;
  struct Int64 q = div_u64(ua, ub);
  if (neg_a != neg_b) q = neg_i64(q);
  return q;
}

struct Int64 rem_i64(struct Int64 a, struct Int64 b) {
  int neg_a = (int)a.hi < 0;
  struct Int64 ua;
  struct Int64 ub;
  if (neg_a) ua = neg_i64(a); else ua = a;
  if ((int)b.hi < 0) ub = neg_i64(b); else ub = b;
  struct Int64 r = rem_u64(ua, ub);
  if (neg_a) r = neg_i64(r);
  return r;
}

// ---------------------------------------------------------------------------
// Comparisons (return int: 1 = true, 0 = false)
// ---------------------------------------------------------------------------

int eq_i64(struct Int64 a, struct Int64 b) {
  return a.lo == b.lo && a.hi == b.hi;
}

int eq_u64(struct Int64 a, struct Int64 b) {
  return eq_i64(a, b);
}

int ne_i64(struct Int64 a, struct Int64 b) {
  return a.lo != b.lo || a.hi != b.hi;
}

int ne_u64(struct Int64 a, struct Int64 b) {
  return ne_i64(a, b);
}

int lt_u64(struct Int64 a, struct Int64 b) {
  return a.hi < b.hi || (a.hi == b.hi && a.lo < b.lo);
}

int lt_i64(struct Int64 a, struct Int64 b) {
  int shi_a = (int)a.hi;
  int shi_b = (int)b.hi;
  return shi_a < shi_b || (shi_a == shi_b && a.lo < b.lo);
}

int le_u64(struct Int64 a, struct Int64 b) {
  return a.hi < b.hi || (a.hi == b.hi && a.lo <= b.lo);
}

int le_i64(struct Int64 a, struct Int64 b) {
  int shi_a = (int)a.hi;
  int shi_b = (int)b.hi;
  return shi_a < shi_b || (shi_a == shi_b && a.lo <= b.lo);
}

int gt_u64(struct Int64 a, struct Int64 b) {
  return lt_u64(b, a);
}

int gt_i64(struct Int64 a, struct Int64 b) {
  return lt_i64(b, a);
}

int ge_u64(struct Int64 a, struct Int64 b) {
  return le_u64(b, a);
}

int ge_i64(struct Int64 a, struct Int64 b) {
  return le_i64(b, a);
}
