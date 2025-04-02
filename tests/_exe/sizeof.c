#include <stdio.h>

#ifdef PNUT_CC
// When bootstrapping pnut, intptr_t is not defined.
// On 64 bit platforms, intptr_t is a long long int.
// On 32 bit (including shells) platforms, intptr_t is an int.
#if defined(PNUT_EXE_64)
#define PTR_MUL 8
#else
#define PTR_MUL 4
#endif
#else
#define PTR_MUL 4
#endif


void putint(int n) {
  if (n < 0) {
    putchar('-');
    n = -n;
  }
  if (n > 9) {
    putint(n / 10);
  }
  putchar('0' + n % 10);
}

struct S0 {
  char a;
}; // 1 byte

struct S1 {
  int a;      // 4 bytes
  int b;      // 4 bytes
}; // 8 bytes

struct S2 {
  int a;
  char b;
  // 3 bytes padding
  int c;
}; // 12 bytes

struct S3 {
  char a;
  char b[2];
  // 1 bytes padding
  int c;
}; // 8 bytes

struct S4 {
  char a;       // 1 byte
  int b[0];     // 4 * 0 = 0 bytes
}; // 1 byte, largest member is int (1)

struct S5 {
  char b[4];
}; // 4 bytes, largest member is char (1)

struct S6 {
  int a;
}; // 4 bytes, largest member is int (4)

union U0 {
  char a;       // 1 byte
  int b;        // 4 bytes
}; // 4 bytes

union U1 {
  char a;       // 1 byte
  int b;        // 4 bytes
  char c[5];    // 5 bytes
}; // 8 bytes because aligned with width of int (4)

union U2 {
  char a[3];    // 3 bytes
  int b;        // 4 bytes
}; // 4 bytes because aligned with width of int (4)

union U3 {
  char a[5];    // 5 bytes
  char b;       // 1 byte
}; // 5 bytes because aligned with width of char (1)

union U4 {
  char a[5];    // 5 bytes
  int b;        // 4 bytes
}; // 8 bytes, because aligned with width of int (4)

union U5 {
  char a[5];    // 5 bytes
  struct S5 b;  // 4 bytes
}; // 5 bytes because aligned with width of char (1)

union U6 {
  char a[5];
  struct S6 b;
}; // 5 bytes

union U7 {
  char a[9];
  struct S5 b[1]; // 4 bytes
}; // 9 bytes because aligned with width of char (1)

union U8 {
  char a[2][3];   // 6 bytes
  struct S5 b[1]; // 4 bytes
}; // 6 bytes because aligned with width of char (1)

union U9 {
  char a[2][3];   // 6 bytes
  struct S4 b[1]; // 1 byte, but largest member is int (array of size 0)
}; // 6 bytes because aligned with width of int (4)

union U10 {
  char a[2][3];   // 6 bytes
  struct S5 b[1]; // 4 bytes
  int  c[1][1][1][1]; // 4 bytes
}; // 8 bytes because aligned with width of int (4)

struct ArHdr {
  char ar_name[16];
  char ar_date[12];
  char ar_uid[6];
  char ar_gid[6];
  char ar_mode[8];
  char ar_size[10];
  char ar_fmag[2];
};

int main() {
  int a[10] = { 0, 1, 2, 3 };
  int b[]   = { 0, 1, 2, 3 }; // Infer size from initializer (4 elements)
  putint(sizeof(char));       putchar('\n'); // No division needed because sizeof(char) is always 1
  putint(sizeof(int));        putchar('\n');
  putint(sizeof(int[10]));    putchar('\n');
  putint(sizeof(int[10][2])); putchar('\n');
  putint(sizeof(a));          putchar('\n'); // sizeof (expr)
  putint(sizeof a);           putchar('\n'); // sizeof expr
  putint(sizeof(b));          putchar('\n'); // sizeof (expr)
  putint(sizeof b);           putchar('\n'); // sizeof expr
  putint(sizeof((void *) a) / PTR_MUL); putchar('\n'); // sizeof (cast_expr)

  // Testing struct/union size and alignment
  putint(sizeof(struct S0)); putchar('\n');
  putint(sizeof(struct S1)); putchar('\n');
  putint(sizeof(struct S2)); putchar('\n');
  putint(sizeof(struct S3)); putchar('\n');
  putint(sizeof(struct S4)); putchar('\n');

  putint(sizeof(union U0)); putchar('\n');
  putint(sizeof(union U1)); putchar('\n');
  putint(sizeof(union U3)); putchar('\n');
  putint(sizeof(union U3[1])); putchar('\n');
  putint(sizeof(union U3[2])); putchar('\n');
  putint(sizeof(union U3[5])); putchar('\n');
  putint(sizeof(union U4)); putchar('\n');
  putint(sizeof(union U5)); putchar('\n');
  putint(sizeof(union U6)); putchar('\n');
  putint(sizeof(union U7)); putchar('\n');
  putint(sizeof(union U8)); putchar('\n');
  putint(sizeof(union U9)); putchar('\n');
  putint(sizeof(union U10)); putchar('\n');
  putint(sizeof(struct ArHdr)); putchar('\n');

  return 0;
}
