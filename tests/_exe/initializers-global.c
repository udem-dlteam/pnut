// Test initializers for global variables

#include <stdio.h>

struct S1 {
  int a;
  int b;
};

struct S2 {
  struct S1 s[3];
  int b;
};

void putint_aux(int n) {
  if (n <= -10) putint_aux(n / 10);
  putchar('0' - (n % 10));
}

void putint(int n) {
  if (n < 0) {
    putchar('-');
    putint_aux(n);
  } else {
    putint_aux(-n);
  }
}

void print_chars(unsigned char* str, int len) {
  int i = 0;
  while (i < len) {
    putchar('\''); putint(str[i]); putchar('\''); putchar(' ');
    i++;
  }
}

void print_ints(int* arr, int len) {
  int i = 0;
  while (i < len) {
    putint(arr[i]); putchar(' ');
    i++;
  }
}

void print_s1(struct S1 s) {
  putint(s.a); putchar(' '); putint(s.b); putchar('\n');
}

void print_s2(struct S2 s) {
  int i = 0;
  while (i < 3) {
    print_s1(s.s[i]);
    i++;
  }
  putint(s.b); putchar('\n');
}

// Copied from tccpp.c
#define TOK_LAND  0x90
#define TOK_LOR   0x91
#define TOK_EQ  0x94
#define TOK_NE  0x95
#define TOK_GE  0x9d
#define TOK_LE  0x9e

#define TOK_DEC     0x80 /* -- */
#define TOK_INC     0x82 /* ++ */
#define TOK_SHL     '<' /* shift left */
#define TOK_SAR     '>' /* signed shift right */

#define TOK_A_ADD   0xb0
#define TOK_A_SUB   0xb1
#define TOK_A_MUL   0xb2
#define TOK_A_DIV   0xb3
#define TOK_A_MOD   0xb4
#define TOK_A_AND   0xb5
#define TOK_A_OR    0xb6
#define TOK_A_XOR   0xb7
#define TOK_ARROW   0xa0 /* -> */
#define TOK_TWODOTS 0xa2 /* C++ token ? */
#define TOK_TWOSHARPS 0xa3 /* ## preprocessing token */

static const unsigned char tok_two_chars[] = {
    '<','=', TOK_LE,
    '>','=', TOK_GE,
    '!','=', TOK_NE,
    '&','&', TOK_LAND,
    '|','|', TOK_LOR,
    '+','+', TOK_INC,
    '-','-', TOK_DEC,
    '=','=', TOK_EQ,
    '<','<', TOK_SHL,
    '>','>', TOK_SAR,
    '+','=', TOK_A_ADD,
    '-','=', TOK_A_SUB,
    '*','=', TOK_A_MUL,
    '/','=', TOK_A_DIV,
    '%','=', TOK_A_MOD,
    '&','=', TOK_A_AND,
    '^','=', TOK_A_XOR,
    '|','=', TOK_A_OR,
    '-','>', TOK_ARROW,
    '.','.', TOK_TWODOTS,
    '#','#', TOK_TWOSHARPS,
    0
};

char str[13] = "Hello, world!";

int arr[4] = {1, 2, 3, 4};
int arr_partial[4] = {1, 0xcc}; // Rest should be 0
int arr_inferred[] = {1, 2, 3, 4};
int scalar = {42}; // This is a scalar, not an array, it should be

char no_size_arr1[] = {1, 12343141, 3, 4, 12321};
char no_size_arr2[] = "abcde";

struct S1 struct1 = { 1, 2 };
struct S1 struct_partial = { 1, }; // Note trailing comma
struct S1 global_struct_scalar = { { 1 }, 2 };
struct S1 inferred_struct[] = { { 4321, 123 }, { 12000, 110 } };
struct S1 struct_arr[3] = { { 42, 39 }, { 32, 23 } };

struct S2 struct2 = { { { 123, 432131 }, { 4311, 53141 }, { 5311, 421313 } }, 2131321 };
struct S2 struct2_partial1 = { { { 12321, 21321 } }, 421321431 }; // Nested array is partially initialized
struct S2 struct2_partial2 = { { { 231321, 4531321 } },  };  // Outer struct is partially initialized

void test_global_initializers() {
  print_chars(tok_two_chars, 64); putchar('\n');
  print_chars(str, 12); putchar('\n');

  print_ints(arr, 4); putchar('\n');
  print_ints(arr_partial, 4); putchar('\n');
  print_ints(arr_inferred, 4); putchar('\n');
  putint(scalar); putchar('\n');

  print_chars(no_size_arr1, 5); putchar('\n');
  print_chars(no_size_arr2, 6); putchar('\n');

  print_s1(struct1);
  print_s1(struct_partial);
  print_s1(global_struct_scalar);
  print_s1(inferred_struct[0]);
  print_s1(inferred_struct[1]);
  print_s1(struct_arr[0]);
  print_s1(struct_arr[1]);
  print_s1(struct_arr[2]);

  print_s2(struct2);
  print_s2(struct2_partial1);
  print_s2(struct2_partial2);
}

int main() {
  test_global_initializers();

  return 0;
}
