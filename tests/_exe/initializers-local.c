
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

void print_chars(char* str, int len) {
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

void test_local_initializers() {
  // I know assignment between 2 structs copies the fields from one to the other
  // But what happens if assignment is inside rvalue?
  struct S1 s1 = { 13, 29 };
  struct S1 s2 = s1;

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

  print_s1(s1);
  print_s1(s2);

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
  test_local_initializers();

  return 0;
}