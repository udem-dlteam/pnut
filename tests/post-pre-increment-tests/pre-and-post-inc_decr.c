
int test1() {
  int x = 5;
  int y = ++x;
  // Expected: x == 6, y == 6
  return (x == 6 && y == 6) ? 0 : 1;
}

int test2() {
  int x = 5;
  int y = x++;
  // Expected: x == 6, y == 5
  return (x == 6 && y == 5) ? 0 : 1;
}

int test3() {
  char x = 'a';
  char y = ++x;
  // Expected: x == 'b', y == 'b'
  return (x == 'b' && y == 'b') ? 0 : 1;
}

int test4() {
  char x = 'a';
  char y = x++;
  // Expected: x == 'b', y == 'a'
  return (x == 'b' && y == 'a') ? 0 : 1;
}

int test5() {
  int arr[2] = {1, 2};
  int* p = arr;
  int x = ++(*p);
  // Expected: arr[0] == 2, x == 2
  return (arr[0] == 2 && x == 2) ? 0 : 1;
}

int test6() {
  int arr[2] = {1, 2};
  int* p = arr;
  int x = (*p)++;
  // Expected: arr[0] == 2, x == 1
  return (arr[0] == 2 && x == 1) ? 0 : 1;
}

int test7() {
  int x = 5;
  int y = ++x + 2;
  // Expected: x == 6, y == 8
  return (x == 6 && y == 8) ? 0 : 1;
}

int test8() {
  int x = 5;
  int y = x++ + 2;
  // Expected: x == 6, y == 7
  return (x == 6 && y == 7) ? 0 : 1;
}

int test9() {
  int x = 0;
  for (int i = 0; i < 5; ++i) {
    ++x;
  }
  // Expected: x == 5
  return (x == 5) ? 0 : 1;
}

int test10() {
  int x = 0;
  for (int i = 0; i < 5; i++) {
    x++;
  }
  // Expected: x == 5
  return (x == 5) ? 0 : 1;
}

int test11() {
  int x = 5;
  int y = ++x + x++ + ++x;
  // Expected: x == 8, y == 22 (6 + 6 + 8)
  return (x == 8 && y == 22) ? 0 : 1;
}

int increment(int a) {
  return a + 1;
}

int test12() {
  int x = 5;
  int y = increment(++x) + increment(x++);
  // Expected: x == 7, y == 14 (increment(6) + increment(6))
  return (x == 7 && y == 14) ? 0 : 1;
}

int test13() {
  int x = 5;
  int b = (++x == 6);
  // Expected: x == 6, b == 1 (true)
  return (x == 6 && b) ? 0 : 1;
}

int test14() {
  int x = 5;
  int b = (x++ == 5);
  // Expected: x == 6, b == 1 (true)
  return (x == 6 && b) ? 0 : 1;
}

int test15() {
  int arr[3] = {1, 2, 3};
  int* p = arr;
  int y = ++(*p) + *p++ + ++(*p);
  // Expected: arr[0] == 2, arr[1] == 2, arr[2] == 3, y == 8 (2 + 2 + 4)
  return (arr[0] == 2 && arr[1] == 2 && arr[2] == 3 && y == 8) ? 0 : 1;
}

int main(){
  if (test1()){
    putchar('1');
    putchar(10);
  }
  if (test2()){
    putchar('2');
    putchar(10);
  }
  if (test3()){
    putchar('3');
    putchar(10);
  }
  if (test4()){
    putchar('4');
    putchar(10);
  }
  if (test5()){
    putchar('5');
    putchar(10);
  }
  if (test6()){
    putchar('6');
    putchar(10);
  }
  if (test7()){
    putchar('7');
    putchar(10);
  }
  if (test8()){
    putchar('8');
    putchar(10);
  }
  if (test9()){
    putchar('9');
    putchar(10);
  }
  if (test10()){
    putchar('1');
    putchar('0');
    putchar(10);
  }
  if (test11()){
    putchar('1');
    putchar('1');
    putchar(10);
  }
  if (test12()){
    putchar('1');
    putchar('2');
    putchar(10);
  }
  if (test13()){
    putchar('1');
    putchar('3');
    putchar(10);
  }
  if (test14()){
    putchar('1');
    putchar('4');
    putchar(10);
  }
  if (test15()){
    putchar('1');
    putchar('5');
    putchar(10);
  }
  return 0;
}
