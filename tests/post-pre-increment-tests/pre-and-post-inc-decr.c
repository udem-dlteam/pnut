
int test1() {
  int x = 5;
  int y = ++x;
  // Expected: x == 6, y == 6

  if(x == 6 && y == 6){
    return 0;
  }else{
    return 1;
  }
}

int test2() {
  int x = 5;
  int y = x++;
  // Expected: x == 6, y == 5
  if(x == 6 && y == 5){
    return 0;
  }else{
    return 1;
  }
}

int test3() {
  char x = 'a';
  char y = ++x;
  // Expected: x == 'b', y == 'b'

  if(x == 'b' && y == 'b'){
    return 0;
  }else{
    return 1;
  }

}

int test4() {
  char x = 'a';
  char y = x++;
  // Expected: x == 'b', y == 'a'

  if (x == 'b' && y == 'a'){
      return 0;
  }else{
    return 1;
  }

}

int test5() {
  // cast not yet supported

  // int arr[2] = {1, 2};
  // int* p = arr;
  // int x = ++(*p);
  // // Expected: arr[0] == 2, x == 2

  // if(arr[0] == 2 && x == 2){
  //   return 0;
  // }else{
  //   return 1;
  // }

  return 1;

}

int test6() {
  // cast not yet supported

  // int arr[2];
  // arr[0] = 1;
  // arr[1] = 2;
  // int* p = arr;
  // int x = (*p)++;
  // // Expected: arr[0] == 2, x == 1

  // if(arr[0] == 2 && x == 1){
  //   return 0;
  // }else{
  //   return 1;
  // }

  return 1;

}

int test7() {
  int x = 3 + 2;
  int y = ++x + 2;
  // Expected: x == 6, y == 8

  if(x == 6 && y == 8){
    return 0;
  }else{
    return 1;
  }

}

int test8() {
  int x = 5;
  int y = x++ + 2;
  // Expected: x == 6, y == 7

  if(x == 6 && y == 7){
    return 0;
  }else{
    return 1;
  }

}

int test9() {
  int x = 0;
  int i = 0;
  while (i < 5) {
    ++x;
    ++i;
  }
  // Expected: x == 5

  if(x == 5){
    return 0;
  }else{
    return 1;
  }

  // return (x == 5) ? 0 : 1;
}

int test10() {
  int x = 0;
  int i = 0;
  while (i < 5) {
    x++;
    i++;
  }

  if(x == 5){
    return 0;
  }else{
    return 1;
  }

  // Expected: x == 5
}

int test11() {
  int x = 5;
  int y1 = ++x; // x is 6, y1 is 6
  int y2 = x++; // y2 is 6, x is now 7
  int y3 = ++x; // x is 8, y3 is 8
  int y = y1 + y2 + y3; // y is 6 + 6 + 8 = 20
  // Expected: x == 8, y == 20

  if(x == 8 && y == 20){
    return 0;
  }else{
    return 1;
  }
}

int increment(int a) {
  return a + 1;
}

int test12() {
  int x = 5;
  int y1 = increment(++x);
  int y2 = increment(x++);
  int y = y1 + y2;
  // Expected: x == 7, y == 14

  if(x == 7 && y == 14){
    return 0;
  }else{
    return 1;
  }
}

int test13() {
  int x = 5;
  int b = ++x;
  // Expected: x == 6, b == 1 (true)

  if(x == 6 && b == 1){
    return 0;
  }else{
    return 1;
  }

}

int test14() {
  int x = 5;
  int b = x++;
  // Expected: x == 6, b == 1 (true)

  if(x == 6 && b == 1){
    return 0;
  }else{
    return 1;
  }

}

int test15() {
  // cast not yet supported

  // int arr[3];
  // arr[0] = 1;
  // arr[1] = 2;
  // arr[2] = 3;
  // int* p = arr;
  // int y1 = ++(*p); // arr[0] is 2, y1 is 2
  // int y2 = *p++; // y2 is 2, p now points to arr[1]
  // int y3 = ++(*p); // arr[1] is 3, y3 is 3
  // int y = y1 + y2 + y3; // y is 2 + 2 + 3 = 7
  // // Expected: arr[0] == 2, arr[1] == 3, arr[2] == 3, y == 7

  // if(arr[0] == 2 && arr[1] == 3 && arr[2] == 3 && y == 7){
  //   return 0;
  // }else{
  //   return 1;
  // }

  return 1;

}

int main(){
  if (test1() == 0){
    putchar('1');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test2() == 0){
    putchar('2');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test3() == 0){
    putchar('3');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test4() == 0){
    putchar('4');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test5() == 0){
    putchar('5');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test6() == 0){
    putchar('6');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test7() == 0){
    putchar('7');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test8() == 0){
    putchar('8');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test9() == 0){
    putchar('9');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test10() == 0){
    putchar('1');
    putchar('0');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test11() == 0){
    putchar('1');
    putchar('1');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test12() == 0){
    putchar('1');
    putchar('2');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test13() == 0){
    putchar('1');
    putchar('3');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test14() == 0){
    putchar('1');
    putchar('4');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  if (test15() == 0){
    putchar('1');
    putchar('5');
    putchar(10);
  } else{
    putchar('x');
    putchar(10);
  }
  return 0;
}
