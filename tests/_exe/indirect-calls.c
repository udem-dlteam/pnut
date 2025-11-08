#include <stdio.h>

void putstr(char *s) {
  while (*s) {
    putchar(*s);
    s += 1;
  }
}

void f(int direct) {
  if (direct) {
    putstr("direct\n");
  } else {
    putstr("indirect\n");
  }
}

void fun1() {
  putstr("fun1\n");
}

void fun2() {
  putstr("fun2\n");
}

void fun3() {
  putstr("fun3\n");
}

void fun4() {
  putstr("fun4\n");
}

int return_0() {
  return 0;
}

int (*return_0_ptr())() {
  return &return_0;
}

void (*funs[4])() = {&fun1, &fun2, *fun3, **fun4};

void calls_funs(void (*funs[])(), int n) {
  int i;
  for (i = 0; i < n; i++) {
    funs[i]();
  }
}

void call_fun(void (fun)(), char *msg) {
  void (fun2)();
  fun(msg);
  (*fun)(msg);
  (**fun)(msg);
}

int main() {
  calls_funs(funs, 4);
  void (*f_ptr1)() = f;
  void (*f_ptr2)() = &f;
  void (*f_ptr3)() = *f;
  void (*f_ptr4)() = *****f;
  f(1);
  f_ptr1(0);
  (*f_ptr1)(0);
  (*f_ptr1)(0);
  (***f_ptr1)(0);
  f_ptr2(0);
  f_ptr3(0);
  f_ptr4(0);
  funs[0]();
  funs[3]();
  // Indirect call on non-literal expression
  int one = return_0_ptr()() + 1;
  if (one != 1) {
    return 1;
  }
  call_fun(putstr,    "hello\n");
  call_fun(*putstr,   "hello\n");
  call_fun(&putstr,   "hello\n");
  call_fun(**putstr,  "hello\n");
  return 0;
}
