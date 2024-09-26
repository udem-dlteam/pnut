// On ksh, when assigning the negative value of a variable containing 0 (i.e.
// -0) to a variable using arithmetic expansion, the variable is assigned the
// string `-0` instead of 0. Other shells assign 0 as expected.

// expect_failure_for: ksh
int return0() {
  int a = 0;
  return -a;
}

void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void main() {
  // Workaround: use `return0() + 0 == 0`.
  // This forces the result of return0 to be in an arithmetic expansion which removes the `-` from `-0`.
  if (return0() == 0) {
    putstring("zero\n");
  } else {
    putstring("non-zero\n");
  }
}
