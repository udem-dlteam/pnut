// The comma operator is not implemented correctly in pnut.sh
// expect_failure_for: ksh, bash, dash, zsh, yash, mksh

#include <stdio.h>

void putstring(char *s) {
	while (*s) {
		putchar(*s);
		s = s + 1;
	}
}

int main() {
  int a = 0, b = 1;
  a = 1, 2, b = 3;
  if (a != 2 || b != 3) { putstring("fail\n"); return 1; }
  a = (b = 2, b + 1);
  if (a != 3 || b != 2) { putstring("fail\n"); return 1; }
}
