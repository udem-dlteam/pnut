// expect_comp_failure_for: awk
// `length_` is the name that pnut-awk gives to a local variable called `length`,
// so it can't be used: the two variables would share one awk variable.

#include <stdio.h>

void main() {
  int length_;

  length_ = 42;
  putchar(length_);
}
