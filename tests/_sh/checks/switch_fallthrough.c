// expect_comp_failure
void main() {
  switch (1) {
    case 1:
      putchar('a');
    case 2:
      putchar('b');
      break;
  }
}
