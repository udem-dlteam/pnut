// expect_comp_failure
void main() {
  switch (1) {
    case 1:
      if (0) {
        break;
      } else {
        // This should not be allowed
      }
    case 2:
      return;
  }
}
