// expect_comp_failure
void main() {
  int a = f() ? 1 : 2; // Valid
  int b = f() ? 1 : f(); // Invalid
}
