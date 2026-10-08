// expect_comp_failure_for: sh
void main() {
  int a = f() ? 1 : 2; // Valid
  int b = f() ? 1 : f(); // Invalid
}
