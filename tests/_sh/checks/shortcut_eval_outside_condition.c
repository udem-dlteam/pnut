// expect_comp_failure_for: sh
void main() {
  int a = f() && g();
  int b = 1   && g();
  int c = f() && 1;
}
