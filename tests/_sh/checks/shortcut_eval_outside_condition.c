// expect_comp_failure
void main() {
  int a = f() && g();
  int b = 1   && g();
  int c = f() && 1;
}
