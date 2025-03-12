// expect_comp_failure

static int some_var; // Allowed

void main() {
  static int some_other_var; // Static local vars are not supported
}
