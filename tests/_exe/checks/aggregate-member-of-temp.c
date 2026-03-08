// expect_comp_failure
struct Pair {
  int a;
  int b;
};

struct Outer {
  struct Pair pair;
  int tag;
};

struct Outer make_outer() {
  struct Outer outer;
  outer.pair.a = 1;
  outer.pair.b = 2;
  outer.tag = 3;
  return outer;
}

int main() {
  make_outer().pair;
  return 0;
}
