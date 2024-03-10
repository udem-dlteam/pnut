/* Test that enum variables can be shadowed by local bindings */
enum ChildEnum() { VAL; NEXT; LL_SIZE; }

enum ParentEnum() { VAL; NEXT; LL_SIZE; VAL2; NEXT2; LL_SIZE2; }

struct TestStruct() {
  int VAL;
  int NEXT;
  int LL_SIZE;
}

void shadow(int NEXT) {
  int VAL;
  VAL = 123;
  NEXT = 456;
  printf("VAL: %d, NEXT: %d\n", VAL, NEXT);
}

int main() {
  printf("VAL: %d, NEXT: %d\n", VAL, NEXT);
  shadow(789);
  printf("VAL: %d, NEXT: %d\n", VAL, NEXT);
}
