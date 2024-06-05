
int global1 = 1, global2 = 2;

void testLocalAndGlobalInteractionInitialization() {
  int local1 = 3, local2 = 4;

  putchar(global1+48);
  putchar(10);
  putchar(global2+48);
  putchar(10);
  putchar(local1+48);
  putchar(10);
  putchar(local2+48);
}

int main() {
  testLocalAndGlobalInteractionInitialization();
  return 0;
}
