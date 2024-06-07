
int global1, global2;

void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

void testLocalAndGlobalInteraction() {
  int local1, local2;
  global1 = 1;
  global2 = 2;
  local1 = 3;
  local2 = 4;
  putstring("Global Variables:\n");
  putchar(global1+48);
  putchar(10);
  putchar(global2+48);
  putchar(10);
  putstring("Local Variables:\n");
  putchar(local1+48);
  putchar(10);
  putchar(local2+48);
}

int global3 = 1, global4 = 2;

void testLocalAndGlobalInteractionInitialization() {
  int local3 = 3, local4 = 4;
  putstring("Global Variables Initialized:\n");
  putchar(global3+48);
  putchar(10);
  putchar(global4+48);
  putchar(10);
  putstring("Local Variables Initialized:\n");
  putchar(local3+48);
  putchar(10);
  putchar(local4+48);
}

int main() {
  testLocalAndGlobalInteraction();
  testLocalAndGlobalInteractionInitialization();
  return 0;
}
