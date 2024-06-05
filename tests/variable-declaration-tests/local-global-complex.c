void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}
// Global declarations
int globalInt = 1, globalArray[3];

void manipulateLocalVariables() {
  // Local declarations
  int localArray[3], localInt = 2;

  // Initialize local array
  localArray[0] = 1;
  localArray[1] = 2;
  localArray[2] = 3;

  // Print local variables
  putstring("Local Integer: ");
  putchar(localInt + 48);
  putchar('\n');

  putstring("Local Array: ");
  int i = 0;
  while (i < 3) {
    putchar(localArray[i] + 48);
    putchar(',');
    i++;
  }
  putchar('\n');

}

void manipulateGlobalVariables() {

  // Initialize global array
  globalArray[0] = 4;
  globalArray[1] = 5;
  globalArray[2] = 6;

  // Print global variables
  putstring("Global Integer: ");
  putchar(globalInt + 48);
  putchar('\n');

  putstring("Global Array: ");
  int i = 0;
  while (i < 3) {
    putchar(globalArray[i] + 48);
    putchar(',');
    i++;
  }
  putchar('\n');

}

int main() {
  manipulateLocalVariables();
  manipulateGlobalVariables();
  return 0;
}
