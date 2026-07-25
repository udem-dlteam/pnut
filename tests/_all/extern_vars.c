#include <stdio.h>

// extern forward-declares a variable defined later in the same translation
// unit. It allocates no storage and must not overwrite the definition's value.
extern int defined_later;

int read_before_definition() {
  return defined_later;
}

int defined_later = 5;

int defined_first = 7;
extern int defined_first;

int main() {
  putchar('0' + read_before_definition());
  putchar('0' + defined_later);
  putchar('0' + defined_first);
  putchar('\n');
  return 0;
}
