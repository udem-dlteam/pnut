#ifndef PNUT_CC
#include <stdio.h>
#else
typedef int FILE;
#endif

void main() {
  // Test %d, %c, %x, %s, %.*s, %4s
  printf("Hello, world!\n");
  printf("%d\n", 42);
  printf("%c\n", 'a');
  printf("Hello %c-%xPO\n", 'C', 3);
  printf("Hello, %s!\n", "world");
  printf("Hello, world! %.*s\n", 5, "R2-D2 (beep-boop)");
  printf("Hello, world! <%12.5s>\n", "ROBOT");

  // Testing flags
  // '-': The result of the conversion shall be left-justified within the field.
  printf("'%-10d'\n", 42);
  printf("'%-10c'\n", 'a');
  printf("'%-10x'\n", 42);
  printf("'%-10s'\n", "world");
  // '+': The result of a signed conversion shall always begin with a sign ( '+' or '-' ).
  printf("'%+10d'\n", 42);
  printf("'%+10c'\n", 'a');     // No effect
  printf("'%+10x'\n", 42);
  printf("'%+10s'\n", "world"); // No effect
  // ' ': If the first character of a signed conversion is not a sign, a space shall be prefixed to the result.
  printf("'% d'\n", 42);
  printf("'% d'\n", -42);
  printf("'% x'\n", 42);
  printf("'% s'\n", "world"); // No effect
  // '#': The result shall always be prefixed by the 0x or 0X prefix.
  // printf("'%#d'\n", 42); // undefined behavior
  printf("'%#10x'\n", 42);
  printf("'%-#10x'\n", 42);
  printf("'%-#10o'\n", 42);
  printf("'%-+#10o'\n", 42);
  // '0': The value should be zero-padded.
  printf("'%0d'\n", 42); // no minimum width so no padding
  printf("'%04d'\n", 42);
  printf("'%04x'\n", 42);

  // Testing width
  printf("'%4d'\n", 42);
  printf("'%*d'\n", 4, 42);
  printf("'%12s'\n", "Wooooooo");
  printf("'%*s'\n", 12, "Wooooooo");

  // Testing precision
  printf("'%.4d'\n", 42); // On numbers, it adds leading 0s
  printf("'%.4x'\n", 42);
  printf("'%.4X'\n", 42);
  printf("'%.4s'\n", "Wooooooo"); // On strings, it truncates
}
