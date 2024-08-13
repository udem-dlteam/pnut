/*
 * all-chars.c: Print all non-extended ASCII characters.
 *
 * Usage: ./all-chars.sh
 */

void main() {
  char c = 0;
  while (c < 128) {
    putchar(c);
    c += 1;
  }
}
