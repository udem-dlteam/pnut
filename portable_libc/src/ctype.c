#include "include/ctype.h"

int isdigit(int c) {
  return c >= '0' && c <= '9';
}

int isxdigit(int c) {
  return isdigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

int isnumber(int c, int base) {
  if (c >= '0') {
    if (base == 2)
      return c <= '1';
    if (base == 8)
      return c <= '7';
    if (base == 10)
      return c <= '9';
  }
  return isxdigit(c);
}

int islower(int c) {
  return c >= 'a' && c <= 'z';
}

int isupper(int c) {
  return c >= 'A' && c <= 'Z';
}

int isalpha(int c) {
  return islower(c) | isupper(c);
}

int isalnum(int c) {
  return isdigit(c) | isalpha(c);
}

int tolower(int c) {
  if (isupper(c))
    c +=  'a' - 'A';
  return c;
}

int toupper(int c) {
  if (islower(c))
    c +=  'A' - 'a';
  return c;
}

int isascii(int c) {
  return c >= 0 && c <= 127;
}

int iscntrl(int c) {
  return c >= 0 && c < 32;
}

int isgraph(int c) {
  return c > 32 && c < 127;
}

int isprint(int c) {
  return c >= 32 && c < 127;
}

int isspace(int c) {
  return (c > ' ') ? 0 : (c == ' ' || c == '\n' || c == '\t' || c == '\v' || c == '\f' || c == '\r');
}

int ispunct(int c) {
  return isprint(c) & !isspace(c) & !isalnum(c);
}
