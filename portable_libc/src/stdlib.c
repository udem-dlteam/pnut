#include "include/stdlib.h"

#define HEAP_SIZE 1000000

char _heap[HEAP_SIZE];
int _heap_alloc = 0;

void *malloc(size_t size) {
  char *result = _heap + _heap_alloc;
  if (_heap_alloc + size > HEAP_SIZE) return 0;
  _heap_alloc += size;
  return result;
}

void free(void *ptr) {
}

void *realloc(void *ptr, size_t size) {
  return 0; /*TODO*/
}

double strtod(const char *str, char **endptr) {
  return 0.0; /*TODO*/
}

float strtof(const char *str, char **endptr) {
  return 0.0; /*TODO*/
}

long double strtold(const char *str, char **endptr) {
  return 0.0; /*TODO*/
}

long int strtol(const char *str, char **endptr, int base) {

  char *probe = str;
  int neg;
  int n = 0;
  int nb_digits = 0;
  int d;

  while (*probe && *probe <= ' ') probe++; /* skip spaces */

  neg = *probe == '-';

  if (neg || *probe == '+') probe++;

  if (*probe == '0') {
    probe++;
    nb_digits = 1;
    if (base == 0 || base == 16) {
      if (*probe == 'x') {
        base = 16;
        probe++;
        nb_digits = 0;
      } else if (base == 0) {
        base = 8;
      }
    }
  }

  while (1) {
    d = *probe;
    if (d >= '0' && d <= '9') {
      d = d-'0';
    } else if (d >= 'A' && d <= 'Z') {
      d = d-'A'+10;
    } else if (d >= 'a' && d <= 'z') {
      d = d-'a'+10;
    } else {
      d = 99;
    }
    if (d >= base) break;
    n = n*base - d;
    nb_digits++;
  }

  if (endptr) {
    if (nb_digits == 0) {
      *endptr = str;
    } else {
      *endptr = probe;
    }
  }

  return neg ? n : -n;
}

int atoi(const char *str) {
  return strtol(str, 0, 10);
}
