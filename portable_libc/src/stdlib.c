#include "../include/stdlib.h"

#define HEAP_SIZE 1000000000

char _heap[HEAP_SIZE];
int _heap_alloc = 0;

void *malloc(size_t size) {
  size += sizeof(size_t); // size + size_t (for size)
  char *result = _heap + _heap_alloc;
  _heap_alloc += size;
  if (_heap_alloc > HEAP_SIZE) return 0; // out of memory
  *((size_t*)result) = size; // store size
  return result + sizeof(size_t); // return pointer to memory, after size field
}

void free(void *ptr) {
  return; // no-op
}

void *realloc(void *ptr, size_t size) {
  int i, new_ptr;
  size_t old_size;
  if (size == 0) {
    free(ptr);
  } else {
    new_ptr = malloc(size);
    if (ptr) {
      old_size = *((size_t*)((char *) ptr - sizeof(size_t)));
      if (old_size < size) size = old_size; //
      // Copy memory
      for (i = 0; i < size; i++) {
        ((char*)new_ptr)[i] = ((char*)ptr)[i];
      }
    }
    ptr = new_ptr;
  }

  return ptr;
}

#ifndef PNUT_CC

double strtod(const char *str, char **endptr) {
  return 0.0; /*TODO*/
}

float strtof(const char *str, char **endptr) {
  return 0.0; /*TODO*/
}

long double strtold(const char *str, char **endptr) {
  return 0.0; /*TODO*/
}

#endif

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

char *getenv(const char *name) {
  return 0; /*TODO*/
}
