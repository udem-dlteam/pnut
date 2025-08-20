#include <pnut_lib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define HEAP_SIZE 26214400 // 25 MB heap size

// TCC defines malloc, free and realloc as macros in libtcc.c which conflicts
// with our definitions here. This wouldn't be a problem if the libc was
// compiled in a separate compilation unit, but pnut only works with one
// compilation unit.
// The solution is to process the libc code first, and then the TCC code.
#if defined(malloc) || defined(free) || defined(realloc)
#error "malloc, free and realloc are already defined"
#endif

char _heap[HEAP_SIZE];
int _heap_alloc = 0;

void *malloc(size_t size) {
  if (size <= 0) return 0; // no-op

  size += sizeof(size_t); // size + size_t (for size)
  char *result = _heap + _heap_alloc;
  _heap_alloc += size;
  if (_heap_alloc > HEAP_SIZE) {
    return 0; // out of memory
  }
  *((size_t*)result) = size; // store size
  return result + sizeof(size_t); // return pointer to memory, after size field
}

void free(void *ptr) {
  return; // no-op
}

void *realloc(void *ptr, size_t size) {
  int i;
  void *new_ptr;
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

double strtod(const char *str, char **endptr) {
  if (strcmp(str, "0.0") == 0) {
    if (endptr) *endptr = (char *) str + 3;
    return 0x0000000000000000;
  } else if (strcmp(str, "1.0") == 0) {
    if (endptr) *endptr = (char *) str + 3;
    return 0x3FF0000000000000;
  } else if (strcmp(str, "4294967296.0") == 0) {
    if (endptr) *endptr = (char *) str + 12;
    return 0x41F0000000000000;
  } else {
    printf("strtod: Unknown string: %s\n", str);
    pnut_abort("stdtod: Unknown string: ");
    return 0;
  }
}

float strtof(const char *str, char **endptr) {
  return (float) strtod(str, endptr);
}

long double strtold(const char *str, char **endptr) {
  return (long double) strtod(str, endptr);
}

unsigned long long strtoull(const char *str, char **endptr, int base) {
  unsigned long long n = 0;
  int d;
  const char *probe = str;
  int nb_digits = 0;

  while (*probe && *probe <= ' ') probe++; /* skip spaces */

  if (base == 0) { // base = 0 => determine base with prefix
    // 0x   => base 16
    // 0    => base 8
    // else => base 10
    if (probe[0] == '0') {
      if (probe[1] == 'x') {
        base = 16;
        probe += 2;
      } else {
        base = 8;
      }
    } else {
      base = 10;
    }
  }

  while (1) {
    d = *probe;
    if (d >= '0' && d <= '9') {
      d = d - '0';
    } else if (d >= 'A' && d <= 'Z') {
      d = d - 'A' + 10;
    } else if (d >= 'a' && d <= 'z') {
      d = d - 'a' + 10;
    } else {
      d = 99;
    }

    if (d >= base) break;
    n = n * base + d;
    nb_digits++;
    probe++;
  }

  if (endptr) {
    if (nb_digits == 0) {
      *endptr = (char*) str;
    } else {
      *endptr = (char*) probe;
    }
  }

  return n;
}

long long strtoll(const char *str, char **endptr, int base) {

  const char *probe = (char *) str;
  int neg;
  long long n = 0;
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
  } else if (base == 0) {
    base = 10; // default base
  }

  while (1) {
    d = *probe;
    if (d >= '0' && d <= '9') {
      d = d - '0';
    } else if (d >= 'A' && d <= 'Z') {
      d = d - 'A' + 10;
    } else if (d >= 'a' && d <= 'z') {
      d = d - 'a' + 10;
    } else {
      d = 99;
    }
    if (d >= base) break;
    n = n * base - d;
    nb_digits++;
    probe++;
  }

  if (endptr) {
    if (nb_digits == 0) {
      *endptr = (char *) str;
    } else {
      *endptr = (char *) probe;
    }
  }

  return neg ? n : -n;
}

long strtol(const char *str, char **endptr, int base) {
  return (long) strtoll(str, endptr, base);
}

unsigned long strtoul(const char *str, char **endptr, int base) {
  return (unsigned long) strtoull(str, endptr, base);
}

int atoi(const char *str) {
  return strtol(str, 0, 10);
}

char *getenv(const char *name) {
  return 0; /*TODO*/
}

void qswap (char *a, char *b, size_t size) {
  while (size > 0) {
    char tmp = *a;
    *a++ = *b;
    *b++ = tmp;
    size--;
  }
}

// Implement Lomuto partition scheme
size_t qpartition(void *base, size_t count, size_t size, int (*compare) (void const *, void const *)) {
  void *p = base + count * size;
  size_t i = 0;
  size_t j;
  for (j = 0; j < count; j++) {
    int c = compare(base + j * size, p);
    if (c < 0) {
      // j^th element < pivot => swap it with i^th element
      qswap (base + i * size, base + j * size, size);
      i++;
    } else if (c == 0) {
      // Small optimization, no need to swap when equal
      i++;
    }
  }

  int c2 = compare(base + count * size, base + i * size);
  if (c2 < 0)
    qswap (base + i * size, base + count * size, size);
  return i;
}

void qsort (void *base, size_t count, size_t size, int (*compare) (void const *, void const *)) {
  if (count > 1) {
    int p = qpartition(base, count - 1, size, compare);
    qsort (base, p, size, compare);
    qsort (base + p * size, count - p, size, compare);
  }
}
