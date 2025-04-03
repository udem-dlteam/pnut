#include "../include/stdlib.h"
#include "../include/stdio.h"

#define HEAP_SIZE 1000000000

char _heap[HEAP_SIZE];
int _heap_alloc = 0;

void *malloc(size_t size) {
  if (size <= 0) return 0; // no-op

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

#ifdef PNUT_CC

void putstr(const char *str) {
  while (*str) {
    putchar(*str);
    str++;
  }
}

double strtod(const char *str, char **endptr) {
  // Support the literals that are used in TCC:
  // 0.0, 1.0, 4294967296.0
  if (str[0] == '0' && str[1] == '.' && str[2] == '0' && str[3] == 0) {
    if (endptr) *endptr = (char*)str + 3;
    return 0x0;;
  } else if (str[0] == '1' && str[1] == '.' && str[2] == '0' && str[3] == 0) {
    if (endptr) *endptr = (char*)str + 3;
    return 0x3ff0000000000000;
  } else if (str[0] == '4' && str[1] == '2' && str[2] == '9' && str[3] == '4' && str[4] == '9' && str[5] == '6' && str[6] == '7' && str[7] == '2' && str[8] == '9' && str[9] == '6' && str[10] == '.' && str[11] == '0' && str[12] == 0) {
    if (endptr) *endptr = (char*)str + 12;
    return 0x41d0000000000000;
  } else {
    putstr("Unknown strtod: ");
    putstr(str);
    exit(1);
    return 0; /*TODO*/
  }
}

float strtof(const char *str, char **endptr) {
  putstr("Unknown strtof: ");
  putstr(str);
  exit(1);
}

long double strtold(const char *str, char **endptr) {
  putstr("Unknown strtold: ");
  putstr(str);
  exit(1);
}

unsigned long long strtoull(const char *nptr, char **endptr, int base) {
  if (base == 0) { // base = 0 => determine base with prefix
    // 0x   => base 16
    // 0    => base 8
    // else => base 10
    if (nptr[0] == '0') {
      if (nptr[1] == 'x') {
        base = 16;
        nptr += 2;
      } else {
        base = 8;
      }
    } else {
      base = 10;
    }
  }

  unsigned long long n = 0;
  switch (base) {
    case 8:
      while (*nptr >= '0' && *nptr <= '7') {
        n = n*8 + *nptr - '0';
        nptr++;
      }
      break;
    case 10:
      while (*nptr >= '0' && *nptr <= '9') {
        n = n*10 + *nptr - '0';
        nptr++;
      }
      break;
    case 16:
      while (1) {
        if (*nptr >= '0' && *nptr <= '9') {
          n = n*16 + *nptr - '0';
        } else if (*nptr >= 'A' && *nptr <= 'F') {
          n = n*16 + *nptr - 'A' + 10;
        } else if (*nptr >= 'a' && *nptr <= 'f') {
          n = n*16 + *nptr - 'a' + 10;
        } else {
          break;
        }
        nptr++;
      }
      break;
    default:
      putstr("Unknown strtoull base");
      exit(1);
      return -1;
  }

  if (endptr) {
    *endptr = (char*)nptr;
  }

  return n;
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

char* qsort_buf1;
char* qsort_buf2;
char* qsort_buf3;
int qsort_buf_len = -1;

void *memcpy(void *dest, const void *src, size_t n);

int compare(int (*compar)(void *, void *)) {
  return compar(qsort_buf1, qsort_buf2);
}

void swap(void *base, size_t size, int i, int j) {
  memcpy(qsort_buf3, base + i*size, size);    // temp = base[i]
  memcpy(base + i*size, base + j*size, size); // base[i] = base[j]
  memcpy(base + j*size, qsort_buf3, size);    // base[j] = temp
}

void qsort(void *base, size_t nmemb, size_t size, int (*compar)(void *, void *)) {
  if (qsort_buf_len < size) {
    qsort_buf1 = realloc(qsort_buf1, size);
    qsort_buf2 = realloc(qsort_buf2, size);
    qsort_buf3 = realloc(qsort_buf3, size);
    qsort_buf_len = size;
  }

  int has_swapped = 1;
  int i;
  // Simple bubble sort
  while (has_swapped) {
    has_swapped = 0;

    for (i = 0; i < nmemb - 1; i++) {
      memcpy(qsort_buf1, base + i*size, size);
      memcpy(qsort_buf2, base + (i+1)*size, size);
      if (compare(compar) > 0) {
        swap(base, size, i, i+1);
        has_swapped = 1;
      }
    }
  }
}
