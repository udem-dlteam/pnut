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
