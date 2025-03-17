#include <stdlib.h>
#include <stdio.h>
#include <stdint.h> // for intptr_t

#ifdef PNUT_CC
// When bootstrapping pnut, intptr_t is not defined.
// On 64 bit platforms, intptr_t is a long long int.
// On 32 bit (including shells) platforms, intptr_t is an int.
#if defined(PNUT_EXE_64)
typedef long long int intptr_t;
#else
typedef int intptr_t;
#endif
#endif

void putstr(const char *s) {
  while (*s) {
    putchar(*s);
    s++;
  }
}

void putint(int n) {
  if (n < 0) {
    putchar('-');
    n = -n;
  }
  if (n >= 10) {
    putint(n / 10);
  }
  putchar('0' + n % 10);
}

enum LinkedList {
  VAL,
  NEXT,
  LL_SIZE
};

intptr_t *iota_linked_list(int max) {
  intptr_t *head, *last, *node;
  int i = 1;

  head = (intptr_t *) malloc(LL_SIZE * sizeof(intptr_t));
  head[VAL] = 0;
  head[NEXT] = 0;
  last = head;

  while (i < max) {
    node = (intptr_t *) malloc(LL_SIZE * sizeof(intptr_t));
    node[VAL] = i;
    node[NEXT] = 0;
    last[NEXT] = (intptr_t) node;
    last = node;
    i = i + 1;
  }

  return head;
}

int linked_list_sum(intptr_t *head) {
  int sum = 0;
  while (head != 0) {
    sum += head[VAL];
    head = (intptr_t *) head[NEXT];
  }
  return sum;
}

int main() {
  intptr_t *ll = iota_linked_list(1000);
  int sum = linked_list_sum(ll);
  putstr("Sum: ");
  putint(sum);
  putchar('\n');

  return 0;
}
