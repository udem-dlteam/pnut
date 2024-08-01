
void putstring(const char *s) {
  while (*s) {
    putchar(*s);
    s++;
  }
}

void putnumber(int n) {
  char * buffer = malloc(10 * sizeof(char));
  int i = 0;
  if (n == 0) {
    putchar('0');
    return;
  }
  if (n < 0) {
    putchar('-');
    n = -n;
  }

  while (n > 0) {
    buffer[i++] = (n % 10) + '0';
    n /= 10;
  }
  while (i > 0) {
    putchar(buffer[--i]);
  }
}

enum LinkedList {
  VAL,
  NEXT,
  LL_SIZE
};

int* iota_linked_list(int max) {
  int *head, *last, *node;
  int i = 1;
  head = malloc(LL_SIZE * sizeof(int));
  if (head == 0) {
    return -1; // Memory allocation failed
  }

  head[VAL] = 0;
  head[NEXT] = 0;
  last = head;

  while (i < max) {
    node = malloc(LL_SIZE * sizeof(int));
    if (node == 0) {

      while (head != 0) {
        int *temp = (int*)head[NEXT];
        free(head);
        head = temp;
      }
      return -1;
    }
    node[VAL] = i;
    node[NEXT] = 0;
    last[NEXT] = (int)node;
    last = node;
    i = i + 1;
  }

  return head;
}

int linked_list_sum(int *head) {
  int sum = 0;
  while (head != 0) {
    sum += head[VAL];
    head = (int*)head[NEXT];
  }
  return sum;
}

int main() {
  int *ll;
  int sum;

  ll = iota_linked_list(1000);
  if (ll == -1) {
    putstring("Memory allocation failed\n");
    return 1;
  }

  sum = linked_list_sum(ll);
  putstring("Sum: ");
  putnumber(sum);
  putchar('\n');

  // Free the linked list
  while (ll != 0) {
    int *temp = (int*)ll[NEXT];
    free(ll);
    ll = temp;
  }
  return 0;
}