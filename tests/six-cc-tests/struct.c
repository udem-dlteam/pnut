struct LinkedList { 
  int val; 
  struct LinkedList *next; 
};

void putnumber(int n) {
  int q;
  if (n < 0) {
    putchar(45);
    n = 0 - n;
  }
  if (n / 10) {
    putnumber(n / 10);
  }
  q = n % 10;
  putchar(q + 48);
}

void putstring(const char *s) {
  while (*s) {
    putchar(*s);
    s++;
  }
}

struct LinkedList* iota_linked_list(int max) {
  int i;
  struct LinkedList *head;
  struct LinkedList *last;
  struct LinkedList *node;
  if (max == 0) return 0;
  head = malloc(sizeof(struct LinkedList));
  if (head == 0) return 0; // Handle memory allocation failure
  head->val = 0;
  head->next = 0;
  last = head;
  for (i = 1; i < max; i++) {
    node = malloc(sizeof(struct LinkedList));
    if (node == 0) {
      // Free previously allocated nodes in case of failure
      while (head != 0) {
        struct LinkedList *temp = head->next;
        free(head);
        head = temp;
      }
      return 0; // Handle memory allocation failure
    }
    node->val = i;
    node->next = 0;
    last->next = node;
    last = node;
  }
  return head;
}

int linked_list_sum(struct LinkedList* head) {
  int sum = 0;
  while (head != 0) {
    sum += head->val;
    head = head->next;
  }
  return sum;
}

int linked_list_sum_except_last(struct LinkedList* head) {
  int sum = 0;
  while (head != 0 && head->next != 0) {
    sum += head->val;
    head = head->next;
  }
  return sum;
}

int main() {
  struct LinkedList* ll;

  ll = iota_linked_list(1000);

  putstring("Sum: ");
  putnumber(linked_list_sum(ll));
  putchar('\n');
  putstring("Sum: ");
  putnumber(linked_list_sum_except_last(ll));
  putchar('\n');

  // Free the linked list
  while (ll != 0) {
    struct LinkedList *temp = ll->next;
    free(ll);
    ll = temp;
  }

  return 0;
}