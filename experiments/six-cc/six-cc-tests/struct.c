/* Simulating structs using enums like in c4.c */
struct LinkedList() { void val; void next; }

int_ptr iota_linked_list(int max) {
  int i;
  int_ptr head;
  int_ptr last;
  int_ptr node;
  if (max == 0) return NULL;
  head = malloc(2);
  head->val = 0;
  last = head;
  i = 1;
  while (i < max) {
    node = malloc(2);
    node->val = i;
    node->next = NULL;
    last->next = node;
    last = node;

    i++;
  }

  return head;
}

int linked_list_sum(int_ptr head) {
  int sum;
  sum = 0;
  while (head != NULL) {
    sum += head->val;
    head = head->next;
  }

  return sum;
}

int linked_list_sum_except_last(int_ptr head) {
  int sum;
  sum = 0;
  while (head != NULL && head->next != NULL) {
    sum += head->val;
    head = head->next;
  }

  return sum;
}

int main() {
  int_ptr ll;

  ll = iota_linked_list(1000);

  printf("Sum: %d\n", linked_list_sum(ll));
  printf("Sum: %d\n", linked_list_sum_except_last(NULL));
  printf("Sum: %d\n", linked_list_sum_except_last(ll));
}
