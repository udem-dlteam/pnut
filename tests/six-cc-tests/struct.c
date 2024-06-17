/* Simulating structs using enums like in c4.c */
struct LinkedList { 
  int val; 
  int * next; 
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

void putstring(char * s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int * iota_linked_list(int max) {
  int i;
  int * head;
  int * last;
  int * node;
  if (max == 0) return 0;
  head = malloc(2);
  head->val = 0;
  last = head;
  i = 1;
  while (i < max) {
    node = malloc(2);
    node->val = i;
    node->next = 0;
    last->next = node;
    last = node;

    i++;
  }

  return head;
}

int linked_list_sum(int* head) {
  int sum;
  sum = 0;
  while (head != 0) {
    sum += head->val;
    head = head->next;
  }

  return sum;
}

int linked_list_sum_except_last(int* head) {
  int sum;
  sum = 0;
  while (head != 0 && head->next != 0) {
    sum += head->val;
    head = head->next;
  }

  return sum;
}

int main() {
  int* ll;

  ll = iota_linked_list(1000);

  putstring("Sum: ");
  putnumber(linked_list_sum(ll));
  putchar('\n');
  putstring("Sum: ");
  putnumber(linked_list_sum_except_last(0));
  putchar('\n');
  putstring("Sum: ");
  putnumber(linked_list_sum_except_last(ll));
  putchar('\n');

  return 0;
}
