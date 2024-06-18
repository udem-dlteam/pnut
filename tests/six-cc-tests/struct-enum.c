/* Simulating structs using enums like in c4.c */
enum LinkedList { 
  VAL, 
  NEXT,
  LL_SIZE };

int * iota_linked_list(int max) {
  int i;
  int * head;
  int * last;
  int * node;
  if (max == 0) return 0;
  head = malloc(LL_SIZE);
  head[VAL] = 0;
  last = head;
  i = 1;
  while (i < max) {
    node = malloc(LL_SIZE);
    node[VAL] = i;
    node[NEXT] = 0;
    last[NEXT] = node;
    last = node;

    i++;
  }

  return head;
}

int linked_list_sum(int * head) {
  int sum;
  sum = 0;
  while (head != 0) {
    sum += head[VAL];
    head = head[NEXT];
  }

  return sum;
}

int main() {
  int * ll;
  int sum;
  char * str;

  ll = iota_linked_list(1000);

  sum = linked_list_sum(ll);
  printf("Sum: %d\n", sum);
}
