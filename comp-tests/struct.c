/* Simulating structs using enums like in c4.c */
enum LinkedList() { VAL; NEXT; LL_SIZE; }

/* struct LinkedListNode(void_ptr value, void_ptr next) { } */

int_ptr iota_linked_list(int max) {
  int i;
  int_ptr head;
  int_ptr last;
  int_ptr node;
  if (max == 0) return NULL;
  head = malloc(LL_SIZE);
  head[VAL] = 0;
  last = head;
  i = 1;
  while (i < max) {
    node = malloc(LL_SIZE);
    node[VAL] = i;
    node[NEXT] = NULL;
    last[NEXT] = node;
    last = node;

    i++;
  }

  return head;
}

int linked_list_sum(int_ptr head) {
  int sum;
  sum = 0;
  while (head != NULL) {
    sum += head[VAL];
    head = head[NEXT];
  }

  return sum;
}

int main() {
  int_ptr ll;
  int sum;
  char_ptr str;

  ll = iota_linked_list(1000);

  sum = linked_list_sum(ll);
  printf("Sum: %d\n", sum);
  str = "LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,";
  printf("%0.4s\n", str[1 * 5]);
  exit(sum);
}
