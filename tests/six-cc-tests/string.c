void putstring(char *s, int length) {
  int i = 0;
  while (i < length && s[i]) {
    putchar(s[i]);
    i++;
  }
}

int main() {
  char * str = "LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,";
  int i = 0;
  while (i < 39) {
    putstring(str + i * 5, 4);
    putchar('\n');
    i++;
  }
  return 0;
}
