void putstring(char * s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int main() {
  char * str = "LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,";
  int i = 0;
  while (i < 39) {
    //printf("%0.4s\n", str + i * 5);
    //the equivalent of the above line in C

    putstring(str + i * 5);
    putchar('\n');
    i++;
  }
}
