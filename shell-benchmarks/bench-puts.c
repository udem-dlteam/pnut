int atoi(const char* str) {
  int res = 0;
  for (;*str != '\0'; ++str) {
    res = res * 10 + *str - '0';
  }
  return res;
}

char c;
void puts_fast(char *str) {
  while (c = *str) {
    putchar(c);
    str += 1;
  }
}

void puts(char *str) {
  while (*str) {
    putchar(*str);
    str += 1;
  }
}

void main(int argc, char** argv) {
  int count = 1;
  char *str;
  if (argc != 4) {
    puts("Usage: bench-puts <string> <count> <method>\n");
    exit(1);
  }
  str = argv[1];
  count = atoi(argv[2]);
  if (argv[3][0] == 'f') {
    while (count--) {
      puts_fast(str);
    }
  } else {
    while (count--) {
      puts(str);
    }
  }
}
