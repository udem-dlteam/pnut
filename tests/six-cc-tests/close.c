void putstring(char* s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

int hash(char* s) {
  int hash;
  int ix;
  hash = 0;
  ix = 0;
  while (s[ix] != 0) {
    hash = s[ix] + (hash << 6) + (hash << 16) - hash;
    hash = hash & 65535; /* Most shells have 32-bit integers, so we have to make sure we don't "overflow" */
    ix = ix + 1;
  }
  return ix;
}

int main() {
  int MAX_SIZE;
  int f;
  char* s;
  int len;
  int h;
  char ch;
  MAX_SIZE = 200;
  s = malloc(MAX_SIZE);
  f = fopen("tests/six-cc-tests/close.c", 0);
  while ((ch = fgetc(f)) != -1 && len < MAX_SIZE - 1) {
    s[len] = ch;
    len = len + 1;
  }
  s[len] = '\0'; 
  putstring("Read content: ");
  putstring(s);
  putchar('\n');
  putstring("File descriptor: ");
  putchar(f + 48);
  putchar('\n');
  putstring("Quote: \"\n");
  putstring("Backslash: \\\n");
  putstring("Read len: ");
  if(len == 199){
    putstring("199\n");
  } else{
    putchar(len + 48);
    putchar('\n');
  }
  putstring("Read result: ");
  putstring(s);
  putchar('\n');
  putstring("hash: ");
  h = hash(s);
  if(h == 199){
    putstring("199\n");
  } else{
    putchar(h + 48);
    putchar('\n');
  }
  fclose(f);
  free(s);
  return 0;
}
