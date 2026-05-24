/*
 * compile with:
 *
 *   $ ksh pnut.sh base64.c > base64.sh
 *
 * execute with:
 *
 *   $ ksh base64.sh < FILE
 */

// #define BUF_SIZE 1024

char buf[BUF_SIZE];

void cat_fd(int fd) {
}

char *codes = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

void encode() {
  int b1;
  int b2;
  int b3;
  while (1) {
    b1 = getchar();
    if (b1 < 0) break;
    b2 = getchar();
    putchar(codes[b1 >> 2]);
    if (b2 < 0) {
      putchar(codes[0x3f & (b1 << 4)]);
      putchar('=');
      putchar('=');
      break;
    } else {
      putchar(codes[0x3f & ((b1 << 4) | (b2 >> 4))]);
      b3 = getchar();
      if (b3 < 0) {
        putchar(codes[0x3f & (b2 << 2)]);
        putchar('=');
        break;
      } else {
        putchar(codes[0x3f & ((b2 << 2) | (b3 >> 6))]);
        putchar(codes[0x3f & b3]);
      }
    }
  }
  putchar('\n');
}

int lut[256];

int get() {
  int c;
  while ((c = getchar()) >= 0) {
    if ((c = lut[c]) >= 0) break;
  }
  return c;
}

void decode() {
  int i;
  int c1;
  int c2;
  int c3;
  int c4;
  for (i=0; i<256; ++i) lut[i] = -1;
  for (i=0; i<64; ++i) lut[0xff & codes[i]] = i;
  while ((c1 = get()) >= 0) {
    if ((c2 = get()) < 0) exit(1);
    putchar((c1 << 2) | (c2 >> 4));
    if ((c3 = get()) < 0) break;
    putchar(0xff & ((c2 << 4) | (c3 >> 2)));
    if ((c4 = get()) < 0) break;
    putchar(0xff & ((c3 << 6) | c4));
  }
}

int main(int argc, char **myargv) {

  if (argc == 1) {
    encode();
  } else if (argc == 2 &&
      myargv[1][0] == '-' && myargv[1][1] == 'd' && myargv[1][2] == '\0') {
    decode();
  } else {
    exit(1);
  }

  return 0;
}
