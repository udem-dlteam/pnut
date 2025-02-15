/*
 * sha256sum.c: Compute the SHA-256 hash of files passed as arguments
 *
 * Usage: ./sha256sum.sh <files>
 */

#include <stdio.h>

#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

#define BLOCK_SIZE 64

int k[64];

void sha256_setup() {
  k[ 0] = 0x428a << 16 | 0x2f98;
  k[ 1] = 0x7137 << 16 | 0x4491;
  k[ 2] = 0xb5c0 << 16 | 0xfbcf;
  k[ 3] = 0xe9b5 << 16 | 0xdba5;
  k[ 4] = 0x3956 << 16 | 0xc25b;
  k[ 5] = 0x59f1 << 16 | 0x11f1;
  k[ 6] = 0x923f << 16 | 0x82a4;
  k[ 7] = 0xab1c << 16 | 0x5ed5;
  k[ 8] = 0xd807 << 16 | 0xaa98;
  k[ 9] = 0x1283 << 16 | 0x5b01;
  k[10] = 0x2431 << 16 | 0x85be;
  k[11] = 0x550c << 16 | 0x7dc3;
  k[12] = 0x72be << 16 | 0x5d74;
  k[13] = 0x80de << 16 | 0xb1fe;
  k[14] = 0x9bdc << 16 | 0x06a7;
  k[15] = 0xc19b << 16 | 0xf174;
  k[16] = 0xe49b << 16 | 0x69c1;
  k[17] = 0xefbe << 16 | 0x4786;
  k[18] = 0x0fc1 << 16 | 0x9dc6;
  k[19] = 0x240c << 16 | 0xa1cc;
  k[20] = 0x2de9 << 16 | 0x2c6f;
  k[21] = 0x4a74 << 16 | 0x84aa;
  k[22] = 0x5cb0 << 16 | 0xa9dc;
  k[23] = 0x76f9 << 16 | 0x88da;
  k[24] = 0x983e << 16 | 0x5152;
  k[25] = 0xa831 << 16 | 0xc66d;
  k[26] = 0xb003 << 16 | 0x27c8;
  k[27] = 0xbf59 << 16 | 0x7fc7;
  k[28] = 0xc6e0 << 16 | 0x0bf3;
  k[29] = 0xd5a7 << 16 | 0x9147;
  k[30] = 0x06ca << 16 | 0x6351;
  k[31] = 0x1429 << 16 | 0x2967;
  k[32] = 0x27b7 << 16 | 0x0a85;
  k[33] = 0x2e1b << 16 | 0x2138;
  k[34] = 0x4d2c << 16 | 0x6dfc;
  k[35] = 0x5338 << 16 | 0x0d13;
  k[36] = 0x650a << 16 | 0x7354;
  k[37] = 0x766a << 16 | 0x0abb;
  k[38] = 0x81c2 << 16 | 0xc92e;
  k[39] = 0x9272 << 16 | 0x2c85;
  k[40] = 0xa2bf << 16 | 0xe8a1;
  k[41] = 0xa81a << 16 | 0x664b;
  k[42] = 0xc24b << 16 | 0x8b70;
  k[43] = 0xc76c << 16 | 0x51a3;
  k[44] = 0xd192 << 16 | 0xe819;
  k[45] = 0xd699 << 16 | 0x0624;
  k[46] = 0xf40e << 16 | 0x3585;
  k[47] = 0x106a << 16 | 0xa070;
  k[48] = 0x19a4 << 16 | 0xc116;
  k[49] = 0x1e37 << 16 | 0x6c08;
  k[50] = 0x2748 << 16 | 0x774c;
  k[51] = 0x34b0 << 16 | 0xbcb5;
  k[52] = 0x391c << 16 | 0x0cb3;
  k[53] = 0x4ed8 << 16 | 0xaa4a;
  k[54] = 0x5b9c << 16 | 0xca4f;
  k[55] = 0x682e << 16 | 0x6ff3;
  k[56] = 0x748f << 16 | 0x82ee;
  k[57] = 0x78a5 << 16 | 0x636f;
  k[58] = 0x84c8 << 16 | 0x7814;
  k[59] = 0x8cc7 << 16 | 0x0208;
  k[60] = 0x90be << 16 | 0xfffa;
  k[61] = 0xa450 << 16 | 0x6ceb;
  k[62] = 0xbef9 << 16 | 0xa3f7;
  k[63] = 0xc671 << 16 | 0x78f2;
}

#define u32mask ((0xffff << 16 | 0xffff))
#define u31mask 0x7fffffff
#define u29mask 0x1fffffff
#define u22mask 0x003fffff

#define rot32(n, shift) (((n >> shift) & (u31mask >> (shift-1))) | ((n << (32 - shift)) & u32mask))

int w[64];
int nbits;
int hash[8];
int temp[8];

void sha256_init() {
  nbits = 0;
  hash[0] = 0x6a09 << 16 | 0xe667;
  hash[1] = 0xbb67 << 16 | 0xae85;
  hash[2] = 0x3c6e << 16 | 0xf372;
  hash[3] = 0xa54f << 16 | 0xf53a;
  hash[4] = 0x510e << 16 | 0x527f;
  hash[5] = 0x9b05 << 16 | 0x688c;
  hash[6] = 0x1f83 << 16 | 0xd9ab;
  hash[7] = 0x5be0 << 16 | 0xcd19;
}

void sha256_add_block(char *bytes) {

  int b0;
  int b1;
  int b2;
  int b3;
  int s0;
  int s1;
  int i;
  int ch;
  int t1;
  int ma;
  int t2;

  for (i=0; i<16; ++i) {
    b0 = 0xff & bytes[i*4];
    b1 = 0xff & bytes[i*4+1];
    b2 = 0xff & bytes[i*4+2];
    b3 = 0xff & bytes[i*4+3];
    w[i] = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
  }

  for (i=16; i<64; ++i) {
    s0 = rot32(w[i-15], 7) ^ rot32(w[i-15], 18) ^ ((w[i-15] >> 3) & u29mask);
    s1 = rot32(w[i-2], 17) ^ rot32(w[i-2], 19) ^ ((w[i-2] >> 10) & u22mask);
    w[i] = (w[i-16] + s0 + w[i-7] + s1) & u32mask;
  }

  for (i=0; i<8; ++i) temp[i] = hash[i];

  for (i=0; i<64; ++i) {

    s1 = rot32(temp[4], 6) ^ rot32(temp[4], 11) ^ rot32(temp[4], 25);
    ch = (temp[4] & temp[5]) ^ (~temp[4] & temp[6]);
    t1 = (temp[7] + s1 + ch + k[i] + w[i]) & u32mask;
    s0 = rot32(temp[0], 2) ^ rot32(temp[0], 13) ^ rot32(temp[0], 22);
    ma = (temp[0] & temp[1]) ^ (temp[0] & temp[2]) ^ (temp[1] & temp[2]);
    t2 = (s0 + ma) & u32mask;

    temp[7] = temp[6];
    temp[6] = temp[5];
    temp[5] = temp[4];
    temp[4] = (temp[3] + t1) & u32mask;
    temp[3] = temp[2];
    temp[2] = temp[1];
    temp[1] = temp[0];
    temp[0] = (t1 + t2) & u32mask;
  }

  for (i=0; i<8; ++i) hash[i] = (hash[i] + temp[i]) & u32mask;
}

char buf[BLOCK_SIZE];

void hex(int byte) {
  char *digits = "0123456789abcdef";
  putchar(digits[0xf & (byte >> 4)]);
  putchar(digits[0xf & byte]);
}

int process_file(char *filename) {

  int i;
  int fd;
  int n = BLOCK_SIZE;
  int h;

  sha256_setup();
  sha256_init();

  fd = open(filename, 0);

  while (n == BLOCK_SIZE) {

    n = read(fd, buf, BLOCK_SIZE);

    if (n < 0) return 1;

    nbits += 8*n;

    if (n < BLOCK_SIZE) {

      buf[n] = 0x80;

      for (i=n+1; i<BLOCK_SIZE; ++i) buf[i] = 0;

      if (n >= BLOCK_SIZE-9) {
        sha256_add_block(buf);
        for (i=0; i<BLOCK_SIZE-8; ++i) buf[i] = 0;
      }

      for (i=1; i<=8; ++i) {
        buf[BLOCK_SIZE-i] = 0xff & nbits;
        nbits >>= 8;
      }
    }

    sha256_add_block(buf);
  }

  close(fd);

  for (i=0; i<8; ++i) {
    h = hash[i];
    hex(h >> 24);
    hex(h >> 16);
    hex(h >> 8);
    hex(h);
  }

  putchar(' ');
  putchar(' ');

  while (*filename) {
    putchar(*filename);
    ++filename;
  }

  putchar('\n');

  return 0;
}

int main(int argc, char **myargv) {

  int i;

  for (i=1; i<argc; ++i) {
    if (process_file(myargv[i]) != 0) break;
  }

  return 0;
}
