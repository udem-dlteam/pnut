/*
 * compile with:
 *
 *   $ ksh pnut.sh sha256sum.c > sha256sum.sh
 *
 * execute with:
 *
 *   $ ksh sha256sum.sh FILE
 */

#include <stdio.h>

#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

#define BLOCK_SIZE 64

int k[64];

void sha256_setup() {
  k[ 0] = 0x428a2f98;
  k[ 1] = 0x71374491;
  k[ 2] = 0xb5c0fbcf;
  k[ 3] = 0xe9b5dba5;
  k[ 4] = 0x3956c25b;
  k[ 5] = 0x59f111f1;
  k[ 6] = 0x923f82a4;
  k[ 7] = 0xab1c5ed5;
  k[ 8] = 0xd807aa98;
  k[ 9] = 0x12835b01;
  k[10] = 0x243185be;
  k[11] = 0x550c7dc3;
  k[12] = 0x72be5d74;
  k[13] = 0x80deb1fe;
  k[14] = 0x9bdc06a7;
  k[15] = 0xc19bf174;
  k[16] = 0xe49b69c1;
  k[17] = 0xefbe4786;
  k[18] = 0x0fc19dc6;
  k[19] = 0x240ca1cc;
  k[20] = 0x2de92c6f;
  k[21] = 0x4a7484aa;
  k[22] = 0x5cb0a9dc;
  k[23] = 0x76f988da;
  k[24] = 0x983e5152;
  k[25] = 0xa831c66d;
  k[26] = 0xb00327c8;
  k[27] = 0xbf597fc7;
  k[28] = 0xc6e00bf3;
  k[29] = 0xd5a79147;
  k[30] = 0x06ca6351;
  k[31] = 0x14292967;
  k[32] = 0x27b70a85;
  k[33] = 0x2e1b2138;
  k[34] = 0x4d2c6dfc;
  k[35] = 0x53380d13;
  k[36] = 0x650a7354;
  k[37] = 0x766a0abb;
  k[38] = 0x81c2c92e;
  k[39] = 0x92722c85;
  k[40] = 0xa2bfe8a1;
  k[41] = 0xa81a664b;
  k[42] = 0xc24b8b70;
  k[43] = 0xc76c51a3;
  k[44] = 0xd192e819;
  k[45] = 0xd6990624;
  k[46] = 0xf40e3585;
  k[47] = 0x106aa070;
  k[48] = 0x19a4c116;
  k[49] = 0x1e376c08;
  k[50] = 0x2748774c;
  k[51] = 0x34b0bcb5;
  k[52] = 0x391c0cb3;
  k[53] = 0x4ed8aa4a;
  k[54] = 0x5b9cca4f;
  k[55] = 0x682e6ff3;
  k[56] = 0x748f82ee;
  k[57] = 0x78a5636f;
  k[58] = 0x84c87814;
  k[59] = 0x8cc70208;
  k[60] = 0x90befffa;
  k[61] = 0xa4506ceb;
  k[62] = 0xbef9a3f7;
  k[63] = 0xc67178f2;
}

#define u32mask 0xffffffff
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
  hash[0] = 0x6a09e667;
  hash[1] = 0xbb67ae85;
  hash[2] = 0x3c6ef372;
  hash[3] = 0xa54ff53a;
  hash[4] = 0x510e527f;
  hash[5] = 0x9b05688c;
  hash[6] = 0x1f83d9ab;
  hash[7] = 0x5be0cd19;
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
