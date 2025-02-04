#include <stdio.h>

#define DEF(id, str) str "\0"
#define DEF_ASM(x) DEF(TOK_ASM_ ## x, #x)

#define DEF_ASMTEST(x,suffix) \
 DEF_ASM(x ## o ## suffix) \
 DEF_ASM(x ## no ## suffix) \
 DEF_ASM(x ## b ## suffix) \
 DEF_ASM(x ## c ## suffix) \
 DEF_ASM(x ## nae ## suffix) \
 DEF_ASM(x ## nb ## suffix) \
 DEF_ASM(x ## nc ## suffix) \
 DEF_ASM(x ## ae ## suffix) \
 DEF_ASM(x ## e ## suffix) \
 DEF_ASM(x ## z ## suffix) \
 DEF_ASM(x ## ne ## suffix) \
 DEF_ASM(x ## nz ## suffix) \
 DEF_ASM(x ## be ## suffix) \
 DEF_ASM(x ## na ## suffix) \
 DEF_ASM(x ## nbe ## suffix) \
 DEF_ASM(x ## a ## suffix) \
 DEF_ASM(x ## s ## suffix) \
 DEF_ASM(x ## ns ## suffix) \
 DEF_ASM(x ## p ## suffix) \
 DEF_ASM(x ## pe ## suffix) \
 DEF_ASM(x ## np ## suffix) \
 DEF_ASM(x ## po ## suffix) \
 DEF_ASM(x ## l ## suffix) \
 DEF_ASM(x ## nge ## suffix) \
 DEF_ASM(x ## nl ## suffix) \
 DEF_ASM(x ## ge ## suffix) \
 DEF_ASM(x ## le ## suffix) \
 DEF_ASM(x ## ng ## suffix) \
 DEF_ASM(x ## nle ## suffix) \
 DEF_ASM(x ## g ## suffix)

const char tcc_keywords[] =
 DEF_ASMTEST(j,)
 "\0"
 ;

#define str str2
#define T(X, Y) X ## Y

void putstr(char* str) {
  while (*str != '\0') {
    putchar(*str);
    str += 1;
  }
}

void main() {
  char* str2 = "abcdef";
  char* kw = tcc_keywords;
  while (*kw != '\0' || *(kw + 1) != '\0') {
    if (*kw != 0) {
      putchar(*kw);
    } else {
      putchar(' ');
    }
    kw += 1;
  }
  putchar('\n');
  putstr(T(str, 2)); putchar('\n');
  putstr(T(str2, )); putchar('\n');
}
