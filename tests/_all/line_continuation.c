// comp_pnut_opt: -DSUPPORT_LINE_CONTINUATION
#include <stdio.h>

void putint_aux(int n) {
  if (n >= 10) putint_aux(n / 10);
  putchar('0' + (n % 10));
}

void putint(int n) {
  if (n < 0) {
    putchar('-');
    putint_aux(-n);
  } else {
    putint_aux(n);
  }
}

int main() {

/**/
int foo = 0;

/\
*
*/ fo\
\
\
o +\
= 1\
\
10\
200;

    putint(foo);
    return 0;
}
