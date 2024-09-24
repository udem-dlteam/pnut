#include <stdio.h>
int main() {
  int i = 0;

  switch (i) {
    case 0: {
      if (i == 0) {
        break;
      } else if (i >= 0) {
        printf("First statement");
        break;
        printf("Second statement");
      } else {
        printf("Third statement");
        break;
      }
    }
  }

  return 0;
}
