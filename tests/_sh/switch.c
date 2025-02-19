void simple_switch(){
  int a = 2;

  switch (a) {
    case 1:
      putchar('A');
      break;
    case 2:
      putchar('B');
      break;
    case 3:
      putchar('C');
      break;
    default:
      putchar('D');
      break;
  }
}

// Fall through between cases is not permitted, but fall through of the last case is
void simple_switch_fall_through(){
  int a = 3;

  switch (a) {
    case 1:
      putchar('A');
      break;
    case 2:
      putchar('B');
      break;
    case 3:
      putchar('C');
  }

  switch (a) {
    case 1:
      putchar('A');
      break;
    case 2:
      putchar('B');
      break;
    case 3:
      putchar('C');
      break;
    default:
      putchar('D');
  }
}

void multiple_labels(){
  int a = 3;

  switch (a) {
    case 0:
    case 2:
    case 4:
    case 6:
    case 8:
      putchar('A');
      break;
    case 1:
    case 3:
    case 5:
    case 7:
    case 9:
      putchar('B');
      break;
  }
}

void switch_in_while(){
  int i = 0;

  while (i < 5) {
    switch (i) {
      case 0:
        putchar('A');
        break;
      case 1:
        putchar('B');
        break;
      case 2:
        putchar('C');
        break;
      case 3:
        putchar('D');
        break;
      default:
        putchar('E');
        break;
    }
    i++;
  }
}

void bodiless_switch() {
  switch (123)
    case 123:
      putchar('F');

  switch (456)
      case 123:
      case 456:
          putchar('E');
}

void main() {
  simple_switch();
  simple_switch_fall_through();
  multiple_labels();
  switch_in_while();
  bodiless_switch();
}
