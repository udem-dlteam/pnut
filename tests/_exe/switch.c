#include <stdio.h>

void empty_switch() {
  switch (1) {
    // Nothing
  }
}

void no_case_switch() {
  switch (1) {
    putchar('A');
    putchar('B');
    putchar('C');
  }
}

void basic_switch() {
  int a = 0;

  while (a < 5) {
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
    a++;
  }
}

void no_default_break_switch() {
  int a = 0;

  while (a < 5) {
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
    a++;
  }
}

void no_default_switch() {
  int a = 0;

  while (a < 5) {
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
    }
    a++;
  }
}

void default_first_switch() {
  int a = 0;

  while (a < 5) {
    switch (a) {
      default:
      case 1:
        putchar('A');
        break;
      case 2:
        putchar('B');
        break;
      case 3:
        putchar('C');
        break;
    }
    a++;
  }
}

void default_middle_switch() {
  int a = 0;

  while (a < 5) {
    switch (a) {
      case 1:
        putchar('A');
        break;
      case 2:
        putchar('B');
      default:
        putchar('-');
        putchar('D');
        putchar('-');
        break;
      case 3:
        putchar('C');
        break;
    }
    a++;
  }
}

void default_middle_switch2() {
  int a = 0;

  while (a < 5) {
    switch (a) {
      case 1:
        putchar('A');
        break;
      case 2:
        putchar('B');
      default:
        putchar('-');
        putchar('D');
        putchar('-');
        // No break here
      case 3:
        putchar('C');
        break;
    }
    a++;
  }
}

void goto_switch(int a) {

  switch (a) {
    case 1:
      putchar('A');
      break;
    case 2:
      putchar('B');
      break;
    case 3:
      putchar('C');
      goto end;
    default:
      putchar('D');
      break;
  }

  return;

  end:
  putchar('E');
}

void gotos_switch() {
  int a = 0;

  start:
  switch (a) {
    case 0:
      putchar('A');
      a = 1;
      goto start;
    case 1:
      putchar('B');
      a = 2;
      goto start;
    case 2:
      putchar('C');
      goto end;
    default:
      putchar('D');
      break;
  }

  return;

  end:
  putchar('E');
}

void switch_while() {
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

void nested_switch() {
  int a = 2;
  int b = 3;

  switch (a) {
    case 1:
      putchar('A');
      break;
    case 2:
      switch (b) {
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
      putchar('T');
      break;
    case 3:
      putchar('F');
      break;
    default:
      putchar('G');
      break;
  }
}

void duff_device_switch(int n) {
  int count = (n + 7) / 8;
  switch (n % 8) {
    case 0: do { putchar('A');
    case 7:      putchar('B');
    case 6:      putchar('C');
    case 5:      putchar('D');
    case 4:      putchar('E');
    case 3:      putchar('F');
    case 2:      putchar('G');
    case 1:      putchar('H');
              } while (--count > 0);
  }
}

int state = 0;
int i;
void state_machine_switch() {
  switch (state) {
    case 0:
      for (i = 0; i < 10; i++) {
        state = 1; // Next call will start at case 1
        putchar('A');
        return;
        case 1: putchar('B');
      }
  }
}

void bodiless_switch() {
  switch (123)
    case 123:
      putchar('E');

  switch (456)
      case 123:
      case 456:
          putchar('E');
}

int main() {
  empty_switch();             putchar('\n');
  no_case_switch();           putchar('\n');
  basic_switch();             putchar('\n');
  no_default_break_switch();  putchar('\n');
  no_default_switch();        putchar('\n');
  default_first_switch();     putchar('\n');
  default_middle_switch();    putchar('\n');
  default_middle_switch2();   putchar('\n');
  goto_switch(1);             putchar('\n');
  goto_switch(3);             putchar('\n');
  gotos_switch();             putchar('\n');
  switch_while();             putchar('\n');
  nested_switch();            putchar('\n');
  duff_device_switch(15);     putchar('\n');
  state_machine_switch();
  state_machine_switch();     putchar('\n');
  bodiless_switch();          putchar('\n');
  return 0;
}
