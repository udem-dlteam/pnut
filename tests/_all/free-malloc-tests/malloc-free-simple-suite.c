
// Test Functions
void test_basic_allocation() {
    int *ptr = malloc(4);
    if (ptr) {
      *ptr = 42;
      if (*ptr == 42) {
        putchar('P');
      } else {
        putchar('X');
      }
      free(ptr);
    } else {
      putchar('X');
    }
}

void test_array_allocation() {
  int *arr = malloc(10 * 4);
  int i = 0;
  if (arr) {
    while (i < 10) {
      arr[i] = i;
      i++;
    }
    i = 0;
    while (i < 10) {
      if (arr[i] != i) {
        putchar('X');
        free(arr);
        return;
      }
      i++;
    }
      putchar('A');
      free(arr);
  } else {
      putchar('X');
  }
}

void test_zero_allocation() {
  int *ptr = malloc(0);
  if (ptr) {
    putchar('S');
    free(ptr);
  } else {
    putchar('X');
  }
}

void test_large_allocation() {
  int large_size = 1024; // That doesn't cause a heap overflow
  int *ptr = malloc(large_size);
  if (ptr) {
    putchar('S');
    free(ptr);
  } else {
    putchar('X');
  }
}

void test_multiple_allocations() {
  int *ptr1 = malloc(4);
  int *ptr2 = malloc(2 * 4);
  int *ptr3 = malloc(3 * 4);

  if (ptr1 && ptr2 && ptr3) {
    putchar('!');
  } else {
    putchar('X');
  }

  free(ptr1);
  free(ptr2);
  free(ptr3);
}

// Main Function
int main() {
  test_basic_allocation();
  test_array_allocation();
  test_zero_allocation();
  test_large_allocation();
  test_multiple_allocations();

  putchar(':');
  putchar(')');
  putchar(10);

  return 0;
}
