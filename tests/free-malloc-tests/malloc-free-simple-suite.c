int test_status = 0; // 0: not run, 1: pass, -1: fail

void observer(int status) {
    test_status = status;
}

// Test Functions
void test_basic_allocation() {
    int *ptr = malloc(4);
    if (ptr) {
        *ptr = 42;
        if (*ptr == 42) {
            observer(1); // Test passed
        } else {
            observer(-1); // Test failed
        }
        free(ptr);
    } else {
        observer(-1); // Test failed
    }
}

void test_array_allocation() {
    int *arr = malloc(10 * 4);
    if (arr) {
        int i = 0;
        while (i < 10) {
            arr[i] = i;
            i++;
        }
        i = 0;
        while (i < 10) {
            if (arr[i] != i) {
                observer(-1); // Test failed
                free(arr);
                return;
            }
            i++;
        }
        observer(1); // Test passed
        free(arr);
    } else {
        observer(-1); // Test failed
    }
}

void test_zero_allocation() {
    int *ptr = malloc(0);
    if (ptr) {
        observer(1); // Non-standard but acceptable behavior
        free(ptr);
    } else {
        observer(1); // Standard behavior (NULL pointer)
    }
}

void test_large_allocation() {
    int large_size = 1024; // That doesn't cause a heap overflow
    int *ptr = malloc(large_size);
    if (ptr) {
        observer(1); // Test passed
        free(ptr);
    } else {
        observer(-1); // Test failed
    }
}

void test_multiple_allocations() {
    int *ptr1 = malloc(4);
    int *ptr2 = malloc(2 * 4);
    int *ptr3 = malloc(3 * 4);

    if (ptr1 && ptr2 && ptr3) {
        observer(1); // Test passed
    } else {
        observer(-1); // Test failed
    }

    free(ptr1);
    free(ptr2);
    free(ptr3);
}

// Main Function
int main() {
    test_basic_allocation();
    if (test_status == 1) {
        putchar('1');
        putchar(10);
    } else {
        putchar('x');
        putchar(10);
    }

    test_array_allocation();
    if (test_status == 1) {
        putchar('2');
        putchar(10);
    } else {
        putchar('x');
        putchar(10);
    }

    test_zero_allocation();
    if (test_status == 1) {
        putchar('3');
        putchar(10);
    } else {
        putchar('x');
        putchar(10);
    }


    test_large_allocation();
    if (test_status == 1) {
        putchar('4');
        putchar(10);
    } else {
        putchar('x');
        putchar(10);
    }

    test_multiple_allocations();
    if (test_status == 1) {
        putchar('5');
        putchar(10);
    } else {
        putchar('x');
        putchar(10);
    }

    putchar(':');
    putchar(')');
    putchar(10);

    return 0;
}