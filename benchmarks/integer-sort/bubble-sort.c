// Allocate a large array and sort its elements using bubble sort.

// #define ARR_SIZE 10000

void bubbleSort(int *arr, int n) {
    int i, j, temp;
    for (i = 0; i < n-1; i++) {
        for (j = 0; j < n-i-1; j++) {
            if (arr[j] > arr[j+1]) {
                temp = arr[j];
                arr[j] = arr[j+1];
                arr[j+1] = temp;
            }
        }
    }
}

void main() {
    int* arr = malloc(ARR_SIZE * sizeof(int));
    int i;

    // Initialize the array with random values
    for (i = 0; i < ARR_SIZE; i++) {
        arr[i] = rand() % 10000;
    }

    // Sort the array
    bubbleSort(arr, ARR_SIZE);
    free(arr);
}
