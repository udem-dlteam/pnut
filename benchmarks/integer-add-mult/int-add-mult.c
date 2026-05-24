// Allocate a large array and sum its elements.

// #define ARR_SIZE 10000

void main() {
  int* arr = malloc(ARR_SIZE * sizeof(int));
  int sum = 0;
  int i = 0;

  // Initialize the array
  for (i = 0; i < ARR_SIZE; i++) {
    arr[i] = i;
  }

  // Sum the elements of the array
  for (i = 0; i < ARR_SIZE; i++) {
    sum += arr[i] * i;
  }

  free(arr);
}
