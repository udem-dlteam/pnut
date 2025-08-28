// Allocate a large array and sum its elements.
// Runtime should be proportional to ARR_SIZE, but some shells show superlinear
// running time.

// #define ARR_SIZE 10000

void main() {
  int* arr = malloc(ARR_SIZE);
  int i = 0;
  for (i = 0; i < ARR_SIZE; i++) {
    arr[i] = 0;
  }
}
