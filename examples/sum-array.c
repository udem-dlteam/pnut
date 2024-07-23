/*
 * sum-array.c: Compute the sum of an array of integers of size 10000
 *
 * Usage: ./sum-array.sh
 */

int sum_array(int* n, int size) {
  int i = 0;
  int sum = 0;

  for (; i < size; i++) {
    sum += n[i];
  }

  return sum;
}

void main() {
  int* n = malloc(10000);
  int i = 0;
  int sum = 0;

  for (; i < 10000; i++) {
    n[i] = i;
  }

  for (i = 0; i < 10000; i++) {
    sum += n[i];
  }
}
