//############################### LARGE INTEGERS ###############################
//
// POSIX shell requires shells to signed integers at least 32 bits wide, but we
// want to be able to read 64 bit unsigned integers. To do this, we emulate
// larger integers by splitting them into smaller 24-bit chunks.
//
// 24-bit integers are used because the largest base read by accum_digit is 16 (2^4),
// and so we must be able to detect overflow without overflowing the 32-bit int.
// 24 is the largest multiple of 8 that is less than 32 that can accommodate this.
//
// The large ints are stored in little-endian order, large_int[0] stores the
// lowest 24 bits, large_int[1] stores the next 24 bits, and so on.
#define LARGE_INT_MAX 0xFFFFFF
#define LARGE_INT_LOG 24
#define LARGE_INT_SIZE 4

int large_int[LARGE_INT_SIZE];
// Index of the largest component. This is used to avoid iterating over empty components.
int large_int_i = 0;

// Not using LARGE_INT_LOG to write generic code that works with any size
// because pnut-sh doesn't perform constant folding and we want to avoid the
// extra computation.
#define LOW_32_BITS(base)   ((base)[0]       | (((base)[1] & 0xFF  ) << 24)) // low 32 bits : 24 bits from #0, 8  bits from #1
#define HIGH_32_BITS(base)  ((base)[1] >> 8  | (((base)[2] & 0xFFFF) << 16)) // high 32 bits: 16 bits from #1, 16 bits from #2
#define OVERFLOW_BITS(base) ((base)[2] >> 16 | (((base)[3] & 0xFFFF) << 8))  // overflow bits: 8 bits from #2, 16 bits from #3

void large_int_init() {
  int i = 0;
  large_int_i = 0; // Reset index
  for (; i < LARGE_INT_SIZE; i += 1) large_int[i] = 0;
}

#ifdef SAFE_MODE
void large_int_printf() {
  int i = LARGE_INT_SIZE - 1;
  printf("##### large int #####\n");
  printf("# large_int_i = %d\n", large_int_i);
  printf("# dec: %ld\n", large_int[0] + ((long) large_int[1] << LARGE_INT_LOG));

  printf("# hex:");
  while (i >= 0) {
    printf(" %x", large_int[i]);
    i -= 1;
  }
  printf("\n");
}
#endif

// Add value to the i^th component of the large integer. If the value overflows
// the component, the carry is propagated to the next component until the carry
// is 0 or we reach the last component.
// This function assumes that value is less than 2^24
void large_int_add(int i, int value) {
#ifdef SAFE_MODE
  if (value > LARGE_INT_MAX) { // Bug!?
    printf("large_int_add: value %d is greater than %d\n", value, LARGE_INT_MAX);
    exit(1);
  }
  if (i >= LARGE_INT_SIZE) { // Overflow!
    printf("large_int_add: number exceed %d-bit precision\n", LARGE_INT_SIZE * LARGE_INT_LOG);
    exit(1);
  }
#endif

  while (value != 0) {
    if (i >= LARGE_INT_SIZE) fatal_error("large_int_add: index is out of bounds");

    large_int[i] += value;
    value = 0; // No carry by default
    if (large_int[i] > LARGE_INT_MAX) {
      value = large_int[i] >> LARGE_INT_LOG; // Carry
      large_int[i] &= LARGE_INT_MAX;
      i += 1;
    }
  }

  large_int_i = large_int_i > i ? large_int_i : i;
}

// Multiply by positive integer value. Value must be less than 2^8.
void large_int_mul(int value) {
#ifdef SAFE_MODE
  if (value > 255 || value < 0) {
    printf("large_int_mul: value %d must be between 1 and 255\n", value);
    exit(1);
  }
#endif

  // Note that we loop from the largest component first to avoid multiplying the
  // carry. This saves us from having a separate buffer for the carry, but in
  // some pathological cases, it we may end up traversing the components N
  // times, for a complexity of O(N^2)
  int i = large_int_i;
  int carry = 0;

  while (i >= 0) {
    large_int[i] *= value;
    if (large_int[i] > LARGE_INT_MAX) {
      carry = large_int[i] >> LARGE_INT_LOG;
      large_int[i] &= LARGE_INT_MAX;
      large_int_add(i + 1, carry);
    }

    i -= 1;
  }
}

int large_int_to_int32() {
  int result = LOW_32_BITS(large_int);

#ifdef SAFE_MODE
  // Make sure the integer doesn't overflow
  if (HIGH_32_BITS(large_int) != 0 || OVERFLOW_BITS(large_int) != 0) {
    large_int_printf();
    fatal_error("large_int_to_int32: integer overflow");
  }
#endif
  return result;
}

// Pack the large int components in an object.
// Because most integers are small and we want to save memory, we only store the
// large int object ("large ints") if it is larger than 31 bits. Otherwise, we
// store it as a regular integer. The sign bit is used to distinguish between
// large ints (positive) and regular ints (negative).
int large_int_to_obj() {
  int result;

  // If the integer is larger than 31 bits, it must be stored as a "large int"
  if (large_int[1] >= 127 || large_int_i > 1) {
    result = alloc_obj(4);
    heap[result]   = large_int[0];
    heap[result+1] = large_int[1];
    heap[result+2] = large_int[2];
    heap[result+3] = large_int[3];

  } else
  // Otherwise, it may be stored as a regular integer.
  // To distinguish between large ints and regular ints, we use the sign bit.
  {
    result = -LOW_32_BITS(large_int);
  }

  return result;
}
