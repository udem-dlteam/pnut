// Test initializer lists for global and local variables

int global_arr[4] = {1, 2, 3, 4};
int global_partial[4] = {1, 2}; // Rest should be 0
int global_single = {42};

void test_global_initializers() {
  int i;
  
  putstring("Testing global initializer lists:\n");
  
  // Test full array initialization
  for(i = 0; i < 4; i++) {
    putint(global_arr[i]);
    putchar('\n');
  }
  
  // Test partial array initialization
  putstring("Partial array init:\n");
  for(i = 0; i < 4; i++) {
    putint(global_partial[i]); 
    putchar('\n');
  }
  
  // Test single value initialization
  putstring("Single value:\n");
  putint(global_single);
  putchar('\n');
}

void test_local_initializers() {
  int local_arr[3] = {10, 20, 30};
  int local_partial[3] = {100, 200};
  int local_single = {500};
  int i;

  putstring("Testing local initializer lists:\n");
  
  // Test full array initialization
  for(i = 0; i < 3; i++) {
    putint(local_arr[i]);
    putchar('\n');
  }
  
  // Test partial array initialization
  putstring("Partial array init:\n");
  for(i = 0; i < 3; i++) {
    putint(local_partial[i]);
    putchar('\n');
  }
  
  // Test single value initialization
  putstring("Single value:\n");
  putint(local_single);
  putchar('\n');
}

int main() {
  test_global_initializers();
  test_local_initializers();
  
  putchar(':');
  putchar(')');
  putchar(10);
  
  return 0;
}
