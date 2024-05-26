void test() {
  while(1)
    return 1;
}

int main() {
  int x = 1;
  test();
  while(0);

  for(; x == 0; x = 1);

  if (0);
}
