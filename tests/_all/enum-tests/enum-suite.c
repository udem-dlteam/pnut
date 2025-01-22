void putstring(char *s) {
  while (*s) {
    putchar(*s);
    s = s + 1;
  }
}

enum Color {
  RED,
  GREEN,
  BLUE
};

enum Weekday {
  MONDAY = 1,
  TUESDAY,
  WEDNESDAY = 5,
  THURSDAY,
  FRIDAY
};

enum Direction {
  NORTH,
  EAST,
  SOUTH,
  WEST
};

enum Boolean {
  FALSE,
  TRUE
};

#define FIRST_VALUE   0
#define SECOND_VALUE  1000000
#define THIRD_VALUE   2000000
#define FORTH_VALUE   FIRST_VALUE + SECOND_VALUE + THIRD_VALUE

enum LargeEnum {
  FIRST = FIRST_VALUE,
  SECOND = SECOND_VALUE,
  THIRD = THIRD_VALUE,
  FORTH = FORTH_VALUE,
};

int main(){

  int c = RED;
  int day = MONDAY;
  int dir = SOUTH;
  int flag = TRUE;
  int value = THIRD;

  putstring("Color: ");
  if(c == 0){
    putstring("RED\n");
  } else{
    putstring("FAIL\n");
    putstring("Color: ");
    putchar(c + 48);
    putchar(10);
  }

  if (day == MONDAY){
    putstring("Day: ");
    putstring("Garfield hates Mondays\n");
  }

  putstring("Direction: ");
  if(dir == NORTH) {
    putstring("NORTH");
  } else if(dir == EAST) {
    putstring("EAST");
  } else if(dir == SOUTH) {
    putstring("SOUTH");
  } else {
    putstring("WEST");
  }
  putchar(10);

  if (flag) {
    putstring("Boolean: TRUE\n");
  } else {
    putstring("Boolean: FALSE\n");
  }

  putstring("Large Enum: ");
  if (value == FIRST) {
    putstring("FIRST");
  } else if (value == SECOND) {
    putstring("SECOND");
  } else {
    putstring("THIRD");
  }
  putchar(10);

  return 0;
}