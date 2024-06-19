#include <stdio.h>
#include <stdlib.h>

typedef enum Direction {
  Up,
  Down,
  Left,
  Right,
} Direction;

typedef enum {
  North,
  South,
  East,
  West,
} CardinalDirection;

// Typedef of named structure
typedef struct Rectangle {
  int w;
  int h;
} Rectangle;

// Typedef of anonymous structure
typedef struct {
  int x;
  int y;
} Point;

// Named structure declaration
struct Shape {
  Point *origin;
  Rectangle *r;
};

void f(enum Direction dir, Direction dir2) {
  printf("Direction: %d %d\n", dir, dir2);
}

void pass_as_ref(Point *pt) {
  printf("pass_as_ref: Point: %d %d\n", pt->x, pt->y);
  pt->x = 123;
  pt->y = 456;
  printf("pass_as_ref: Point: %d %d\n", pt->x, pt->y);
}

void pass_as_ref_int(int *x) {
  printf("pass_as_ref_int: %d\n", *x);
  *x = 42;
  printf("pass_as_ref_int: %d\n", *x);
}

// Show that we can have a struct pointer
struct Point *pts;

void main() {
  Direction up = Up;
  const enum Direction down = Down;
  Direction left = Left, right = Right;

  // Rectangle can be with and without the struct keyword
  struct Rectangle *r1 = malloc(sizeof(struct Rectangle));
  Rectangle *r2 = malloc(sizeof(Rectangle));
  Point *pt = malloc(sizeof(Point));
  struct Shape *shape = malloc(sizeof(struct Shape));
  struct Shape **shapes = (struct Shape **) malloc(3 * sizeof(struct Shape));
  int *p = malloc(2 * sizeof(int));
  int *arr = malloc(24);

  int i;

  // Initialize shapes array
  for (i = 0; i < 3; i++) {
    shapes[i] = malloc(sizeof(struct Shape));
    shapes[i]->origin = malloc(sizeof(Point));
    shapes[i]->r = malloc(sizeof(Rectangle));
  }

  // Assign values
  for (i = 0; i < 3; i++) {
    shapes[i]->origin->x = i;
    shapes[i]->origin->y = i * i;
    shapes[i]->r->w = i;
    shapes[i]->r->h = i * i;
  }

  // Print values
  for (i = 0; i < 3; i++) {
    printf("Shape %d: %d %d %d %d\n", i, shapes[i]->origin->x, shapes[i]->origin->y, shapes[i]->r->w, shapes[i]->r->h);
  }

  pt->x = 5;
  pt->y = 6;

  r1->w = 1;
  r1->h = 2;

  shape->origin = pt;
  shape->r = r1;

  printf("Rectangle 1: %d %d\n", r1->w, r1->h);
  printf("Point: %d %d\n", pt->x, pt->y);
  printf("Shape: %d %d %d %d\n", shape->origin->x, shape->origin->y, shape->r->w, shape->r->h);

  // Test passing as reference a struct ptr
  pass_as_ref(pt);
  printf("Point: %d %d\n", pt->x, pt->y);

  // Test passing as reference an integer from a struct
  pass_as_ref_int(&pt->x);
  printf("Point: %d %d\n", pt->x, pt->y);

  // Test passing as reference an integer from an array
  arr[12] = 12;
  printf("arr[12]: %d\n", arr[12]);
  pass_as_ref_int(&arr[12]);
  printf("arr[12]: %d\n", arr[12]);

  exit(0);
}
