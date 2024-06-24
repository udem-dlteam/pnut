struct Point {
	int x;
	int y;
};

int test_basic_struct() {
	struct Point p;
	p.x = 10;
	p.y = 20;
	
	if (p.x == 10 && p.y == 20) {
		return 0; // success
	} else {
		return 1; // failure
	}
}

struct Rectangle {
	int width;
	int height;
};

int test_struct_initialization() {
	struct Rectangle r;
	r.width = 30;
	r.height = 40;
	
	if (r.width == 30 && r.height == 40) {
		return 0; // success
	} else {
		return 1; // failure
	}
}

struct Circle {
	int radius;
};

int calculateArea(struct Circle c) { // we assume pi = 3
	int area = 3 * c.radius * c.radius;
	return area;
}

int test_structs_and_functions() {
	struct Circle c;
	c.radius = 5;
	
	int area = calculateArea(c);
	
	if (area == 75) {
		return 0; // success
	} else {
		return 1; // failure
	}
}

struct Square {
	struct Point top_left;
	struct Point bottom_right;
};

int test_nested_structs() {

	struct Square s;
	s.top_left.x = 0;
	s.top_left.y = 10;
	s.bottom_right.x = 10;
	s.bottom_right.y = 0;
	
	if (s.top_left.x == 0 && s.top_left.y == 10 && s.bottom_right.x == 10 && s.bottom_right.y == 0) {
		return 0;
	} else {
		return 1;
	}
}

int test_array_of_structs() {

	struct Point* points = (struct Point*)malloc(3 * sizeof(struct Point));
	if (points == 0) {
			return -1; // failure due to memory allocation error
	}
	points[0].x = 1;
	points[0].y = 2;
	points[1].x = 3;
	points[1].y = 4;
	points[2].x = 5;
	points[2].y = 6;
	
	if (points[0].x == 1 && points[0].y == 2 &&
			points[1].x == 3 && points[1].y == 4 &&
			points[2].x == 5 && points[2].y == 6) {
		return 0; 
	} else {
		return 1;
	}
}

int main() {
	if(test_basic_struct() == 0) {
		putchar('O');
		putchar(10);
	} else {
		putchar('X');
		putchar(10);
	}
	if(test_struct_initialization() == 0) {
		putchar('O');
		putchar(10);
	} else {
		putchar('X');
		putchar(10);
	}
	if(test_structs_and_functions() == 0) {
		putchar('O');
		putchar(10);
	} else {
		putchar('X');
		putchar(10);
	}
	if(test_nested_structs() == 0) {
		putchar('O');
		putchar(10);
	} else {
		putchar('X');
		putchar(10);
	}
	if(test_array_of_structs() == 0) {
		putchar('O');
		putchar(10);
	} else {
		putchar('X');
		putchar(10);
	}

	return 0;
}
