// Tak function calculation
// #define X 3
// #define Y 2
// #define Z 1

int tak(int x, int y, int z) {
    if (y >= x) {
        return z;
    }
    return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y));
}

void main() {
    int result = tak(X, Y, Z);
}
