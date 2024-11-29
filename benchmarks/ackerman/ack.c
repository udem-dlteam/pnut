// Ackermann function calculation
// #define M 3
// #define N 6

int ackermann(int m, int n) {
    if (m == 0) {
        return n + 1;
    } else if (m > 0 && n == 0) {
        return ackermann(m - 1, 1);
    } else {
        return ackermann(m - 1, ackermann(m, n - 1));
    }
}

void main() {
    int result = ackermann(M, N);
}
