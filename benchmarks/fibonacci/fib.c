// Fibonacci sequence calculation
// #define N 30

int fib(int n) {
    if (n <= 1) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

void main() {
    int result = fib(N);
}
