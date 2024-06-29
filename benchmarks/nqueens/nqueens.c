
// Define the number of queens
// #define queens 4

int nqueens(int i, int diag1, int diag2, int cols, int col) {
    int free;
    int s1, s2;

    free = diag1 & diag2 & cols;

    if (col > free)
        return 0;

    if (i == 1)
        return 1;

    while (!(col & free)) col = col << 1;

    return nqueens(i - 1, ((diag1 - col) << 1) + 1, (diag2 - col) >> 1, cols - col, 1) +
           nqueens(i, diag1, diag2, cols, col << 1);
}

int main() {
    int n = queens;
    printf("%d-queens problem has %d solutions\n", n, nqueens(n, -1, -1, (1 << n) - 1, 1));
    return 0;
}
