// #define ARR_SIZE 100

void multiply(int n, int **mat1, int **mat2, int **res) {
  int i, j, k;
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            res[i][j] = 0;
            for (k = 0; k < n; k++) {
                res[i][j] += mat1[i][k] * mat2[k][j];
            }
        }
    }
}

void main() {
    int n = ARR_SIZE;
    int i, j;

    // Allocate memory for the matrices
    int **mat1 = malloc(n * sizeof(int *));
    int **mat2 = malloc(n * sizeof(int *));
    int **res = malloc(n * sizeof(int *));

    for (i = 0; i < n; i++) {
        mat1[i] = malloc(n * sizeof(int));
        mat2[i] = malloc(n * sizeof(int));
        res[i] = malloc(n * sizeof(int));
    }

    // Initialize the matrices with random values
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            mat1[i][j] = rand() % 100;
            mat2[i][j] = rand() % 100;
        }
    }

    // Perform matrix multiplication
    multiply(n, mat1, mat2, res);

    // Free the allocated memory
    for (i = 0; i < n; i++) {
        free(mat1[i]);
        free(mat2[i]);
        free(res[i]);
    }
    free(mat1);
    free(mat2);
    free(res);
}
