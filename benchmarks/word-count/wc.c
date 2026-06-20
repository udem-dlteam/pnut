// Word count implementation
// #define INPUT_FILE "input.txt"

void main() {
    int *file = fopen(INPUT_FILE, 0);
    int lines = 0, words = 0, chars = 0;
    int in_word = 0;
    char ch;

    if (!file) {
        return;
    }
    while ((ch = fgetc(file)) != -1) {
        chars++;
        if (ch == '\n') lines++;
        if (ch == ' ' || ch == '\n' || ch == '\t') {
            in_word = 0;
        } else if (in_word == 0) {
            in_word = 1;
            words++;
        }
    }
    fclose(file);
}
