// File copy implementation
// #define SRC_FILE "src.txt"
// #define DEST_FILE "dest.txt"

void main() {
    char ch;
    int *src;
    int *dest;
    src = fopen(SRC_FILE, 0);
    if (!src) {
        return;
    }

    dest = fopen(DEST_FILE, 1);
    if (!dest) {
        fclose(src);
        return;
    }

    while ((ch = fgetc(src)) != -1) {
        fputc(ch, dest);
    }

    fclose(src);
    fclose(dest);
}


