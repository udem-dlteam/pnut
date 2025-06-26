#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#define ASSERT(x, msg) \
  if (!(x)) { \
    fprintf(stderr, msg "\n"); \
    exit(1); \
  }

char *read_file(char *filename) {
  // Open file for reading
  int fd = open(filename, O_RDONLY);
  ASSERT(fd >= 0, "Error opening file");

  // Allocate buffer for file content
  int size = lseek(fd, 0, SEEK_END); // Get file size
  char *content = malloc(size + 1); // Allocate memory for file content and \0
  ASSERT(content != NULL, "Error allocating memory for file content");

  // Read file content
  lseek(fd, 0, SEEK_SET); // Reset file pointer to the beginning
  ASSERT(read(fd, content, size) == size, "Error reading file content");
  content[size] = '\0'; // Null-terminate the string

  // Close file descriptor
  close(fd);
  return content;
}

// Usage: simple-patch file before-patch after-patch
int main(int argc, char **argv) {
  if (argc != 4) {
    fprintf(stderr, "Usage: %s file before-patch after-patch\n", argv[0]);
    return 1;
  }

  char *file_content = read_file(argv[1]);
  int file_len = strlen(file_content);

  char *patch_before = read_file(argv[2]);
  int before_patch_len = strlen(patch_before);

  char *patch_after = read_file(argv[3]);
  int after_patch_len = strlen(patch_after);

  // Find the before patch string in the file content
  char *pos = strstr(file_content, patch_before);
  ASSERT(pos != NULL, "Error finding patch string");
  int pos_index = pos - file_content;

  int fd_output = open(argv[1], O_WRONLY | O_TRUNC); // Open the output file, truncating it
  ASSERT(fd_output >= 0, "Error opening after patch file");

  // Create a new file content with the after patch
  write(fd_output, file_content, pos_index); // Write before patch
  write(fd_output, patch_after, after_patch_len); // Write after patch
  write(fd_output, file_content + pos_index + before_patch_len, file_len - pos_index - before_patch_len); // Write the rest of the file

  // Clean up
  free(file_content);
  free(patch_before);
  free(patch_after);
  close(fd_output);

  return 0;
}
