/*
 * chmod.c: Change the file mode (permissions) of the specified files
 *
 * Usage: ./chmod.sh [--verbose] MODE FILE...
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef FLAT_INCLUDES
#include <stat.h>
#else
#include <sys/stat.h>
#endif

#ifndef ENTRY_POINT
#define ENTRY_POINT main
#endif

int ENTRY_POINT(int argc, char **argv) {
  int verbose = 0;
  int permissions = -1;

  while (argc > 1) {
    if (strcmp(argv[1], "--verbose") == 0) {
      verbose = 1;
      argv += 1;
      argc -= 1;
    } else if (strcmp(argv[1], "--help") == 0) {
      printf("Usage: chmod [--verbose] MODE FILE...\n");
      exit(0);
    } else if ('0' <= argv[1][0] && argv[1][0] <= '7') {
      if (permissions != -1) { break; } // only one mode allowed, this must be a file starting with a digit
      permissions = 0;
      int i;
      for (i = 0; argv[1][i] != '\0' && i < 4; i++) {
        permissions = (permissions << 3) | (argv[1][i] - '0');
      }
      argv += 1;
      argc -= 1;
    } else {
      break;
    }
  }

  if (permissions == -1) {
    fprintf(stderr, "chmod: missing mode\n");
    exit(1);
  }

  if (argc < 2) {
    fprintf(stderr, "chmod: missing file\n");
    exit(1);
  }

  while (argc > 1) {
    if (access(argv[1], F_OK) == -1) {
      fprintf(stderr, "chmod: cannot access '%s'\n", argv[1]);
      exit(1);
    }

    if (verbose) {
      printf("chmod: changing mode of '%s' to %o\n", argv[1], permissions);
    }

    if (chmod(argv[1], permissions) == -1) {
      fprintf(stderr, "chmod: cannot change mode of '%s'\n", argv[1]);
      exit(1);
    }

    argv += 1;
    argc -= 1;
  }

  return 0;
}
