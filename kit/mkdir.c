/*
 * mkdir.c: Create directories
 *
 * Usage: ./mkdir.sh [-p|--parents] <directory>...
 *
 * Known issues: Blows up if the directory already exists.
 *
 * Options:
 *   -p, --parents   Create parent directories as needed
 *
 */

#ifndef ENTRY_POINT
#define ENTRY_POINT main
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

void create_dir_recursive(int create_parent_dirs, char *pathname, int mode) {
	int res;

	// Try creating the directory, if it fails, we'll try to create the parent
  // directories and then retry. This is not guaranteed to succeed since it
  // could be failing for other reasons.
	res = mkdir(pathname, mode);

	if(res != 0 && create_parent_dirs) {
    // Find the last slash in the pathname, then temporarily remove the last part
    // of the pathname while creating the parent directories.
    char *parent_dirs_path = strrchr(pathname, '/');
    parent_dirs_path[0] = '\0';
    create_dir_recursive(create_parent_dirs, pathname, mode);

    // Restore the original pathname and try creating the directory again.
    parent_dirs_path[0] = '/';
    res = mkdir(pathname, mode);
	}

	if(res != 0) {
		printf("Could not create directory %s\n", pathname);
    exit(1);
	}
}

int ENTRY_POINT(int argc, char **argv) {
	int create_parent_dirs = 0;

  while (argc > 1) {
    if (strcmp(argv[1], "-p") == 0 || strcmp(argv[1], "--parents") == 0) {
			create_parent_dirs = 1;
      argv += 1;
      argc -= 1;
		}
		else {
      // Strip trailing '/' if present
      if(argv[1][strlen(argv[1]) - 1] == '/') {
        argv[1][strlen(argv[1]) - 1] = '\0';
      }
      create_dir_recursive(create_parent_dirs, argv[1], 0755);
      argv += 1;
      argc -= 1;
    }
	}

	return 0;
}
