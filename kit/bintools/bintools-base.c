/*
 * bintools.c: A bootstrap utility for various binary tools.
 *
 * Supported commands:
 *  cp: Copy files
 *  cat: Concatenate files
 *  mkdir: Create directories
 *  sha256sum: Compute SHA256 checksums
 *  simple-patch: Apply simple patches
 *  ungz: Uncompress .gz files
 *  untar: Extract .tar files
 */

#include <string.h>

// Add includes for all subcommands so process-includes.sh puts them at the top
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <setjmp.h>

#ifdef FLAT_INCLUDES
#include <stat.h>
#else
#include <sys/stat.h>
#endif

#define ENTRY_POINT cat_main
#include "cat.c"
#undef ENTRY_POINT
#define ENTRY_POINT chmod_main
#include "chmod.c"
#undef ENTRY_POINT
#define ENTRY_POINT cp_main
#include "cp.c"
#undef ENTRY_POINT
#define ENTRY_POINT mkdir_main
#include "mkdir.c"
#undef ENTRY_POINT
#define ENTRY_POINT sha256sum_main
#include "sha256sum.c"
#undef ENTRY_POINT
#define ENTRY_POINT simple_patch_main
#include "simple-patch.c"
#undef ENTRY_POINT
#define ENTRY_POINT ungz_main
#include "ungz.c"
#undef ENTRY_POINT
#define ENTRY_POINT untar_main
#include "untar.c"
#undef ENTRY_POINT

// Function to invoke the appropriate subcommand based on the command name.
// Takes the command name and the arguments passed to it. argc and argv include
// the command name as the first argument.
// Returns the exit code of the invoked command.
int invoke_subcommand(char* name, int argc, char **argv) {
  if (strcmp(name, "bintools") == 0) {
    // If no command is provided, show usage
    if (argc < 2) {
      // No command provided, show usage
      static const char *usage = "Usage: bintools <command> [<args>]\n"
                          "Commands:\n"
                          "  cp <source> <destination>   Copy files\n"
                          "  cat <files>                 Concatenate files\n"
                          "  mkdir <directory>           Create directories\n"
                          "  sha256sum <files>           Compute SHA256 checksums\n"
                          "  simple-patch <patch> <file> Apply simple patches\n"
                          "  ungz <file.gz>              Uncompress .gz files\n"
                          "  untar <file.tar>            Extract .tar files\n";
      printf("%s\n", usage);
    } else {
      // Shift arguments to pass to subcommand
      return invoke_subcommand(argv[1], argc - 1, argv + 1);
    }
  } else if (strcmp(name, "cat") == 0) {
    return cat_main(argc, argv);
  } else if (strcmp(name, "chmod") == 0) {
    return chmod_main(argc, argv);
  } else if (strcmp(name, "cp") == 0) {
    return cp_main(argc, argv);
  } else if (strcmp(name, "mkdir") == 0) {
    return mkdir_main(argc, argv);
  } else if (strcmp(name, "sha256sum") == 0) {
    return sha256sum_main(argc, argv);
  } else if (strcmp(name, "simple-patch") == 0) {
    return simple_patch_main(argc, argv);
  } else if (strcmp(name, "ungz") == 0) {
    return ungz_main(argc, argv);
  } else if (strcmp(name, "untar") == 0) {
    return untar_main(argc, argv);
  } else {
    printf("bintools: unknown command '%s'\n", name);
    return 1;
  }
}

// bintools may be called with ./path/to/bintools or /usr/bin/bintools
// We want to extract the actual command name (bintools) from the path.
char *strip_path(char *path) {
  char *base = strrchr(path, '/');
  return base ? base + 1 : path;
}

int main(int argc, char **argv) {
  // The tool looks at argv[0] to determine which sub-command to pass control to.
  // If no command is provided, it shows usage information.
  return invoke_subcommand(strip_path(argv[0]), argc, argv);
}
