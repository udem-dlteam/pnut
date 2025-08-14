#include <sys/mman.h>

#ifdef ADD_LIBC_STUB

int mprotect(void *addr, size_t len, int prot) {
  pnut_abort("mprotect not implemented");
}

#endif
