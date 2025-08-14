#ifndef _SYS_MMAP_H
#define _SYS_MMAP_H

#define PROT_NONE 0
#define PROT_READ 1
#define PROT_WRITE 2
#define PROT_EXEC 4

int mprotect(void *addr, size_t len, int prot);

#endif
