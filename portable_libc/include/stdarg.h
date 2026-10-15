#ifndef _STDARG_H
#define _STDARG_H

#if defined(__x86_64__) && defined(__TINYC__)

/* Taken from TCC 0.9.27's include/stdarg.h, keeping only the Linux branch:
   TCC also has one for Windows and for other CPUs, which nothing here uses.
   This must stay in sync with TCC's lib/libtcc1.c and lib/va_list.c, which
   repeat it, and with what the x86-64 backend stores in the frame. */

typedef struct {
    unsigned int gp_offset;
    unsigned int fp_offset;
    union {
        unsigned int overflow_offset;
        char *overflow_arg_area;
    };
    char *reg_save_area;
} __va_list_struct;

typedef __va_list_struct va_list[1];

void __va_start(__va_list_struct *ap, void *fp);
void *__va_arg(__va_list_struct *ap, int arg_type, int size, int align);

#define va_start(ap, last) __va_start(ap, __builtin_frame_address(0))
#define va_arg(ap, type)                                                \
    (*(type *)(__va_arg(ap, __builtin_va_arg_types(type), sizeof(type), __alignof__(type))))
#define va_copy(dest, src) (*(dest) = *(src))
#define va_end(ap)

#elif defined(__i386__) || defined(PNUT_CC)

// On 32-bit x86 every argument is passed on the stack, so a pointer into the
// stack is enough to iterate over the variadic arguments.
// Pnut uses the same calling convention for x86-64, so this also works there:
// __i386__ covers TCC and pnut compiling for i386, and PNUT_CC covers pnut
// compiling for x86-64, where it cannot use the branch above. PNUT_CC is not
// predefined by pnut, so it has to come from the command line: it is passed
// when pnut compiles TCC, but not when TCC compiles the libc, which is why
// __i386__ is needed for the 32-bit case.

typedef char *va_list;

#define va_ptr_size      (sizeof(void *))
#define va_word_align(x) (((x)+va_ptr_size-1)&~(va_ptr_size-1))

#define va_start(ap,last) ap = ((char *)&(last)) + va_word_align(sizeof(last))
#define va_arg(ap,type) (ap += va_word_align(sizeof(type)), *(type *)(ap - va_word_align(sizeof(type))))
#define va_end(ap)
#define va_copy(d,s) ((d) = (s))

#else

#error "Unsupported architecture or compiler for stdarg.h"

#endif

#endif
