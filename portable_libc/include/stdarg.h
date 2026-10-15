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

#define _VA_LIST_TYPE va_list
#define _VA_PASS(ap) (ap)
#define _VA_ARG(ap, type) va_arg(ap, type)

#elif defined(__aarch64__)

/* Taken from TCC 0.9.27's include/stdarg.h. The aarch64 backend implements
   __va_start and __va_arg itself (arm64-gen.c's gen_va_start/gen_va_arg),
   writing this exact layout, so the struct must not be changed. */

typedef struct {
    void *__stack;
    void *__gr_top;
    void *__vr_top;
    int   __gr_offs;
    int   __vr_offs;
} va_list;

#define va_start(ap, last) __va_start(ap, last)
#define va_arg(ap, type) __va_arg(ap, type)
#define va_end(ap)
#define va_copy(dest, src) ((dest) = (src))

/* TCC's aarch64 backend doesn't support struct arguments (arm64_pcs asserts)
   and va_list is a struct here, so functions that forward a va_list take a
   pointer to it. The macros below let the same code work on both targets. */
#define _VA_LIST_TYPE va_list *
#define _VA_PASS(ap) (&(ap))
#define _VA_ARG(ap, type) __va_arg(*(ap), type)

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

/* On targets where va_list is a plain value, it is passed around as is. */
#define _VA_LIST_TYPE va_list
#define _VA_PASS(ap) (ap)
#define _VA_ARG(ap, type) va_arg(ap, type)

#else

#error "Unsupported architecture or compiler for stdarg.h"

#endif

#endif
