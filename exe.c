// common part of machine code generators
void generate_exe();

// When placing globals on the stack, it's important that globals don't occupy
// so much space they overflow the stack.
#ifdef USE_STACK_FOR_GLOBALS
// 128KB heap because it's also on the stack
#define RT_HEAP_SIZE 131072
// Allow overriding globals size with -D option. Default is 2MB
#ifndef RT_GLO_SIZE
#define RT_GLO_SIZE 2097152
#endif
#else
// 100MB
#define RT_HEAP_SIZE 104857600
// Allow overriding globals size with -D option. Default is 100MB
#ifndef RT_GLO_SIZE
#define RT_GLO_SIZE 104857600
#endif
#endif

// Maximum code size, used for program size in elf header when using one-pass generator
#ifndef MAX_CODE_SIZE
#define MAX_CODE_SIZE 1000000
#endif

#if defined(ONE_PASS_GENERATOR) && !defined(ONE_PASS_GENERATOR_NO_EARLY_OUTPUT)
#define CODE_SIZE 100000
#else
#define CODE_SIZE 5000000
#endif
int code[CODE_SIZE];
// Index of the next free byte in the code buffer
int code_alloc = 0;
// Total number of bytes emitted
// code_alloc + code_address_base = address of the next instruction.
int code_address_base = 0;
#ifdef PRINT_MEMORY_STATS
// Maximum size of the code buffer, used for debugging
int code_alloc_max = 0;
#endif

// ONE_PASS_GENERATOR option:
//
//   Makes the code generator one-pass.
//
//   If set, the machine code is written to the output at the end of each
//   function definition. This significantly reduces the required size of the
//   code buffer (240k to 15k when compiling pnut-exe), but introduces extra
//   complexity to the code generation since fixups can only be done for code
//   that is still in the buffer. The difficulties come in 2 variants:
//
//     1. Forward jumps to labels that are not yet defined.
//
//     2. Certain constants must be placed at the beginning of the code, but
//         their value is only known at the end of the program.
//
//   Here are some concrete examples of such problems:
//
//     1. Calls to functions that are defined later in the code cannot be done
//         directly.
//
//     2. The initialization of global variables is interspersed with the rest of
//         the code, with forward jumps from the N^th initialization to the N+1^th
//         initialization. This means that at any time, the init_next_lbl label is
//         undefined.
//
//     3. Allocating space for global variables must be done at the beginning of
//         the program where the size of the globals is not known yet. More
//         generally, transfering control from the beginning of the program to the
//         end in 1 jump is not possible. This makes transfering information from
//         the end of the program to the beginning much more difficult.
//
//     4. The ELF header contains the size of the code, which is not known until
//         the end of the program.
//
//   And how they are solved:
//
//     1. The code generator maintains a jump table with all functions of the
//         program. When the function is declared, the code generator adds an
//         entry to the globals to store the address of the function, which will
//         be initialized with the address of the function during program
//         initialization when the address is known. This makes forward function
//         calls more expensive as they go through the jump table. This motivated
//         moving the definition of the built-in functions to the beginning of the
//         program to speed them up.
//
//     2. To be able to output the code after a function definition, all jumps in
//         the code must be resolved. Fortunately, non-call jumps inside a
//         function are all resolved at the end of the function, this leaves
//         init_next_lbl as the only unresolved label. Because function definition
//         is followed by the initialization of its jump table entry, the
//         init_next_lbl label is temporarily resolved, at which point all labels
//         are resolved and the code can be flushed.
//         See init_forward_jump_table for more details.
//
//     3. The size of the global variables is set assumed to be up under a
//         certain hardcoded limit. If the limit is exceeded, the code generator
//         will emit a fatal error at the end.
//
//     4. On certain platforms, the size of the program in the ELF header doesn't
//         need to be equal to the actual size of the code. As long as the
//         declared size is greater than the actual size, the program will run.

#ifdef ONE_PASS_GENERATOR
void reset_code_buffer() {
  code_address_base += code_alloc;
  code_alloc = 0;
}
#endif

void emit_i8(const int a) {
  if (code_alloc >= CODE_SIZE) {
    fatal_error("code buffer overflow");
  }
  code[code_alloc] = (a & 0xff);
  code_alloc += 1;
}

void emit_2_i8(const int a, const int b) {
  emit_i8(a);
  emit_i8(b);
}

void emit_4_i8(const int a, const int b, const int c, const int d) {
  emit_2_i8(a, b);
  emit_2_i8(c, d);
}

void emit_i32_le(const int n) {
  emit_4_i8(n, n >> 8, n >> 16, n >> 24);
}

#if WORD_SIZE == 8
void emit_i64_le(const int n) {
  emit_i32_le(n);
  // Sign extend to 64 bits. Arithmetic shift by 31 gives -1 for negative numbers and 0 for positive numbers.
  emit_i32_le(n >> 31);
}
#endif

#ifdef SUPPORT_64_BIT_LITERALS
void emit_i32_le_large_imm(const int imm_obj) {
  if (!is_large_int(imm_obj)) {
    emit_i32_le(-imm_obj);
  } else {
    // Check that the number doesn't overflow 64 bits
    if (large_int_hi(imm_obj) != 0) fatal_error("emit_i32_le_large_imm: integer overflow");
    emit_i32_le(large_int_lo(imm_obj));
  }
}

#if WORD_SIZE == 8
void emit_i64_le_large_imm(const int imm_obj) {
  if (!is_large_int(imm_obj)) {
    emit_i64_le(-imm_obj);
  } else {
    emit_i32_le(large_int_lo(imm_obj));
    emit_i32_le(large_int_hi(imm_obj));
  }
}
#endif
#endif

char write_buf[1];
void write_i8(const int n) {
  write_buf[0] = (n & 0xff);
  write(output_fd, write_buf, 1);
}

void write_2_i8(const int a, const int b) {
  write_i8(a);
  write_i8(b);
}

void write_4_i8(const int a, const int b, const int c, const int d) {
  write_2_i8(a, b);
  write_2_i8(c, d);
}

void write_i32_le(const int n) {
  write_4_i8(n, n >> 8, n >> 16, n >> 24);
}

// If the main function returns a value
bool main_returns = false;
// Return type of the current function, used to convert returned values
ast current_fun_return_type = 0;

// Environment tracking
#include "env.c"

int grow_fs(const int words) {
  // avoid `return (cgc_fs += words)` because some shells parse assignment ops
  // with lower precedence than assignment.
  cgc_fs += words;
  return cgc_fs;
}

const int reg_X;
const int reg_Y;
const int reg_Z;
const int reg_SP;
const int reg_glo;

void mov_reg_imm(int dst, int imm);             // Move 32 bit immediate to register
#ifdef SUPPORT_64_BIT_LITERALS
void mov_reg_large_imm(int dst, int large_imm); // Move large immediate to register
#endif
void mov_reg_reg(int dst, int src);
void mov_mem8_reg(int base, int offset, int src);
void mov_mem16_reg(int base, int offset, int src);
void mov_mem32_reg(int base, int offset, int src);
void mov_mem64_reg(int base, int offset, int src);
void mov_mem8_reg(int base, int offset, int src);
void mov_reg_mem8(int dst, int base, int offset);
void mov_reg_mem16(int dst, int base, int offset);
void mov_reg_mem32(int dst, int base, int offset);
void mov_reg_mem8_sign_ext(int dst, int base, int offset);
void mov_reg_mem16_sign_ext(int dst, int base, int offset);
void mov_reg_mem32_sign_ext(int dst, int base, int offset);
void mov_reg_mem64(int dst, int base, int offset);

#if WORD_SIZE == 4
#define mov_mem_reg(base, offset, src) mov_mem32_reg(base, offset, src)
#define mov_reg_mem(dst, base, offset) mov_reg_mem32(dst, base, offset)
#elif WORD_SIZE == 8
#define mov_mem_reg(base, offset, src) mov_mem64_reg(base, offset, src)
#define mov_reg_mem(dst, base, offset) mov_reg_mem64(dst, base, offset)
#endif
void load_mem_location(int dst, int base, int offset, int width, bool is_signed);

void add_reg_imm(int dst, int imm);
void add_reg_lbl(int dst, int lbl);
void add_reg_reg(int dst, int src);
void or_reg_reg (int dst, int src);
void and_reg_reg(int dst, int src);
void sub_reg_reg(int dst, int src);
void xor_reg_reg(int dst, int src);
void imul_reg_reg(int dst, int src); // signed multiplication
void idiv_reg_reg(int dst, int src); // signed division
void irem_reg_reg(int dst, int src); // signed remainder
void mul_reg_reg(int dst, int src);  // unsigned multiplication
void div_reg_reg(int dst, int src);  // unsigned division
void rem_reg_reg(int dst, int src);  // unsigned remainder
void s_l_reg_reg(int dst, int src);  // signed/unsigned left shift
void sar_reg_reg(int dst, int src);  // signed right shift
void shr_reg_reg(int dst, int src);  // unsigned right shift
void mov_reg_lbl(int reg, int lbl);
#ifdef SUPPORT_FULL_ARITHMETIC
void extend_reg(int reg, int width, bool is_signed); // sign/zero-extend the low `width` bytes into the whole register
#endif

void push_reg(int src);
void pop_reg (int dst);

void jump(int lbl);
void jump_rel(int offset);
void call(int lbl);
void call_reg(int reg);
void ret();
void debug_interrupt();

// ============================== Stack Operations =============================
//
// The following functions abstract over the stack operations, which are used to
// manage the stack pointer and the frame size (cgc_fs). Use these functions
// instead of directly manipulating the stack pointer and cgc_fs unless needed.

void stack_push(const int reg) {
  push_reg(reg);
  grow_fs(1);
}

void stack_pop(const int reg) {
  pop_reg(reg);
  grow_fs(-1);
}

void stack_grow(const int words) {
  add_reg_imm(reg_SP, -words * WORD_SIZE);
}

void reset_stack_to(const int to_cgc_fs) {
  if (to_cgc_fs != cgc_fs) {
    stack_grow(to_cgc_fs - cgc_fs);
    cgc_fs += (to_cgc_fs - cgc_fs);
  }
}

// Load word-sized value from the stack at index `ix` into register `reg`.
// The index is relative to the current frame size (cgc_fs).
void stack_load(const int reg, const int ix) {
  mov_reg_mem(reg, reg_SP, (cgc_fs - ix) * WORD_SIZE);
}

// Load a value of size `width` and sign `is_signed` from the stack at index
// `ix` into register `reg`.
// The index is relative to the current frame size (cgc_fs).
void stack_dereference(const int reg, const int ix, const int width, const bool is_signed) {
  load_mem_location(reg, reg_SP, (cgc_fs - ix) * WORD_SIZE, width, is_signed);
}

// Push the address of the stack value at index `ix` onto the stack.
// The index is relative to the current frame size (cgc_fs).
// Clobbers reg_Z.
void stack_push_address_of(const int ix) {
  if (cgc_fs != ix) {
    mov_reg_reg(reg_Z, reg_SP);
    add_reg_imm(reg_Z, (cgc_fs - ix) * WORD_SIZE);
    stack_push(reg_Z);
  } else {
    stack_push(reg_SP);
  }
}

// =============================================================================

void load_mem_location(int dst, int base, int offset, int width, bool is_signed) {
  if (is_signed) {
    switch (width) {
      case 1: mov_reg_mem8_sign_ext(dst, base, offset);  break;
      case 2: mov_reg_mem16_sign_ext(dst, base, offset); break;
#if WORD_SIZE == 4
      case 4: mov_reg_mem32(dst, base, offset); break;
#elif WORD_SIZE == 8
      case 4: mov_reg_mem32_sign_ext(dst, base, offset); break; // This instruction is only available in 64-bit mode
      case 8: mov_reg_mem64(dst, base, offset);          break; // no sign extension needed
#endif
      default: fatal_error("load_mem_location: unknown width");
    }
  } else {
    switch (width) {
      case 1: mov_reg_mem8(dst, base, offset);  break;
      case 2: mov_reg_mem16(dst, base, offset); break;
      case 4: mov_reg_mem32(dst, base, offset); break;
#if WORD_SIZE == 8
      case 8: mov_reg_mem64(dst, base, offset); break;
#endif
      default: fatal_error("load_mem_location: unknown width");
    }
  }
}

// Write a value from a register to a memory location
void write_mem_location(int base, int offset, int src, int width) {
  if (width > WORD_SIZE) {
    fatal_error("write_mem_location: width > WORD_SIZE");
  }

  switch (width) {
    case 1: mov_mem8_reg(base, offset, src); break;
    case 2: mov_mem16_reg(base, offset, src); break;
    case 4: mov_mem32_reg(base, offset, src); break;
    case 8: mov_mem64_reg(base, offset, src); break;
    default: fatal_error("write_mem_location: unknown width");
  }
}

#ifdef SUPPORT_STRUCT_UNION

void copy_obj(int dst_base, int dst_offset, int src_base, int src_offset, int width) {
  int i;
  // move the words
  for (i = 0; i < width / WORD_SIZE; i += 1) {
    mov_reg_mem(reg_Z, src_base, src_offset + i * WORD_SIZE);
    mov_mem_reg(dst_base, dst_offset + i * WORD_SIZE, reg_Z);
  }

  // then move the remaining bytes
  for (i = width - width % WORD_SIZE; i < width; i += 1) {
    mov_reg_mem8(reg_Z, src_base, src_offset + i);
    mov_mem8_reg(dst_base, dst_offset + i, reg_Z);
  }
}

#endif // SUPPORT_STRUCT_UNION

#ifdef SUPPORT_COMPLEX_INITIALIZER

// Initialize a memory location with a value
void initialize_memory(int val, int base, int offset, int width) {
  int i;
  mov_reg_imm(reg_Z, val);
  for (i = 0; i < width / WORD_SIZE; i += 1) {
    mov_mem_reg(base, offset + i * WORD_SIZE, reg_Z);
  }
  for (i = width - width % WORD_SIZE; i < width; i += 1) {
    mov_mem8_reg(base, offset + i, reg_Z);
  }
}

#endif // SUPPORT_COMPLEX_INITIALIZER

int is_power_of_2(int n) {
  return n != 0 && (n & (n - 1)) == 0;
}

int power_of_2_log(int n) {
  int i = 0;
  while (n > 1) {
    n /= 2;
    i += 1;
  }
  return i;
}

void mul_for_pointer_arith(int reg, int width) {
  int other_reg = TERNARY(reg == reg_Y, reg_X, reg_Y);
  if (width == 1) return;

  if (is_power_of_2(width)) {
    while (width > 1) {
      width /= 2;
      add_reg_reg(reg, reg);
    }
  } else {
    stack_push(other_reg);
    mov_reg_imm(other_reg, width);
    mul_reg_reg(reg, other_reg);
    stack_pop(other_reg);
  }
}

void div_for_pointer_arith(int reg, int width) {
  int reg_start = reg;

  if (width == 1) return;

  if (is_power_of_2(width)) {
    // sar_reg_reg does not work with reg_Y, so we need to shift the value to reg_X
    if (reg_start != reg_X) {
      stack_push(reg_X);                // Save reg_X
      mov_reg_reg(reg_X, reg_start);  // Move the value to reg_X
      reg = reg_X;
    } else {
      stack_push(reg_Y);                // Otherwise we still clobber reg_Y so save it
    }

    // At this point, reg is always reg_X, and reg_Y is free
    mov_reg_imm(reg_Y, power_of_2_log(width));
    sar_reg_reg(reg_X, reg_Y);

    // Now reg_X contains the result, and we move it back in reg_start if needed
    if (reg_start != reg_X) {
      mov_reg_reg(reg_start, reg_X);
      stack_pop(reg_X);
    } else {
      stack_pop(reg_Y); // Otherwise, we still need to restore reg_Y
    }
  } else {
    // div_reg_reg only works with reg_X on certain architectures, so we need to save it
    if (reg_start != reg_X) {
      stack_push(reg_X);
      reg = reg_X;
    } else {
      stack_push(reg_Y);
    }

    mov_reg_imm(reg_Y, width);
    div_reg_reg(reg_X, reg_Y);

    if (reg_start != reg_X) {
      mov_reg_reg(reg_start, reg_X);
      stack_pop(reg_X);
    } else {
      stack_pop(reg_Y);
    }
  }
}

const int EQ; // x == y
const int NE; // x != y
const int LT; // x < y
const int LT_U; // x < y  (unsigned)
const int GE; // x >= y
const int GE_U; // x >= y (unsigned)
const int LE; // x <= y
const int LE_U; // x <= y (unsigned)
const int GT; // x > y
const int GT_U; // x > y  (unsigned)

void jump_cond_reg_reg(int cond, int lbl, int reg1, int reg2);

void os_exit();
void os_allocate_memory(int size);
void os_read();
void os_write();
void os_open();
void os_close();
void os_seek();
void os_unlink();
void os_mkdir();
void os_chmod();
void os_access();

void rt_putchar();
void rt_debug(char* msg);
void rt_crash(char* msg);

void setup_proc_args(int global_vars_size);

#define cgc int

// Labels to initialize global variables
int setup_lbl;
int init_start_lbl;
int init_next_lbl;
int main_lbl = 0;
int exit_lbl;

int word_size_align(const int n) {
  return (n + WORD_SIZE - 1) / WORD_SIZE * WORD_SIZE;
}

int align_to(const int mul, const int n) {
  return (n + mul - 1) / mul * mul;
}

void rt_debug(char* msg);
void rt_crash(char* msg);

// Label definition

enum {
  GENERIC_LABEL,
  GOTO_LABEL,
};

// Labels are objects allocated on the heap. Every label has a kind and an
// address: the address is negative once the label is defined, and otherwise
// heads the list of code locations to patch (see use_label).
// Generic labels additionally carry the name and source location used in error
// messages, but only when they are allocated with room for them (see
// alloc_label). Goto labels instead use offset 2 for the frame size.
#define label_kind(lbl) heap[lbl]
#define label_addr(lbl) heap[lbl+1]
#define label_name(lbl) heap[lbl+2]
#define label_file(lbl) heap[lbl+3]
#define label_line(lbl) heap[lbl+4]
#define goto_label_fs(lbl) heap[lbl+2]

#define START_INIT_BLOCK() \
  def_label(init_next_lbl); \
  init_next_lbl = alloc_label("init_next");
#define END_INIT_BLOCK()   \
  jump(init_next_lbl);

#if defined (UNDEFINED_LABELS_ARE_RUNTIME_ERRORS) || defined (SAFE_MODE)
#define LABELS_ARR_SIZE 100000
int labels[LABELS_ARR_SIZE];
int labels_ix = 0;

#ifdef UNDEFINED_LABELS_ARE_RUNTIME_ERRORS
void def_label(int lbl);
#endif

void assert_all_labels_defined(int init_next_lbl) {
  int i = 0;
  int lbl;
  // Check that all labels are defined
  for (; i < labels_ix; i++) {
    lbl = labels[i];
    if (lbl != init_next_lbl && label_addr(lbl) > 0) {
#ifdef UNDEFINED_LABELS_ARE_RUNTIME_ERRORS
      if (label_kind(lbl) == GENERIC_LABEL && label_name(lbl) != 0) {
        def_label(lbl);
        rt_debug("Function or label is not defined\n");
        rt_debug("name = ");
        rt_debug((char*) label_name(lbl));
        rt_debug("\n");
        // TODO: This should crash but let's just return for now to see how far we can get
        ret();
      }
#else
      putstr("Label ");
      if (label_kind(lbl) == GENERIC_LABEL && label_name(lbl) != 0) {
        putstr((char*) label_name(lbl));
      } else {
        putint(lbl);
      }
      putstr(" is not defined\n");
      exit(1);
#endif
    }
  }
}

void add_label(int lbl) {
  if (labels_ix >= LABELS_ARR_SIZE) fatal_error("labels array is full");

  labels[labels_ix++] = lbl;
}

int alloc_label(char* name) {
  int lbl = alloc_obj(5);
  label_kind(lbl) = GENERIC_LABEL;
  label_addr(lbl) = 0;
  label_name(lbl) = (intptr_t) name;
  label_file(lbl) = (intptr_t) fd_filepath;
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
  label_line(lbl) = line_number;
#endif
  add_label(lbl);
  return lbl;
}
#else

#define assert_all_labels_defined(x) // No-op
#define add_label(lbl) // No-op
#define alloc_label(name) alloc_label_()

int alloc_label_() {
  int lbl = alloc_obj(2);
  label_kind(lbl) = GENERIC_LABEL;
  label_addr(lbl) = 0;
  add_label(lbl);
  return lbl;
}
#endif

#ifdef SUPPORT_GOTO

int alloc_goto_label() {
  int lbl = alloc_obj(3);
  label_kind(lbl) = GOTO_LABEL;
  label_addr(lbl) = 0;
  goto_label_fs(lbl) = 0;
  add_label(lbl);
  return lbl;
}

#endif // SUPPORT_GOTO

bool is_label_defined(int lbl) {
  return label_addr(lbl) < 0;
}

void use_label(int lbl) {

  int addr = label_addr(lbl);

#ifdef SAFE_MODE
  if (label_kind(lbl) != GENERIC_LABEL) fatal_error("use_label expects generic label");
#endif

  if (addr < 0) {
    // label address is currently known
    addr = -addr - (code_address_base + code_alloc + 4); // compute relative address
    emit_i32_le(addr);
  } else {
    // label address is not yet known.
    // In this case, we keep track of the locations that need to be patched when
    // the label is defined as a list stored in the code buffer. The label
    // points to the first address to patch, and the address of the next patch
    // is stored in the code buffer like so:
    // label_addr(lbl) = [ patch address #1 ]
    //
    // Code buffer:
    // |-----------------------------------------------
    // | ...
    // | patch address #1: [ patch address #2 ]
    // | ...
    // | patch address #2: 0 (end of list)
    // | ...
    // |-----------------------------------------------
    emit_i32_le(0); // 32 bit placeholder for distance
    code[code_alloc-1] = addr; // chain with previous patch address
    label_addr(lbl) = code_alloc;
  }
}

void def_label(int lbl) {

  int addr = label_addr(lbl);
  int def_addr = code_alloc;
  int next_addr;

#ifdef SAFE_MODE
  if (label_kind(lbl) != GENERIC_LABEL) fatal_error("def_label expects generic label");
#endif

  if (addr < 0) {
#ifdef SAFE_MODE
    putstr("Label ");
    if (label_name(lbl) != 0) {
      putstr((char*) label_name(lbl));
    } else {
      putint(lbl);
    }
    putstr(" previously defined at ");
    putstr((char*) label_file(lbl));
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
    putstr(":");
    putint(label_line(lbl));
#endif
    fatal_error(" being redefined");
#endif
  } else {
    label_addr(lbl) = - (code_address_base + code_alloc); // define label's address
    while (addr != 0) {
      next_addr = code[addr - 1]; // get pointer to next patch address before we overwrite it
      code_alloc = addr - 4; // place code pointer to where use_label was called
      emit_i32_le(def_addr - addr); // replace placeholder with relative address
      addr = next_addr;
    }
    code_alloc = def_addr;
  }
}

#ifdef SUPPORT_GOTO

// Similar to use_label, but for gotos.
// The main difference is that it adjusts the stack and jumps, as opposed to
// simply emitting the address.
void jump_to_goto_label(int lbl) {

  int addr = label_addr(lbl);
  int lbl_fs = goto_label_fs(lbl);
  int start_code_alloc = code_alloc;

#ifdef SAFE_MODE
  if (label_kind(lbl) != GOTO_LABEL) fatal_error("jump_to_goto_label expects goto label");
#endif

  if (addr < 0) {
    // label address is currently known
    stack_grow(lbl_fs - cgc_fs);
    start_code_alloc = code_alloc;
    jump_rel(0); // Generate dummy jump instruction to get instruction length
    addr = -addr - code_alloc; // compute relative address
    code_alloc = start_code_alloc;
    jump_rel(addr);
  } else {
    // label address is not yet known
    // placeholders for when we know the destination address and frame size
    stack_grow(0);
    jump_rel(0);
    code[code_alloc-1] = addr; // chain with previous patch address
    code[code_alloc-2] = cgc_fs; // save current frame size
    code[code_alloc-3] = start_code_alloc; // track initial code alloc so we can come back
    label_addr(lbl) = code_alloc;
  }
}

void def_goto_label(int lbl) {

  int addr = label_addr(lbl);
  int def_addr = code_alloc;
  int next_addr;
  int goto_fs;
  int start_code_alloc;

#ifdef SAFE_MODE
  if (label_kind(lbl) != GOTO_LABEL) fatal_error("def_goto_label expects goto label");
#endif

  if (addr < 0) {
    fatal_error("goto label defined more than once");
  } else {
    label_addr(lbl) = -def_addr;    // define label's address
    goto_label_fs(lbl) = cgc_fs;    // define label's frame size
    while (addr != 0) {
      next_addr = code[addr-1]; // get pointer to next patch address
      goto_fs = code[addr-2]; // get frame size at goto instruction
      code_alloc = code[addr-3]; // reset code pointer to start of jump_to_goto_label instruction
      stack_grow(cgc_fs - goto_fs); // adjust stack
      start_code_alloc = code_alloc;
      jump_rel(0); // Generate dummy jump instruction to get instruction length
      addr = def_addr - code_alloc; // compute relative address
      code_alloc = start_code_alloc;
      jump_rel(addr);
      addr = next_addr;
    }
    code_alloc = def_addr;
  }
}

#endif // SUPPORT_GOTO

ast one_literal;
ast int_type;
#if defined(PARSE_NUMERIC_LITERAL_SUFFIX) || defined(SUPPORT_SIZEOF)
ast uint_type;
#endif
#if defined(PARSE_NUMERIC_LITERAL_SUFFIX)
ast long_type;
ast ulong_type;
#endif
ast char_type;
ast string_type;
ast void_type;
ast void_star_type;

ast dereference_type(ast type) {
  switch (get_op(type)) {
    case '[': // Array type
      return get_child_('[', type, 0);
    case '*': // Pointer type
      return get_child_('*', type, 1);
    default:
      dump_op(get_op(type));
      fatal_error("dereference_type: non pointer is being dereferenced with *");
      return -1;
  }
}

ast function_return_type(ast type) {
  if (get_op(type) == '*') {
    type = get_child_('*', type, 1);
  }
  return get_child_('(', type, 0);
}

// Type, structure and union handling
int struct_union_size(ast struct_type);

// A pointer type is either an array type or a type with at least one star
bool is_pointer_type(ast type) {
  bool op = get_op(type);
  return op == '[' || op == '*';
}

bool is_function_type(ast type) {
  int op = get_op(type);
  if (op == '*') {
    op = get_op(get_child_('*', type, 1));
  }
  return op == '(';
}

#ifdef SUPPORT_EMULATED_INT64
bool is_int64_type(ast type) {
  return get_op(type) == LONG_KW;
}
#endif

#ifdef SUPPORT_STRUCT_UNION

bool is_struct_or_union_type(ast type) {
  int op = get_op(type);
  return op == STRUCT_KW || op == UNION_KW;
}

#ifdef SUPPORT_EMULATED_INT64
// A type handled by the struct machinery: struct, union, 8-byte long long.
// These are multi-word objects copied by value, returned through a hidden
// pointer, and represented on the operand stack by the address of their
// storage.
bool is_struct_like(ast type) {
  int op = get_op(type);
  return op == STRUCT_KW || op == UNION_KW || op == LONG_KW;
}
#else
#define is_struct_like(type) is_struct_or_union_type(type)
#endif

#endif // SUPPORT_STRUCT_UNION

// An aggregate type is either an array type or a struct/union type (that's not a
// reference), or (under SUPPORT_EMULATED_INT64) a 64-bit long long.
// Aggregate values live in memory and their rvalue on the operand stack is the
// address of that memory, not the value itself.
bool is_aggregate_type(ast type) {
  int op = get_op(type);
#ifdef SUPPORT_STRUCT_UNION
  return op == '[' || op == STRUCT_KW || op == UNION_KW
#ifdef SUPPORT_EMULATED_INT64
      || op == LONG_KW
#endif
      ;
#else
  return op == '[';
#endif // SUPPORT_STRUCT_UNION
}

bool is_numeric_type(ast type) {
  switch (get_op(type)) {
    case CHAR_KW:
    case INT_KW:
    case FLOAT_KW:
    case DOUBLE_KW:
    case SHORT_KW:
    case LONG_KW:
    case ENUM_KW: // Enums are considered numeric types
      return true;
    default: // Struct/union/pointer/array
      return false;
  }
}

bool is_signed_numeric_type(ast type) {
  switch (get_op(type)) {
    case CHAR_KW:
    case INT_KW:
    case FLOAT_KW:
    case DOUBLE_KW:
    case SHORT_KW:
    case LONG_KW:
      return !TEST_TYPE_SPECIFIER(get_val(type), UNSIGNED_KW);
    default:
      return true; // Not a numeric type => it's a struct/union/pointer/array and we consider it signed
  }
}

// Size an object of the given type would occupy in memory (in bytes).
// If array_value is true, the size of the array is returned, otherwise the
// size of the pointer is returned.
// If word_align is true, the size is rounded up to the word size.
int type_width(ast type, bool array_value, bool word_align) {
  int width = 1;
  // Basic type kw
  switch (get_op(type)) {
    case '[':
      // In certain contexts, we want to know the static size of the array (i.e.
      // sizeof, in struct definitions, etc.) while in other contexts we care
      // about the pointer (i.e. when passing an array to a function, etc.)
      if (array_value) {
        width = get_child_('[', type, 1) * type_width(get_child_('[', type, 0), true, false);
      } else {
        width = WORD_SIZE; // Array is a pointer to the first element
      }
      break;
    case '*':      width = WORD_SIZE; break;
    case VOID_KW:  width = 1;         break; // Default to 1 byte for void so pointer arithmetic and void casts work
    case CHAR_KW:  width = 1;         break;
    case SHORT_KW: width = 2;         break;
    case INT_KW:   width = 4;         break;
    case LONG_KW:
#if WORD_SIZE == 8 || defined (SUPPORT_EMULATED_INT64)
      width = 8;
      break;
#elif defined (BOOTSTRAP_LONG)
      width = 4;
      break;
#else
      fatal_error("type_width: long type not supported");
      return -1;
#endif
#ifdef SUPPORT_STRUCT_UNION
    case STRUCT_KW:
    case UNION_KW:
      width = struct_union_size(type);
      break;
#endif // SUPPORT_STRUCT_UNION
    default:       width = WORD_SIZE; break;
  }

  if (word_align) width = word_size_align(width);
  return width;
}

// Width of an object pointed to by a reference type.
ast ref_type_width(ast type) {
  return type_width(dereference_type(type), false, false);
}

// Structs, enums and unions types come in 2 variants:
//  - definition: the type contains the members of the struct/enum/union
//  - reference: the type reference an already declared struct/enum/union and doesn't contain the members.
//
// We mostly want to work with type definitions, and not type references so
// this function returns the type definition when passed a type reference.
ast canonicalize_type(ast type) {
  ast res = type;
  int binding;

#ifdef SUPPORT_STRUCT_UNION
  if (get_op(type) == STRUCT_KW && get_child_opt_(STRUCT_KW, LIST, type, 2) == 0) { // struct with empty def => reference
    binding = cgc_lookup_struct(get_val_(IDENTIFIER, get_child__(STRUCT_KW, IDENTIFIER, type, 1)), cgc_globals);
  } else
  if (get_op(type) == UNION_KW && get_child_opt_(UNION_KW, LIST, type, 2) == 0) { // union with empty def => reference
    binding = cgc_lookup_union(get_val_(IDENTIFIER, get_child__(UNION_KW, IDENTIFIER, type, 1)), cgc_globals);
  } else
#endif // SUPPORT_STRUCT_UNION
  if (get_op(type) == ENUM_KW && get_child_opt_(ENUM_KW, LIST, type, 2) == 0) { // enum with empty def => reference
    binding = cgc_lookup_enum(get_val_(IDENTIFIER, get_child__(ENUM_KW, IDENTIFIER, type, 1)), cgc_globals);
  } else {
    return res;
  }

  if (binding == 0) {
    dump_ident(get_val_(IDENTIFIER, get_child(type, 1)));
    fatal_error("canonicalize_type: type is not defined");
  }
  res = typedef_binding_type(binding);

  return res;
}

#ifdef SUPPORT_STRUCT_UNION

// Size of the largest member of a struct or union, used for alignment
int struct_union_size_largest_member = 0;

int type_largest_member(ast type) {
  switch (get_op(type)) {
    case STRUCT_KW:
    case UNION_KW:
      struct_union_size_largest_member = 0;
      struct_union_size(type); // Compute struct_union_size_largest_member global
      return struct_union_size_largest_member;
    case '[':
      return type_largest_member(get_child_('[', type, 0));
    default:
      return type_width(type, true, false);
  }
}

// Size of a struct or union type
int struct_union_size(ast type) {
  ast members;
  ast member_type;
  int member_size, largest_submember_size;
  int sum_size = 0, max_size = 0, largest_member_size = 0;

  type = canonicalize_type(type);
  members = get_child(type, 2);

  while (members != 0) {
    member_type = get_child_(DECL, car_(DECL, members), 1);
    members = tail(members);
    member_size = type_width(member_type, true, false);
    largest_submember_size = type_largest_member(member_type);
    if (member_size != 0) sum_size = align_to(largest_submember_size, sum_size); // Align the member to the word size
    sum_size += member_size;                                          // Struct size is the sum of its members
    if (member_size > max_size) max_size = member_size;               // Union size is the max of its members
    if (largest_member_size < largest_submember_size) largest_member_size = largest_submember_size;
  }

  sum_size = align_to(largest_member_size, sum_size); // The final struct size is a multiple of its widest member
  max_size = align_to(largest_member_size, max_size); // The final union size is a multiple of its widest member

  // Set the struct_union_size_largest_member global to "return" it
  struct_union_size_largest_member = largest_member_size;
  return TERNARY(get_op(type) == STRUCT_KW, sum_size, max_size);
}

// Find offset of struct member
int struct_member_offset_go(ast struct_type, ast member_ident) {
  ast members = get_child(canonicalize_type(struct_type), 2);
  int offset = 0;
  int member_size, sub_offset;
  ast decl, ident;

  while (members != 0) {
    decl = car_(DECL, members);
    ident = get_child_opt_(DECL, IDENTIFIER, decl, 0);
    if (ident == 0) { // Anonymous struct member, search that struct
      sub_offset = struct_member_offset_go(get_child_(DECL, decl, 1), member_ident);
      if (sub_offset != -1) return offset + sub_offset;
    } else if (get_val_(IDENTIFIER, member_ident) == get_val_(IDENTIFIER, ident)) {
      return offset;
    }

    if (get_op(struct_type) == STRUCT_KW) {
      // For unions, fields are always at offset 0. We must still iterate
      // because the field may be in an anonymous struct, in which case the
      // final offset is not 0.
      member_size = type_width(get_child_(DECL, decl, 1), true, false);
      if (member_size != 0) offset = align_to(type_largest_member(get_child_(DECL, decl, 1)), offset);
      offset += member_size;
    }
    members = tail(members);
  }

  return -1;
}

int struct_member_offset(ast struct_type, ast member_ident) {
  int offset = struct_member_offset_go(struct_type, member_ident);
  if (offset == -1) fatal_error("struct_member_offset: member not found");
  return offset;
}

// Find a struct member
ast struct_member_go(ast struct_type, ast member_ident) {
  ast members = get_child(canonicalize_type(struct_type), 2);
  ast decl, ident;

  while (members != 0) {
    decl = car_(DECL, members);
    ident = get_child_opt_(DECL, IDENTIFIER, decl, 0);
    if (ident == 0) { // Anonymous struct member, search that struct
      ident = struct_member_go(get_child_(DECL, decl, 1), member_ident);
      if (ident != -1) return ident; // Found member in the anonymous struct
    } else if (get_val_(IDENTIFIER, member_ident) == get_val_(IDENTIFIER, ident)) {
      return decl;
    }
    members = tail(members);
  }

  return -1;
}

ast struct_member(ast struct_type, ast member_ident) {
  ast member = struct_member_go(struct_type, member_ident);
  if (member == -1) fatal_error("struct_member: member not found");
  return member;
}

#endif // SUPPORT_STRUCT_UNION

int resolve_identifier(int ident_symbol) {
  int binding = cgc_lookup_var(ident_symbol, cgc_locals);
  if (binding != 0) return binding;

  binding = cgc_lookup_var(ident_symbol, cgc_globals);
  if (binding != 0) return binding;

  binding = cgc_lookup_fun(ident_symbol, cgc_globals);
  if (binding != 0) return binding;

  binding = cgc_lookup_enum_value(ident_symbol, cgc_globals);
  if (binding != 0) return binding;

  dump_ident(ident_symbol);
  fatal_error("identifier not found");
  return 0;
}

#ifdef SUPPORT_FULL_ARITHMETIC

// ============================= Proper Arithmetic =============================
//
// To simplify the code generation, pnut-exe generates code for a stack machine,
// where the operands of every expression are kept on the stack. Because the
// instructions generated by pnut-exe are mostly word-sized, the values in the
// registers and on the stacck are always correctly extended to the word size.
// The following functions implement the integer promotion and the usual
// arithmetic conversions rules of C, to ensure that the invariant "the value of
// every expression is correctly extended to the word size in registers and on
// the stack" is maintained.
//
// =============================================================================

// Integer conversion rank.
// Note that the parser maps "long" to INT_KW and "long long" to LONG_KW.
int type_rank(const ast type) {
  switch (get_op(type)) {
    case CHAR_KW:   return 1;
    case SHORT_KW:  return 2;
    case LONG_KW:   return 4;
    case FLOAT_KW:  return 5;
    case DOUBLE_KW: return 6;
    default:        return 3; // INT_KW, ENUM_KW
  }
}

// Integer promotions (C99 6.3.1.1): types narrower than int are widened to int.
// int can represent all values of char and short, signed or unsigned, so the
// promoted type is always plain (signed) int.
ast integer_promote(const ast type) {
  if (type_rank(type) < 3) return int_type;
  return type;
}

// Usual arithmetic conversions (C99 6.3.1.8): the common type both operands of
// a binary operation are converted to, which is also the result type. Operands
// are promoted, then the higher-rank type wins; at equal rank, unsigned wins.
// For pointer arithmetic type, return void* as the common type.
ast usual_arith_conv(ast left_type, ast right_type) {
  if (is_pointer_type(left_type) || is_pointer_type(right_type)) {
    // For pointer arithmetic, return void* as the common type.
    return void_star_type;
  }
  left_type = integer_promote(left_type);
  right_type = integer_promote(right_type);
  if (type_rank(left_type) < type_rank(right_type)) return right_type;
  if (type_rank(left_type) > type_rank(right_type)) return left_type;
  return TERNARY(is_signed_numeric_type(left_type), right_type, left_type);
}

// Whether converting a value from from_type to to_type changes its word-sized
// register representation. Registers always hold values correctly extended for
// their type, and integer conversions keep the value bits (C99 6.3.1.3), so a
// conversion is only visible when it changes how the value is extended into the
// register.
bool conversion_needed(ast from_type, ast to_type) {
  int from_width = type_width(from_type, false, false);
  int to_width = type_width(to_type, false, false);
  if (to_width >= WORD_SIZE) {
    return false; // The value already fills the register
  } else if (from_width > to_width) {
    return true; // Narrowing truncates the value
  } else if (from_width == to_width) {
    // Same width: only a signedness change re-extends
    if (is_signed_numeric_type(from_type) != is_signed_numeric_type(to_type)) {
      return true;
    } else {
      return false;
    }
  } else { // from_width < to_width (widening)
    // Widening only matters when going from signed to unsigned: the must be
    // zero-extended if the target is unsigned.
    // signed->signed and unsigned->* are already correctly extended.
    if (is_signed_numeric_type(from_type) && !is_signed_numeric_type(to_type)) {
      return true;
    } else {
      return false;
    }
  }
}

// Convert a value in a register from from_type to to_type, performing the
// conversion if needed.
void convert_reg(const int reg, const ast from_type, const ast to_type) {
  if (conversion_needed(from_type, to_type)) {
    extend_reg(reg, type_width(to_type, false, false), is_signed_numeric_type(to_type));
  }
}

#else

// Bootstrapping pnut doesn't require proper integer promotion and the usual
// arithmetic conversions. Stub them out to just return the type of the right
// operand, which is good enough.
#define integer_promote(x) (x)
#define usual_arith_conv(x, y) (y)
#define convert_reg(reg, from_type, to_type) ((void) 0)

#endif // SUPPORT_FULL_ARITHMETIC

// Compute result type of a binary arithmetic operation.
ast arith_value_type(int op, ast left_type, ast right_type) {
  if (is_pointer_type(left_type) && is_pointer_type(right_type) && op == '-') {
    return int_type; // pointer - pointer -> integer
  } else if (is_pointer_type(left_type)) {
    // pointer + integer -> pointer
    return left_type;
  } else if (is_pointer_type(right_type)) {
    // integer + pointer -> pointer
    return right_type;
  } else {
    // integer + integer -> integer, with promotion rules applied
    return usual_arith_conv(left_type, right_type);
  }
}

// Compute the type of an expression
ast value_type(ast node) {
  int op = get_op(node);
  int nb_children = get_nb_children(node);
  int binding;
  ast left_type, right_type;
  ast child0, child1;

  if (nb_children >= 1) child0 = get_child(node, 0);
  if (nb_children >= 2) child1 = get_child(node, 1);

  if (nb_children == 0) {
    if (op == INTEGER) {
#if 0
      // C99 6.4.4.1 Integer constants:
      // > The type of an integer constant is the first of the corresponding
      // > list in which its value can be represented.
      //
      // > Decimal literal:
      // >  No suffix: int -> long int -> long long int
      // >  {u|U} suffix: uint -> long uint -> long long uint
      // >  {l|L} suffix: long int -> long long int;
      // >  {u|U}{l|L} suffix: long uint -> long long uint
      // > {ll|LL} suffix: long long int;
      // > {u|U}{ll|LL} suffix: long long uint
      //
      // > Octal/Hex literal:
      // >  No suffix: int -> uint -> long int -> long uint -> long long int -> long long uint
      // >  {u|U} suffix: uint -> long uint -> long long uint
      // >  {l|L} suffix: long int -> long uint -> long long int -> long long uint
      // >  {u|U}{l|L} suffix: long uint -> long long uint
      // >  {ll|LL} suffix: long long int -> long long uint
      // >  {u|U}{ll|LL} suffix: long long uint
      //
      // The suffixes are parsed by the parser, but the values are not tested
      // for fitting in the type they are suffixed with, so we need to apply the
      // rules above to determine the type of the literal.
      //
      // Note that the parser overwrites the base of the literal when a suffix
      // is present. In that case, the suffix is used to determine the type of
      // the literal, and the base is ignored. This is _almost_ correct, with
      // the type ladder being the same for decimal and hex/octal literals,
      // except for the `l` and `ll` suffix, where decimal literals are always
      // signed and hex/octal literals can be unsigned.
      // Ideally, the parser would keep both the base and the suffix, but this
      // is good enough for now.
#endif

#ifdef SUPPORT_64_BIT_LITERALS
      // The value is encoded by pnut.c::u64_to_obj, see function for details.
      if (!is_large_int(get_val_(INTEGER, node))) { // Small "unboxed" int
        return int_type;
      } else if (I32_POSITIVE(large_int_hi(get_val_(INTEGER, node)))) { // Large int with non-negative high word
        return long_type;
      } else { // Large int with negative high word
        return ulong_type;
      }
#else
      // Without support for 64-bit literals, all literals fit in an int
      return int_type;
#endif
    }
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
    else if (op == INTEGER_HEX || op == INTEGER_OCT) {
#ifdef SUPPORT_64_BIT_LITERALS
      // Type ladder: int -> uint -> long -> ulong.
      if (!is_large_int(get_val(node))) { // Small "unboxed" int
        return int_type;
      } else if (large_int_hi(get_val(node)) == 0) { // Large int with zero high word
        return uint_type;
      } else if (I32_POSITIVE(large_int_hi(get_val(node)))) { // Large int with non-negative high word
        return long_type;
      } else { // Large int with negative high word
        return ulong_type;
      }
#else
      return int_type;
#endif
    }
#endif
#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
    else if (op == INTEGER_L || op == INTEGER_LL) {
#ifdef SUPPORT_64_BIT_LITERALS
      // long and long long coincide as the 64-bit LONG_KW type here. A value
      // that doesn't fit in a signed 64-bit long -- bit 63 set, i.e. a large
      // int with a negative high word -- becomes unsigned long.
      if (is_large_int(get_val(node)) && I32_NEGATIVE(large_int_hi(get_val(node)))) {
        return ulong_type;
      } else {
        return long_type;
      }
#else
      return int_type;
#endif
    } else if (op == INTEGER_U) {
#ifdef SUPPORT_64_BIT_LITERALS
      // unsigned int -> unsigned long: a value too wide for a 32-bit unsigned
      // int (large int with a non-zero high word) becomes unsigned long.
      if (is_large_int(get_val(node)) && large_int_hi(get_val(node)) != 0) {
        return ulong_type;
      } else {
        return uint_type;
      }
#else
      return uint_type;
#endif
    } else if (op == INTEGER_UL || op == INTEGER_ULL) {
      return ulong_type;
    }
#endif
    else if (op == CHARACTER) {
      return int_type; // Character literals have type int in C
    } else if (op == STRING) {
      return string_type;
    } else if (op == IDENTIFIER) {
      binding = resolve_identifier(get_val_(IDENTIFIER, node));
      switch (binding_kind(binding)) {
        case BINDING_PARAM_LOCAL:
        case BINDING_VAR_LOCAL:
          return var_binding_type(binding);
        case BINDING_VAR_GLOBAL:
          return var_binding_type(binding);
        case BINDING_ENUM_CST:
          return int_type;
        case BINDING_FUN:
          return fun_binding_type(binding);
        default:
          dump_ident(get_val_(IDENTIFIER, node));
          fatal_error("value_type: unknown identifier");
          return -1;
      }
    } else {
      dump_node(node);
      fatal_error("value_type: unexpected operator");
      return -1;
    }

  } else if (nb_children == 1) {

    if (op == '*') {
      left_type = value_type(child0);
      if (is_function_type(left_type)) {
        return left_type;
      } else {
        return dereference_type(left_type);
      }
    } else if (op == '&') {
      left_type = value_type(child0);
      return pointer_type(left_type, false);
    } else if (op == '!') {
      return int_type; // Logical not always returns an integer
    } else if (op == '+' || op == '-' || op == '~') {
      // Unary +, - and ~ apply the integer promotions to their operand
      return integer_promote(value_type(child0));
    } else if (op == MINUS_MINUS_POST || op == PLUS_PLUS_POST || op == PLUS_PLUS_PRE || op == MINUS_MINUS_PRE) {
      // Increment/decrement, like assignment, keep the operand's type
      return value_type(child0);
    }
#ifdef SUPPORT_SIZEOF
    else if (op == SIZEOF_KW) {
      return uint_type; // sizeof always returns an unsigned integer
    }
#endif
    else {
      dump_node(node);
      fatal_error("value_type: unexpected operator");
      return -1;
    }

  } else if (nb_children == 2) {

    if (op == LSHIFT || op == RSHIFT) {
      // The result type of a shift is the (promoted) left operand type; the
      // right operand's type plays no role.
      return integer_promote(value_type(child0));
    } else if (op == '+' || op == '-' || op == '*' || op == '/' || op == '%' || op == '&' || op == '|' || op == '^') {
      left_type = value_type(child0);
      right_type = value_type(child1);
      return arith_value_type(op, left_type, right_type);
    } else if (op == '<' || op == '>' || op == EQ_EQ || op == EXCL_EQ || op == LT_EQ || op == GT_EQ) {
      return int_type; // Comparison always returns an integer
    } else if (op == ',') {
      return value_type(child1); // The type of the right operand
    } else if (op == '[') {
      left_type = value_type(child0);
      right_type = value_type(child1);

      if (get_op(left_type) == '[' || get_op(left_type) == '*') {
        return dereference_type(left_type);
      } else if (get_op(right_type) == '[' || get_op(right_type) == '*') {
        return dereference_type(right_type);
      } else {
        dump_op(get_op(left_type));
        dump_op(get_op(right_type));
        fatal_error("value_type: non pointer is being dereferenced as array");
        return -1;
      }
    } else if (op == '=' || op == AMP_EQ || op == BAR_EQ || op == CARET_EQ || op == LSHIFT_EQ || op == MINUS_EQ || op == PERCENT_EQ || op == PLUS_EQ || op == RSHIFT_EQ || op == SLASH_EQ || op == STAR_EQ) {
      return value_type(child0); // Only the left side is relevant here
    } else if (op == AMP_AMP || op == BAR_BAR) {
      return int_type; // Logical and/or always returns an integer
    } else if (op == '(') {
      left_type = value_type(child0);
      if (is_function_type(left_type)) {
        return function_return_type(left_type);
      } else {
        fatal_error("value_type: not a function or function pointer");
        return -1;
      }
    }
#ifdef SUPPORT_STRUCT_UNION
    else if (op == '.') {
      left_type = value_type(child0);
      if (is_struct_or_union_type(left_type)) {
        return get_child_(DECL, struct_member(left_type, child1), 1); // child 1 of member is the type
      } else {
        fatal_error("value_type: . operator on non-struct pointer type");
        return -1;
      }
    } else if (op == ARROW) {
      // Same as '.', but left_type must be a pointer
      left_type = value_type(child0);
      if (get_op(left_type) == '*' && is_struct_or_union_type(get_child_('*', left_type, 1))) {
        return get_child_(DECL, struct_member(get_child_('*', left_type, 1), child1), 1); // child 1 of member is the type
      } else {
        fatal_error("value_type: -> operator on non-struct pointer type");
        return -1;
      }
    }
#endif
    else if (op == CAST) {
      return get_child_(DECL, child0, 1);
    } else {
      fatal_error("value_type: unexpected operator");
      return -1;
    }

  } else if (nb_children == 3) {

    if (op == '?') {
#ifdef SUPPORT_FULL_ARITHMETIC
      left_type = value_type(child1);
      right_type = value_type(get_child(node, 2));
      if (is_numeric_type(left_type) && is_numeric_type(right_type)) {
        // The result type is the common type of the two arms
        return usual_arith_conv(left_type, right_type);
      } else {
        return left_type; // Otherwise assume the 2 cases have the same type.
      }
#else
      return value_type(child1); // Assume the 2 cases have the same type
#endif // SUPPORT_FULL_ARITHMETIC
    } else {
      dump_node(node);
      fatal_error("value_type: unexpected operator");
      return -1;
    }

  } else {
    dump_node(node);
    fatal_error("value_type: unexpected operator");
    return -1;
  }
}

void codegen_binop_cmp(int cond) {
  int lbl1 = alloc_label(0);
  int lbl2 = alloc_label(0);
  jump_cond_reg_reg(cond, lbl1, reg_X, reg_Y);
  xor_reg_reg(reg_X, reg_X);
  jump(lbl2);
  def_label(lbl1);
  mov_reg_imm(reg_X, 1);
  def_label(lbl2);
}

void codegen_binop_add(ast left_type, ast right_type) {
  // Add two operands, applying pointer arithmetic rules if needed.

  if (is_pointer_type(left_type) && !is_pointer_type(right_type)) {
    // pointer + integer -> pointer
    // result = ptr + integer * sizeof(dereferenced object)
    mul_for_pointer_arith(reg_Y, ref_type_width(left_type));
  } else if (is_pointer_type(right_type) && !is_pointer_type(left_type)) {
    // integer + pointer -> pointer
    // result = ptr + integer * sizeof(dereferenced object)
    mul_for_pointer_arith(reg_X, ref_type_width(right_type));
  }

  add_reg_reg(reg_X, reg_Y);
}

void codegen_binop_sub(ast left_type, ast right_type) {
  // Subtract two operands, applying pointer arithmetic rules if needed.

  if (is_pointer_type(left_type) && is_pointer_type(right_type)) {
    // pointer - pointer -> integer
    // result = (ptr1 - ptr2) / sizeof(dereferenced object)
    sub_reg_reg(reg_X, reg_Y);
    div_for_pointer_arith(reg_X, ref_type_width(left_type));
  } else if (is_pointer_type(left_type)) {
    // pointer - integer -> pointer
    // result = ptr - integer * sizeof(dereferenced object)
    mul_for_pointer_arith(reg_Y, ref_type_width(left_type));
    sub_reg_reg(reg_X, reg_Y);
  } else if (is_pointer_type(right_type)) {
    // integer - pointer -> pointer
    // result = ptr - integer * sizeof(dereferenced object)
    mul_for_pointer_arith(reg_X, ref_type_width(right_type));
    sub_reg_reg(reg_X, reg_Y);
  } else {
    sub_reg_reg(reg_X, reg_Y);
  }
}

void codegen_binop(int op, ast left_type, ast right_type) {
  ast common_type = usual_arith_conv(left_type, right_type);
  bool left_is_numeric = is_numeric_type(left_type);
  bool right_is_numeric = is_numeric_type(right_type);

  stack_pop(reg_Y); // rhs operand
  stack_pop(reg_X); // lhs operand

#ifdef SUPPORT_FULL_ARITHMETIC
  // Operands are converted to the common type given by the usual arithmetic
  // conversions, which only emits code when the conversion changes the
  // operand's register representation (mixed signedness or narrowing).
  // As an optimization, we skip the conversion for ops that don't need it:
  // - add, sub, mul and left shift: lower bits are not affected by upper bits
  // - right shift: only the lower 6 bits of the shift count are relevant
  if (left_is_numeric && right_is_numeric) {
    if ( op != '+' && op != PLUS_EQ && op != PLUS_PLUS_PRE && op != PLUS_PLUS_POST
      && op != '-' && op != MINUS_EQ && op != MINUS_MINUS_PRE && op != MINUS_MINUS_POST
      && op != '*' && op != STAR_EQ
      && op != LSHIFT && op != LSHIFT_EQ
      && op != RSHIFT && op != RSHIFT_EQ) {
      convert_reg(reg_X, left_type, common_type);
      convert_reg(reg_Y, right_type, common_type);
    }
  }
#endif // SUPPORT_FULL_ARITHMETIC

  if      (op == '<')     { codegen_binop_cmp(TERNARY(is_signed_numeric_type(common_type), LT, LT_U)); }
  else if (op == '>')     { codegen_binop_cmp(TERNARY(is_signed_numeric_type(common_type), GT, GT_U)); }
  else if (op == LT_EQ)   { codegen_binop_cmp(TERNARY(is_signed_numeric_type(common_type), LE, LE_U)); }
  else if (op == GT_EQ)   { codegen_binop_cmp(TERNARY(is_signed_numeric_type(common_type), GE, GE_U)); }
  else if (op == EQ_EQ)   { codegen_binop_cmp(EQ); }
  else if (op == EXCL_EQ) { codegen_binop_cmp(NE); }
  else if (op == '+' || op == PLUS_EQ || op == PLUS_PLUS_PRE || op == PLUS_PLUS_POST) {
    codegen_binop_add(left_type, right_type);
  }
  else if (op == '-' || op == MINUS_EQ || op == MINUS_MINUS_PRE || op == MINUS_MINUS_POST) {
    codegen_binop_sub(left_type, right_type);
  }
  else if (op == '*' || op == STAR_EQ) {
    if (!left_is_numeric || !right_is_numeric) fatal_error("invalid operands to *");
    if (is_signed_numeric_type(common_type)) imul_reg_reg(reg_X, reg_Y);
    else mul_reg_reg(reg_X, reg_Y);
  }
  else if (op == '/' || op == SLASH_EQ) {
    if (!left_is_numeric || !right_is_numeric) fatal_error("invalid operands to /");
    if (is_signed_numeric_type(common_type)) idiv_reg_reg(reg_X, reg_Y);
    else div_reg_reg(reg_X, reg_Y);
  }
  else if (op == '%' || op == PERCENT_EQ) {
    if (!left_is_numeric || !right_is_numeric) fatal_error("invalid operands to %");
    if (is_signed_numeric_type(common_type)) irem_reg_reg(reg_X, reg_Y);
    else rem_reg_reg(reg_X, reg_Y);
  }
  else if (op == RSHIFT || op == RSHIFT_EQ) {
    if (!left_is_numeric || !right_is_numeric) fatal_error("invalid operands to >>");
    // Whether the shift is arithmetic or logical depends only on the promoted
    // left operand's type; the signedness of the shift count is irrelevant.
    if (is_signed_numeric_type(integer_promote(left_type))) sar_reg_reg(reg_X, reg_Y);
    else shr_reg_reg(reg_X, reg_Y);
  }
  else if (op == LSHIFT || op == LSHIFT_EQ) {
    if (!left_is_numeric || !right_is_numeric) fatal_error("invalid operands to <<");
    s_l_reg_reg(reg_X, reg_Y); // Shift left, independent of sign
  }
  else if (op == '&' || op == AMP_EQ) {
    if (!left_is_numeric || !right_is_numeric) fatal_error("invalid operands to &");
    and_reg_reg(reg_X, reg_Y);
  }
  else if (op == '|' || op == BAR_EQ) {
    if (!left_is_numeric || !right_is_numeric) fatal_error("invalid operands to |");
    or_reg_reg(reg_X, reg_Y);
  }
  else if (op == '^' || op == CARET_EQ) {
    if (!left_is_numeric || !right_is_numeric) fatal_error("invalid operands to ^");
    xor_reg_reg(reg_X, reg_Y);
  }
  else if (op == '[') {
    // Same as pointer addition + dereference of the result.
    codegen_binop_add(left_type, right_type); // Compute the address resulting from the addition
    left_type = arith_value_type('+', left_type, right_type); // The pointer type
    // Aggregates are represented by their address, so the address is the value
    if (!is_aggregate_type(dereference_type(left_type))) {
      load_mem_location(reg_X, reg_X, 0, ref_type_width(left_type), is_signed_numeric_type(dereference_type(left_type)));
    }
  } else {
    dump_op(op);
    fatal_error("codegen_binop: unknown op");
  }

#ifdef SUPPORT_FULL_ARITHMETIC
  // Maintain the invariant that values are always correctly extended for their
  // type. Additive operators, multiplication and left shift can overflow the
  // result type into the high bits of the register, so it's re-extended. Also,
  // the value of a compound assignment is converted to the type of the assigned
  // location. Note the use of raw extend_reg calls, and not convert_reg, to
  // force the re-extension.
  if (left_is_numeric && right_is_numeric) {
    if (op == '+' || op == '-' || op == '*') {
      if (type_width(common_type, false, false) < WORD_SIZE)
        extend_reg(reg_X, type_width(common_type, false, false), is_signed_numeric_type(common_type));
    } else if (op == LSHIFT) {
      left_type = integer_promote(left_type);
      if (type_width(left_type, false, false) < WORD_SIZE)
        extend_reg(reg_X, type_width(left_type, false, false), is_signed_numeric_type(left_type));
    } else if (op == PLUS_EQ || op == MINUS_EQ || op == STAR_EQ || op == LSHIFT_EQ
            || op == PLUS_PLUS_PRE || op == PLUS_PLUS_POST
            || op == MINUS_MINUS_PRE || op == MINUS_MINUS_POST) {
      // extend to assigned type
      if (type_width(left_type, false, false) < WORD_SIZE)
        extend_reg(reg_X, type_width(left_type, false, false), is_signed_numeric_type(left_type));
    } else if (op == SLASH_EQ || op == PERCENT_EQ
            || op == AMP_EQ || op == BAR_EQ || op == CARET_EQ) {
      convert_reg(reg_X, common_type, left_type);
    } else if (op == RSHIFT_EQ) {
      convert_reg(reg_X, integer_promote(left_type), left_type);
    }
  }
#endif // SUPPORT_FULL_ARITHMETIC

  stack_push(reg_X);
}

void codegen_rvalue(ast node);
void codegen_rvalue_and_cmp_0(int cond, int lbl, ast node);
void codegen_lvalue(ast node);
void codegen_statement(ast node);

#ifdef SUPPORT_EMULATED_INT64
// =============================== Emulated Int64 ==============================
//
// On 32-bit hosts, 64-bit integers are emulated by a pair of 32-bit words, with
// the low word on the stack top and the high word below it, corresponding to
// the memory layout of 64-bit integers in little-endian architectures. The code
// generator emits calls to runtime functions to perform 64-bit arithmetic and
// comparison operations, and the runtime library (arith64.c) emulates them with
// 32-bit operations.
//
// Because 64-bit values are represented as structures, the calls reuse the
// struct-return machinery: arguments are copied by value, the result comes back
// through a hidden pointer, and temporaries live until the end of the
// expression unless they are explicitly freed by the code generator (using
// reset_stack_to).
//
// Unlike for the "native" integer types, where values are always properly
// {signed/zero}-extended to the word size in registers and on the stack,
// operations between native integer types and emulated 64-bit integers require
// explicit narrowing/widening/truthiness conversions to translate from one
// representation to the other.
//
// These conversions (widen, narrow, truthiness) are simple enough to emit
// inline instead, simplifying the code generation as they avoid the need for
// temporary buffers and the associated lifetime management.
//
// narrowed_rvalue_type is used to determine the type of an rvalue expression
// after applying the narrowing conversion, and is a no-op when emulated 64-bit
// integers are not supported. Used to indicate to codegen_binop the type of the
// operands after applying the narrowing conversion.
//
// =============================================================================

void codegen_int64_compound_assignment(int op, ast lhs, ast rhs);
void codegen_int64_case_eq(ast case_expr);
int int64_runtime_binding(char *name);
void codegen_int64_widen_scalar(ast node);
void codegen_int64_value(ast node);
void codegen_int64_call(int binding, ast arg0, ast arg1, int result_words);
void codegen_int64_narrow(ast node, ast target_type);
void codegen_int64_truthy(ast node);
char *int64_resolve_binop_name(int op, ast left_type, ast right_type);
void codegen_int64_binop(ast node, ast child0, ast child1);
void codegen_int64_unary(int op, ast child0);
void codegen_int64_literal(ast node);
ast narrowed_rvalue_type(ast node);

#else

#define narrowed_rvalue_type(node) value_type(node)

#endif

// =========================== rvalue code generation ==========================
//
// In the minimal version of pnut-exe, rvalues are compiled naively, without any
// implicit conversions. This is enough to bootstrap pnut and bootstrap-friendly
// TCC, but is not C99 compliant.
//
// To support proper C99 semantics, the code generator must apply the integer
// promotions and the usual arithmetic conversions to rvalues, and convert them
// to the representation of their target type when needed. This is enabled with
// the SUPPORT_FULL_ARITHMETIC flag.
//
// In parallel, pnut-exe supports functions returning structures/unions, which
// requires allocating temporaries for the return value. The lifetime of these
// temporaries is managed by the code generator, which frees them when they are
// no longer needed.
//
// As a result, we define two functions that abstract over the different
// versions of codegen_rvalue, which are defined differently depending on the
// compilation flags:
//  - codegen_rvalue_coerced:
//      Evaluate an rvalue expression and convert the result to the
//      representation of target_type. Temporaries that are allocated are not
//      dropped, and must be freed by the caller or are freed when the
//      expression is consumed.
//
//  - codegen_rvalue_coerced_no_temps:
//      Evaluate an rvalue expression and convert the result to the
//      representation of target_type. Enforces that any temporaries allocated
//      during evaluation must be freed, fails if the expression returns an
//      array backed by a temporary.
//
// =============================================================================

#ifdef SUPPORT_EMULATED_INT64
// Compile node as rvalue, converting result to target type.
// target_type == 0 (varargs/unknown parameter) means no coercion.
void codegen_rvalue_coerced(ast node, ast target_type) {
  ast type = value_type(node);
  target_type = TERNARY(target_type == 0, type, target_type);
#ifdef SUPPORT_EMULATED_INT64
  if (is_int64_type(target_type) && !is_int64_type(type)) { // 64-bit widening
    codegen_int64_widen_scalar(node);
    stack_push(reg_SP);
    return;
  }
  else if (!is_aggregate_type(target_type) && is_int64_type(type)) { // 64-bit narrowing
    codegen_int64_narrow(node, target_type);
  } else // no 64-bit conversion needed
#endif
  {
    codegen_rvalue(node);
    if (conversion_needed(type, target_type)) {
      stack_pop(reg_X);
      convert_reg(reg_X, type, target_type);
      stack_push(reg_X);
    }
  }
}
#elif defined(SUPPORT_FULL_ARITHMETIC)
void codegen_rvalue_coerced(ast node, ast target_type) {
  ast type = value_type(node);
  target_type = TERNARY(target_type == 0, type, target_type);
  codegen_rvalue(node);
  if (conversion_needed(type, target_type)) {
    stack_pop(reg_X);
    convert_reg(reg_X, type, target_type);
    stack_push(reg_X);
  }
}
#else
#define codegen_rvalue_coerced(node, target_type) codegen_rvalue(node)
#endif

#ifdef SUPPORT_STRUCT_UNION

// Compile node as rvalue, converting result to target type. Free any
// temporaries allocated during evaluation, leaving the value directly on top of
// the stack.
// target_type == 0 (varargs/unknown parameter) means no coercion.
void codegen_rvalue_coerced_no_temps(ast node, ast target_type) {
  ast src_type = value_type(node);
  int save_fs = cgc_fs;
  codegen_rvalue_coerced(node, target_type);
  if (cgc_fs != save_fs + 1) {
    if (is_aggregate_type(src_type)) {
      fatal_error("codegen_rvalue_coerced_no_temps: array value backed by a temporary is not supported in this context");
    }
    stack_pop(reg_X);
    reset_stack_to(save_fs);
    stack_push(reg_X);
  }
}

#else

#define codegen_rvalue_coerced_no_temps(node, target_type) codegen_rvalue_coerced(node, target_type)

#endif

#ifdef SUPPORT_STRUCT_UNION
// =============================== Struct return ===============================
//
// A call to a function returning a struct/union allocates a temporary buffer
// for the result of the call, which is returned by the callee through a hidden
// parameter. The value of the call expression is the buffer's address, a single
// word, so codegen keeps its "one value = one stack word" convention. Member
// access, struct assignment and by-value arguments all consume that address
// like any other aggregate lvalue. Temporaries are accounted for in cgc_fs, so
// locals and parameters (addressed relative to cgc_fs) are unaffected by them.
//
// A temporary may be used as part of a larger expression, meaning it must
// outlive the expression that created it. To simplify the code generator, we
// don't track the exact lifetime of temporaries (nor reuse them) and instead
// just let them accumulate on the stack until the end of the statement, where
// they are all freed at once by codegen_statement's stack cleanup.
//
// However, there are cases where a temporary must be freed before the end of
// the statement to keep the stack balanced:
//  - When evaluating a condition, the temporary never escapes the condition
//    expression, and would pile up on the stack during loops if not freed right
//    away.
//    Handled by `codegen_rvalue_and_cmp_0`.
//  - When evaluating a ternary operator arm, since the individual arms may
//    allocate a different number of temporaries while both must leave the same
//    stack shape at the join point.
//    Handled by `codegen_aggregate` for struct/union-typed ternaries, and
//    `codegen_rvalue_no_temps` for the other arms.
//  - When generating a function call, since the temporary would sit between the
//    arguments and/or function pointer (for indirect calls).
//    Handled by `codegen_rvalue_no_temps` for scalar arguments, and
//    `codegen_aggregate` for by-value struct/union arguments.
//  - When evaluating a scalar assignment or local variable initializer, since
//    temporary values would offset the destination address / the local
//    variable's SP-relative offset which are assumed to not change.
//    Handled by `codegen_rvalue_no_temps`.
//  - When evaluating the right operand of a binary operator, since
//    codegen_binop expects its two operand words to be adjacent on top of the
//    stack (the left operand's temporaries can stay buried below its value
//    word, so the left operand doesn't need this).
//    Handled by `codegen_rvalue_no_temps`.
//
// In those contexts, an array-typed expression backed by a temporary (e.g.
// f().arr with f returning a struct by value) cannot be handled: its value is
// a pointer into a temporary that cannot be dropped, moved nor copied (the
// pointer may alias a named object). `codegen_rvalue_no_temps` rejects it
// with a compilation error instead of miscompiling it.
//
// =============================================================================


// Evaluate an rvalue expression for contexts that require the value to be
// exactly one word on top of the stack with no temporaries left behind
// (binop operands, function arguments, ternary arms, scalar assignments and
// initializers): any temporaries allocated during evaluation are freed, the
// value word being preserved. Aggregate temporaries can't be dropped when the
// expression's value is a pointer into them, so array-typed expressions that
// allocate temporaries (e.g. f().arr with f returning a struct by value) are
// rejected instead of being miscompiled. Struct/union-valued expressions
// never reach this: these contexts route them through codegen_aggregate*.

void codegen_aggregate_into(int dst_reg, int dst_offset, ast node, int width, ast target_type) {
  int save_fs = cgc_fs;
  if (dst_reg != reg_SP) {
    // Save reg in case codegen_rvalue clobbers it
    stack_push(dst_reg);
  }
  codegen_rvalue_coerced(node, target_type);
  stack_pop(reg_X); // source aggregate address
  if (dst_reg == reg_SP) {
    // Account for temporaries allocated during evaluation
    dst_offset += (cgc_fs - save_fs) * WORD_SIZE;
  } else {
    // Reload destination register, saved below the temporaries
    stack_load(dst_reg, (save_fs + 1));
  }
  copy_obj(dst_reg, dst_offset, reg_X, 0, width);
  reset_stack_to(save_fs);
}

// Evaluate an aggregate rvalue and place it directly on top of the stack,
// dropping any temporaries allocated during evaluation. The buffer is
// allocated before evaluating the expression so that the value is copied
// directly into place, below the temporaries (in which the value may live,
// hence the copy-then-drop order).
void codegen_aggregate(ast node, ast target_type) {
  int size_word = type_width(target_type, true, true) / WORD_SIZE;

#ifdef SUPPORT_EMULATED_INT64
  // Optimization: 64-bit values (including widened scalars) are materialized
  // directly into place instead of being copied from a temporary buffer.
  if (is_int64_type(target_type)) {
    codegen_int64_value(node);
    return;
  }
#endif


  stack_grow(size_word);
  grow_fs(size_word);
  codegen_aggregate_into(reg_SP, 0, node, size_word * WORD_SIZE, target_type);
}

#endif // SUPPORT_STRUCT_UNION

// Evaluate an rvalue for use as a function argument, coercing it to the
// parameter's declared type if needed.
void codegen_param(ast param, ast target_type) {
  if (target_type == 0) target_type = value_type(param);

#ifdef SUPPORT_STRUCT_UNION
  if (is_struct_like(target_type)) {
    // Aggregate values (structs/unions and 64-bit values, including scalars
    // widened to a 64-bit parameter) are passed by value
    codegen_aggregate(param, target_type);
  } else
#endif
  {
    // Scalars, including a 64-bit value narrowed to a scalar parameter: keep
    // the argument words contiguous, flushing any leftover temporaries below
    // them on the stack.
    codegen_rvalue_coerced_no_temps(param, target_type);
  }
}

// Evaluate the call arguments, converting the them to their expected types.
#ifdef SAFE_MODE
void codegen_params(ast params, ast params_type, bool allow_extra_params) {
#else
void codegen_params(ast params, ast params_type) {
#endif

  ast param_type;

  if (params != 0) {
#ifdef SAFE_MODE
    if (!allow_extra_params && params_type == 0) {
      fatal_error("codegen_params: Function expects less parameters than provided");
    }
#endif

    param_type = 0;
    if (params_type != 0) {
      param_type = get_child_(DECL, car(params_type), 1);
      params_type = tail(params_type);
    }

#ifdef SAFE_MODE
    codegen_params(tail(params), params_type, allow_extra_params);
#else
    codegen_params(tail(params), params_type);
#endif
    codegen_param(car(params), param_type);
  }
#ifdef SAFE_MODE
  else if (params_type != 0) {
    fatal_error("codegen_params: Function expects more parameters than provided");
  }
#endif
}

void emit_function_call(ast fun, int binding) {
  // Generate a fast path for direct calls
  if (binding != 0) {
#ifdef ONE_PASS_GENERATOR
    // When compiling in one pass mode, forward jumps must go through the jump table
    if (is_label_defined(fun_binding_lbl(binding))) {
      call(fun_binding_lbl(binding));
    } else {
      mov_reg_mem(reg_X, reg_glo, fun_binding_glo_entry(binding));
#ifdef SAFE_MODE
      // In safe mode, we check that the indirect call location is initialized
      mov_reg_imm(reg_Y, 0);
      int good_lbl = alloc_label(0);
      // Check if reg_X == 0 and call debug_interrupt otherwise
      jump_cond_reg_reg(NE, good_lbl, reg_X, reg_Y);
      debug_interrupt();
      def_label(good_lbl);
#endif
      call_reg(reg_X);
    }
#else
    call(fun_binding_lbl(binding));
#endif
  } else {
    // Otherwise we go through the function pointer. Temporaries are flushed
    // because they would sit between the arguments and the callee's frame,
    // breaking the callee's SP-relative parameter addressing.
    codegen_rvalue_coerced_no_temps(fun, 0);
    stack_pop(reg_X);
    call_reg(reg_X);
  }
}

#ifdef SUPPORT_STRUCT_UNION
// Complete a function call whose argument words are already on the stack.
// Setup the hidden result-buffer pointer for aggregate returns, emit the call,
// then do the stack cleanup.
void codegen_call_finish(ast fun, int binding, int args_fs, int buf_words) {
  // Push the buffer address as the hidden first argument (pushed last)
  if (buf_words != 0) stack_push_address_of(args_fs);

  emit_function_call(fun, binding);
  reset_stack_to(args_fs);
  // After popping the arguments, the result buffer is on top of the stack.
  if (buf_words != 0) mov_reg_reg(reg_X, reg_SP);
}
#endif

void codegen_call(ast node) {
  ast fun = get_child_('(', node, 0);
  ast params = get_child_('(', node, 1);
  ast type = value_type(fun);
  int save_fs = cgc_fs;
  int binding = 0;

#ifdef SUPPORT_STRUCT_UNION
  int buf_words = 0;
  ast fun_return_type = function_return_type(value_type(fun));

  if (is_struct_like(fun_return_type)) {
    // The function returns a struct/union: allocate the buffer in which the
    // callee will write the result. The buffer's address is passed as a
    // hidden argument, and is also the value of the call expression. The
    // buffer is freed at the end of the full expression.
    buf_words = type_width(fun_return_type, true, true) / WORD_SIZE;
    stack_grow(buf_words);
    save_fs = grow_fs(buf_words); // Keep buffer alive after call
  }
#endif

  // Check if the function is a direct call, find the binding if it is
  if (get_op(fun) == IDENTIFIER) {
    #ifdef ENABLE_PNUT_INLINE_INTERRUPT
    if (get_val_(IDENTIFIER, fun) == intern_str("PNUT_INLINE_INTERRUPT")) {
      debug_interrupt();
      stack_push(reg_X); // Dummy push to keep the stack balanced
      return;
    }
    #endif
    binding = resolve_identifier(get_val_(IDENTIFIER, fun));
    if (binding_kind(binding) != BINDING_FUN) binding = 0;
  }

  // Declared parameter types. codegen_params pairs each argument with its type
  // and converts it to that type if needed.
#ifdef SAFE_MODE
  if (!is_function_type(type)) {
    dump_node(type);
    fatal_error("Not a function or function pointer");
  }
#endif
  if (get_op(type) == '*') type = get_child_('*', type, 1); // dereference function pointer

#ifdef SAFE_MODE
  // allow_extra_params is true if the function is called indirectly or is variadic
  bool allow_extra_params = binding == 0;
  if (get_child_('(', type, 2)) allow_extra_params = true;
  codegen_params(params, get_child_opt_('(', LIST, type, 1), allow_extra_params);
#else
  codegen_params(params, get_child_opt_('(', LIST, type, 1));
#endif

#ifdef SUPPORT_STRUCT_UNION
  codegen_call_finish(fun, binding, save_fs, buf_words);
#else
  emit_function_call(fun, binding);
  reset_stack_to(save_fs);
#endif

  stack_push(reg_X);
}

// Ternary expression. Each arm leaves its value on the stack.
void codegen_ternary(ast node) {
  int lbl1 = alloc_label(0); // false label
  int lbl2 = alloc_label(0); // end label
  int save_fs = cgc_fs;
#if defined(SUPPORT_FULL_ARITHMETIC) || defined(SUPPORT_STRUCT_UNION)
  ast type = value_type(node);
#endif
#ifdef SUPPORT_STRUCT_UNION

  if (is_struct_like(type)) {
    codegen_rvalue_and_cmp_0(EQ, lbl1, get_child_('?', node, 0));
    codegen_aggregate(get_child_('?', node, 1), type); // value when true
    jump(lbl2);                                        // jump to end
    def_label(lbl1);                                   // false label
    cgc_fs = save_fs;                                  // reset fs for false arm
    codegen_aggregate(get_child_('?', node, 2), type); // value when false
    def_label(lbl2);                                   // end label
    stack_push(reg_SP);                                // agg buffer is on top of the stack
  } else
#endif
  {
    codegen_rvalue_and_cmp_0(EQ, lbl1, get_child_('?', node, 0));
    codegen_rvalue_coerced_no_temps(get_child_('?', node, 1), type); // value when true
    cgc_fs = save_fs;                                        // reset fs for false arm
    jump(lbl2);                                              // jump to end
    def_label(lbl1);                                         // false label
    codegen_rvalue_coerced_no_temps(get_child_('?', node, 2), type); // value when false
    def_label(lbl2);                                         // end label
  }
}

#ifdef SUPPORT_GOTO

void codegen_goto(ast node) {
  ast label_ident = get_val_(IDENTIFIER, get_child__(GOTO_KW, IDENTIFIER, node, 0));

  int binding = cgc_lookup_goto_label(label_ident, cgc_locals_fun);
  int goto_lbl;

  if (binding == 0) {
    goto_lbl = alloc_goto_label();
    cgc_add_goto_label(label_ident, goto_lbl);
    binding = cgc_locals_fun;
  }

  jump_to_goto_label(goto_binding_lbl(binding));
}

#endif // SUPPORT_GOTO

// Return the width of the lvalue
void codegen_lvalue(ast node) {
  int op = get_op(node);
  int nb_children = get_nb_children(node);
  int binding;
  ast type;
  ast child0, child1;

  if (nb_children >= 1) child0 = get_child(node, 0);
  if (nb_children >= 2) child1 = get_child(node, 1);

  if (nb_children == 0) {
    if (op == IDENTIFIER) {
      binding = resolve_identifier(get_val_(IDENTIFIER, node));
      switch (binding_kind(binding)) {
        case BINDING_PARAM_LOCAL:
        case BINDING_VAR_LOCAL:
          stack_push_address_of(var_binding_offset(binding));
          break;
        case BINDING_VAR_GLOBAL:
          mov_reg_imm(reg_X, var_binding_offset(binding));
          add_reg_reg(reg_X, reg_glo);
          stack_push(reg_X);
          break;
        case BINDING_FUN:
          // Function pointers are stored in the forward jump table
#ifdef ONE_PASS_GENERATOR
          mov_reg_mem(reg_X, reg_glo, fun_binding_glo_entry(binding));
#else
          mov_reg_lbl(reg_X, fun_binding_lbl(binding));
#endif
          stack_push(reg_X);
          break;
        default:
          fatal_error("codegen_lvalue: identifier not found");
          break;
      }
    } else {
#ifdef SUPPORT_EMULATED_INT64
  if (is_int64_type(value_type(node))) {
    codegen_rvalue(node);
    return; // codegen_rvalue already accounted for the pushed address
  }
#endif
      dump_node(node);
      fatal_error("codegen_lvalue: unexpected operator");
    }

  } else if (nb_children == 1) {

    if (op == '*') {
      codegen_rvalue(child0);
    } else {
#ifdef SUPPORT_EMULATED_INT64
  if (is_int64_type(value_type(node))) {
    codegen_rvalue(node);
    return; // codegen_rvalue already accounted for the pushed address
  }
#endif
      dump_node(node);
      fatal_error("codegen_lvalue: unexpected operator");
    }

  } else if (nb_children == 2) {

    if (op == '[') {
      type = value_type(child0);
      codegen_rvalue(child0);
      codegen_rvalue(child1);
      codegen_binop('+', type, value_type(child1));
    }
#ifdef SUPPORT_STRUCT_UNION
    else if (op == '.') {
      type = value_type(child0);
      if (is_struct_or_union_type(type)) {
        codegen_lvalue(child0);
        stack_pop(reg_X);
        // union members are at the same offset: 0
        if (get_op(type) == STRUCT_KW) {
          add_reg_imm(reg_X, struct_member_offset(type, child1));
        }
        stack_push(reg_X);
      } else {
        fatal_error("codegen_lvalue: . operator on non-struct type");
      }
    } else if (op == ARROW) {
      // Same as '.', but type must be a pointer
      type = value_type(child0);
      if (get_op(type) == '*' && is_struct_or_union_type(get_child_('*', type, 1))) {
        type = get_child_('*', type, 1);
        codegen_rvalue(child0);
        stack_pop(reg_X);
        // union members are at the same offset: 0
        if (get_op(type) == STRUCT_KW) {
          add_reg_imm(reg_X, struct_member_offset(type, child1));
        }
        stack_push(reg_X);
      } else {
        fatal_error("codegen_lvalue: -> operator on non-struct pointer type");
      }
    }
#endif // SUPPORT_STRUCT_UNION
    else if (op == CAST) {
      codegen_lvalue(child1);
    } else {
#ifdef SUPPORT_EMULATED_INT64
  if (is_int64_type(value_type(node))) {
    codegen_rvalue(node);
    return; // codegen_rvalue already accounted for the pushed address
  }
#endif
      dump_node(node);
      fatal_error("codegen_lvalue: unexpected operator");
    }

  } else {
#ifdef SUPPORT_EMULATED_INT64
  if (is_int64_type(value_type(node))) {
    codegen_rvalue(node);
    return; // codegen_rvalue already accounted for the pushed address
  }
#endif
    dump_node(node);
    fatal_error("codegen_lvalue: unexpected operator");
  }
}

void codegen_string(char *string_start, char *string_end) {
  int lbl = alloc_label(0);

  call(lbl);
  grow_fs(1); // Account for the value pushed by the call

  while (string_start != string_end) {
    emit_i8(*string_start);
    string_start += 1;
  }

  emit_i8(0);

  def_label(lbl);
}

// Generates code for an arithmetic assignment operator (+=, -=, *=, /=, %=, &=,
// |=, ^=, <<=, >>=) and ++/-- (pre/post), leaving the result on the stack.
void codegen_compound_assignment(int op, ast child0, ast child1) {
  ast left_type = value_type(child0);
  int left_width;

#ifdef SUPPORT_EMULATED_INT64
  // A narrower lvalue with a 64-bit rhs is handled below by narrowing the rhs
  if (is_int64_type(left_type)) {
    codegen_int64_compound_assignment(op, child0, child1);
    return;
  }
#endif

  left_width = type_width(left_type, true, false);
  codegen_lvalue(child0);
  // Copy the initial value and place it at the bottom of the stack
  stack_pop(reg_Y); // destination address
  load_mem_location(reg_X, reg_Y, 0, left_width, is_signed_numeric_type(left_type));
  stack_push(reg_X); // initial value / result slot
  stack_push(reg_Y); // destination address, kept adjacent to the value
  stack_push(reg_X); // current value of the lvalue to be modified

  codegen_rvalue_coerced_no_temps(child1, narrowed_rvalue_type(child1));
  codegen_binop(op, left_type, narrowed_rvalue_type(child1));

  // Stack layout at this point:
  //   top:       binop result
  //   top-1:     destination address of the lvalue
  //   top-2:     initial value of the modified lvalue

  // Write the result back to the lvalue's destination address
  stack_pop(reg_X);
  stack_pop(reg_Y);
  write_mem_location(reg_Y, 0, reg_X, left_width);

  if (op != MINUS_MINUS_POST && op != PLUS_PLUS_POST) {
    // Overwrite the result slot for operations that return the new value
    stack_pop(reg_Y);
    stack_push(reg_X);
  }
}

#ifdef SUPPORT_EMULATED_INT64

// A 64-bit value occupies this many operand-stack words (32-bit hosts only)
#define INT64_WORDS 2

// The binding of a 64-bit runtime function, looked up by name.
int int64_runtime_binding(char *name) {
  int binding = cgc_lookup_fun(intern_str(name), cgc_globals);
  if (binding == 0) {
    dump_string("64-bit runtime function not found: ", name);
    fatal_error("missing 64-bit runtime; compile with -rt <path-to-arith64.c>");
  }
  return binding;
}

// The runtime function implementing a operator on 64-bit operands.
char *int64_resolve_binop_name(int op, ast left_type, ast right_type) {
  ast comp_common_type = usual_arith_conv(left_type, right_type);
  ast arith_common_type = arith_value_type(op, left_type, right_type);

  switch (op) {
    case '+':     case PLUS_EQ:     return "add_i64";
    case '-':     case MINUS_EQ:    return "sub_i64";
    case '*':     case STAR_EQ:     return "mul_i64";
    case '&':     case AMP_EQ:      return "and_i64";
    case '|':     case BAR_EQ:      return "or_i64";
    case '^':     case CARET_EQ:    return "xor_i64";
    case LSHIFT:  case LSHIFT_EQ:   return "shl_i64";
    case '/':     case SLASH_EQ:    return TERNARY(is_signed_numeric_type(arith_common_type), "div_i64", "div_u64");
    case '%':     case PERCENT_EQ:  return TERNARY(is_signed_numeric_type(arith_common_type), "rem_i64", "rem_u64");
    case RSHIFT:  case RSHIFT_EQ:   return TERNARY(is_signed_numeric_type(arith_common_type), "shr_i64", "shr_u64");
    case '<':                       return TERNARY(is_signed_numeric_type(comp_common_type), "lt_i64", "lt_u64");
    case '>':                       return TERNARY(is_signed_numeric_type(comp_common_type), "gt_i64", "gt_u64");
    case LT_EQ:                     return TERNARY(is_signed_numeric_type(comp_common_type), "le_i64", "le_u64");
    case GT_EQ:                     return TERNARY(is_signed_numeric_type(comp_common_type), "ge_i64", "ge_u64");
    case EQ_EQ:                     return "eq_i64";
    case EXCL_EQ:                   return "ne_i64";
    default:                        {
      fatal_error("int64_resolve_binop_name: unexpected operator");
      return 0;
    }
  }
}

// Widen a scalar to a 64-bit value.
// Leaves the 64-bit value on the stack as an 8-byte buffer.
void codegen_int64_widen_scalar(ast node) {
  int save_fs = cgc_fs;
  bool is_signed = is_signed_numeric_type(value_type(node));
  codegen_rvalue(node);
  stack_pop(reg_X);                       // lo = the source scalar
  reset_stack_to(save_fs);                // drop the temporaries if any
  if (is_signed) {
    mov_reg_reg(reg_Z, reg_X);            // preserve lo (sar also clobbers reg_Y/CX)
    mov_reg_imm(reg_Y, WORD_SIZE * 8 - 1);
    sar_reg_reg(reg_Z, reg_Y);            // hi = sign fill
  } else {
    xor_reg_reg(reg_Z, reg_Z);            // hi = 0
  }
  stack_push(reg_Z);                      // push buffer.hi
  stack_push(reg_X);                      // push buffer.lo
}

// Push a integer (scalar or 64-bit) value onto the stack as 64-bit integer.
void codegen_int64_value(ast node) {
  int save_fs;
  if (!is_int64_type(value_type(node))) {
    codegen_int64_widen_scalar(node);
    return;
  }
  save_fs = cgc_fs;
  codegen_rvalue(node);
  stack_pop(reg_X);                         // the value is the buffer's address
  if (cgc_fs != save_fs + INT64_WORDS) {    // drop temps if any, otherwise the buffer is already on top of the stack
    mov_reg_mem(reg_Z, reg_X, WORD_SIZE);   // hi
    mov_reg_mem(reg_X, reg_X, 0);           // lo
    reset_stack_to(save_fs);                // drop the temporaries and the source buffer
    stack_push(reg_Z);                      // push buffer.hi
    stack_push(reg_X);                      // push buffer.lo
  }
}

// Truncate a 64-bit value to a scalar value
void codegen_int64_narrow(ast node, ast target_type) {
  int save_fs = cgc_fs;
  bool is_signed = is_signed_numeric_type(target_type);
  codegen_rvalue(node);         // the value is the buffer's address
  stack_pop(reg_X);             // pop the buffer's address
  mov_reg_mem(reg_X, reg_X, 0); // load lo word
  reset_stack_to(save_fs);      // drop the temporaries and the buffer
  convert_reg(reg_X, TERNARY(is_signed, int_type, uint_type), target_type); // convert to the target scalar type
  stack_push(reg_X);            // push the scalar value
}

// Reduce a 64-bit value to (lo | hi): zero iff the value is zero.
// Note that the result is not normalized to 0/1.
void codegen_int64_truthy(ast node) {
  int save_fs = cgc_fs;
  codegen_rvalue(node);                 // the value is the buffer's address
  stack_pop(reg_X);                     // pop the buffer's address
  mov_reg_mem(reg_Y, reg_X, WORD_SIZE); // load hi word
  mov_reg_mem(reg_X, reg_X, 0);         // load lo word
  or_reg_reg(reg_X, reg_Y);             // lo | hi
  reset_stack_to(save_fs);              // drop the temporaries and the buffer
  stack_push(reg_X);                    // push the result
}

// Emit a direct call to a 64-bit runtime function, leaving its result on the
// operand stack. 64-bit runtime functions take their arguments by value and
// return their value through a hidden pointer to an 8-byte buffer.
void codegen_int64_call(int binding, ast arg0, ast arg1, int result_words) {
  int args_fs = cgc_fs + result_words; // account for the result buffer if any
  if (result_words != 0) { stack_grow(result_words); grow_fs(result_words); }
  if (arg1) codegen_param(arg1, ulong_type);
  if (arg0) codegen_param(arg0, ulong_type);
  codegen_call_finish(0, binding, args_fs, result_words);
  stack_push(reg_X); // the call's result is the expression's value
}

// Emit the code for 64-bit binary operations. The value of an arithmetic result
// is the address of its 8-byte buffer and a comparison yields a plain int.
void codegen_int64_binop(ast node, ast child0, ast child1) {
  int op = get_op(node);
  ast node_type = value_type(node);
  int binding = int64_runtime_binding(int64_resolve_binop_name(op, value_type(child0), value_type(child1)));
  codegen_int64_call(binding, child0, child1, TERNARY(is_int64_type(node_type), INT64_WORDS, 0));
}

// Push a 64-bit literal onto the stack as an 8-byte buffer ({lo, hi}) with its
// address on top of the stack.
void codegen_int64_literal(ast node) {
  int val = get_val(node);
  int lo, hi;
  if (is_large_int(val)) { lo = large_int_lo(val); hi = large_int_hi(val); }
  else                   { lo = -val; hi = 0; }         // small LL literal, fits 32 bits (non-negative)
  mov_reg_imm(reg_X, hi);
  stack_push(reg_X);                                    // push buffer.hi
  if (lo != hi) mov_reg_imm(reg_X, lo);                 // reg_X already equals lo if lo == hi
  stack_push(reg_X);                                    // push buffer.lo
  stack_push(reg_SP);                                   // push buffer address
}

// Type of a value after it has been narrowed to a scalar type. This is the type
// that should be used for the following convert_reg / codegen_binop, so that
// the right source type is fed to the conversion or binary operation.
ast narrowed_rvalue_type(ast node) {
  ast type = value_type(node);
  if (is_int64_type(type)) {
    return TERNARY(is_signed_numeric_type(type), int_type, uint_type);
  } else {
    return type;
  }
}

// Push a copy of the 8-byte value whose address is stored at slot_fs.
void codegen_int64_push_copy(int slot_fs) {
  stack_grow(INT64_WORDS); grow_fs(INT64_WORDS);
  stack_load(reg_X, slot_fs); // reg_X = &value
  copy_obj(reg_SP, 0, reg_X, 0, INT64_WORDS * WORD_SIZE);
}

// 64-bit variant of codegen_compound_assignment, when the lvalue is 64-bit.
void codegen_int64_compound_assignment(int op, ast lhs, ast rhs) {
  int binding;
  int lhs_fs;
  int args_fs;
  bool want_old_value = op == MINUS_MINUS_POST || op == PLUS_PLUS_POST;
  op = TERNARY(op == MINUS_MINUS_POST || op == MINUS_MINUS_PRE, '-', op);
  op = TERNARY(op == PLUS_PLUS_POST || op == PLUS_PLUS_PRE, '+', op);
  binding = int64_runtime_binding(int64_resolve_binop_name(op, value_type(lhs), value_type(rhs)));

  codegen_lvalue(lhs); // top of stack = &lhs
  lhs_fs = cgc_fs;

  // Save the pre-update value before the original value is overwritten
  if (want_old_value) {
    codegen_int64_push_copy(lhs_fs);
    stack_push(reg_SP);
  }

  args_fs = cgc_fs;
  codegen_param(rhs, ulong_type);  // push rhs
  codegen_int64_push_copy(lhs_fs); // push copy of lhs
  stack_load(reg_X, lhs_fs);       // return buffer = &lhs
  stack_push(reg_X);               // hidden buffer pointer (pushed last) = &lhs
  emit_function_call(0, binding);
  reset_stack_to(args_fs);         // pop arguments; &lhs (or &saved_copy) is on top
}

// Compare a switch's 64-bit operand against a case expression, reg_X = 0 or 1.
void codegen_int64_case_eq(ast case_expr) {
  int op_fs = cgc_fs; // the 64-bit switch operand address is on top of stack

  codegen_int64_push_copy(op_fs); // Copy of the switch operand
  codegen_param(case_expr, ulong_type); // Then the case expression as value (widened to 64-bit if needed)
  codegen_call_finish(0, int64_runtime_binding("eq_i64"), op_fs, 0); // Call eq_i64
}

#endif // SUPPORT_EMULATED_INT64

void codegen_integer(ast node) {
#ifdef SUPPORT_EMULATED_INT64
  if (get_op(value_type(node)) == LONG_KW) {
    codegen_int64_literal(node);
  } else
#endif
  {
#ifdef SUPPORT_64_BIT_LITERALS
      mov_reg_large_imm(reg_X, get_val(node));
#else
      mov_reg_imm(reg_X, -get_val(node));
#endif
      stack_push(reg_X);
  }
}

void codegen_rvalue(ast node) {
  int op = get_op(node);
  int nb_children = get_nb_children(node);
  int binding;
  int lbl1, lbl2;
  ast type;
  ast child0, child1;

  if (nb_children >= 1) child0 = get_child(node, 0);
  if (nb_children >= 2) child1 = get_child(node, 1);

  if (nb_children == 0) {
    if ( op == INTEGER
#ifdef PARSE_NUMERIC_LITERAL_WITH_BASE
      || op == INTEGER_HEX || op == INTEGER_OCT
#endif
#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
      || op == INTEGER_L || op == INTEGER_LL || op == INTEGER_U || op == INTEGER_UL || op == INTEGER_ULL
#endif
       ) {
      codegen_integer(node);
    } else if (op == CHARACTER) {
      mov_reg_imm(reg_X, get_val_(CHARACTER, node));
      stack_push(reg_X);
    } else if (op == IDENTIFIER) {
      binding = resolve_identifier(get_val_(IDENTIFIER, node));
      switch (binding_kind(binding)) {
        case BINDING_PARAM_LOCAL:
        case BINDING_VAR_LOCAL:
          // structs and unions locals and parameters are always on the stack,
          // so their value is their address.
          // Arrays are allocated on the stack only when they are local
          // variables, so their value is also their address.
          // Array parameters (see add_function_params) are passed as pointers,
          // so we dereference the stack value to get the value of the pointer.
          if (is_aggregate_type(var_binding_type(binding))) {
            stack_push_address_of(var_binding_offset(binding));
          } else {
            stack_dereference(reg_X, var_binding_offset(binding), type_width(var_binding_type(binding), false, false), is_signed_numeric_type(var_binding_type(binding)));
            stack_push(reg_X);
          }
          break;
        case BINDING_VAR_GLOBAL:
          // global arrays/structs/unions are also allocated in
          // memory, so their value is their address (no dereference)
          if (is_aggregate_type(var_binding_type(binding))) {
            mov_reg_reg(reg_X, reg_glo);
            add_reg_imm(reg_X, var_binding_offset(binding));
          } else {
            load_mem_location(reg_X, reg_glo, var_binding_offset(binding), type_width(var_binding_type(binding), false, false), is_signed_numeric_type(var_binding_type(binding)));
          }
          stack_push(reg_X);
          break;
        case BINDING_ENUM_CST:
#ifdef SUPPORT_64_BIT_LITERALS
          mov_reg_large_imm(reg_X, get_val(enum_binding_value(binding)));
#else
          mov_reg_imm(reg_X, -get_val_(INTEGER, enum_binding_value(binding)));
#endif
          stack_push(reg_X);
          break;

        case BINDING_FUN:
#ifdef ONE_PASS_GENERATOR
          mov_reg_mem(reg_X, reg_glo, fun_binding_glo_entry(binding));
#else
          mov_reg_lbl(reg_X, fun_binding_lbl(binding));
#endif
          stack_push(reg_X);
          break;

        default:
          dump_ident(get_val_(IDENTIFIER, node));
          fatal_error("codegen_rvalue: identifier not found");
          break;
      }
    } else if (op == STRING) {
      codegen_string(symbol_buf(get_val_(STRING, node)), symbol_buf_end(get_val_(STRING, node)));
    } else {
      dump_node(node);
      fatal_error("codegen_rvalue: unexpected operator");
    }

  } else if (nb_children == 1) {
    if (op == '*') {
      type = value_type(child0);
      codegen_rvalue(child0);
      if (is_function_type(type)) {
      } else if (is_pointer_type(type)) {
        // The value of an aggregate (struct/union/array) is its address, which
        // is already on the stack, so no load is needed. This also avoids an
        // oversized load_mem_location for objects wider than a word.
        if (!is_aggregate_type(dereference_type(type))) {
          stack_pop(reg_X);
          load_mem_location(reg_X, reg_X, 0, ref_type_width(type), is_signed_numeric_type(dereference_type(type)));
          stack_push(reg_X);
        }
      } else {
        fatal_error("codegen_rvalue: non-pointer is being dereferenced with *");
      }
    } else if (op == '+') {
      codegen_rvalue(child0);
    } else if (op == '-' || op == '~') {
#ifdef SUPPORT_EMULATED_INT64
      if (is_int64_type(value_type(child0))) {
        codegen_int64_call(
          int64_runtime_binding(TERNARY(op == '-', "neg_i64", "not_i64")),
          child0, 0, INT64_WORDS);
        return;
      }
#endif
      codegen_rvalue(child0);
      stack_pop(reg_Y);
      if (op == '-') {
        xor_reg_reg(reg_X, reg_X);
        sub_reg_reg(reg_X, reg_Y);
      } else {
        mov_reg_imm(reg_X, -1);
        xor_reg_reg(reg_X, reg_Y);
      }
#ifdef SUPPORT_FULL_ARITHMETIC
      // Negation can overflow the result type and complementing a zero-extended
      // value sets the high register bits, so the result is re-extended to keep
      // values correctly extended for their type.
      type = integer_promote(value_type(child0));
      if (is_numeric_type(type) && type_width(type, false, false) < WORD_SIZE) {
        extend_reg(reg_X, type_width(type, false, false), is_signed_numeric_type(type));
      }
#endif // SUPPORT_FULL_ARITHMETIC
      stack_push(reg_X);
    } else if (op == '!') {
      lbl1 = alloc_label(0);
      lbl2 = alloc_label(0);
      codegen_rvalue_and_cmp_0(EQ, lbl1, child0);
      // fall through => child0 != 0 => result = 0
      xor_reg_reg(reg_X, reg_X);
      jump(lbl2);
      def_label(lbl1); // child0 == 0 => result = 1
      mov_reg_imm(reg_X, 1);
      def_label(lbl2);
      stack_push(reg_X);
    } else if (op == MINUS_MINUS_POST || op == PLUS_PLUS_POST || op == MINUS_MINUS_PRE || op == PLUS_PLUS_PRE) {
      codegen_compound_assignment(op, child0, one_literal);
    } else if (op == '&') {
      codegen_lvalue(child0);
    }
#ifdef SUPPORT_SIZEOF
    else if (op == SIZEOF_KW) {
      if (get_op(child0) == DECL) {
        mov_reg_imm(reg_X, type_width(get_child_(DECL, child0, 1), true, false));
      } else {
        mov_reg_imm(reg_X, type_width(value_type(child0), true, false));
      }
      stack_push(reg_X);
    }
#endif // SUPPORT_SIZEOF
    else {
      dump_node(node);
      fatal_error("codegen_rvalue: unexpected operator");
    }

  } else if (nb_children == 2) {
    if (op == '+' || op == '-' || op == '*' || op == '/' || op == '%' || op == '&' || op == '|' || op == '^' || op == LSHIFT || op == RSHIFT || op == '<' || op == '>' || op == EQ_EQ || op == EXCL_EQ || op == LT_EQ || op == GT_EQ || op == '[') {
#ifdef SUPPORT_EMULATED_INT64
      // An arithmetic/comparison operator with a 64-bit operand and no pointers
      // requires 64-bit arithmetic, which is emulated through a runtime call.
      if (!is_pointer_type(value_type(child0)) && !is_pointer_type(value_type(child1))
          && (is_int64_type(value_type(child0)) || is_int64_type(value_type(child1)))) {
        codegen_int64_binop(node, child0, child1);
      } else
#endif
      {
        // codegen_binop expects the rhs value word to sit directly on top of
        // the lhs value word, so the rhs's temporaries are flushed.
        codegen_rvalue_coerced(child0, narrowed_rvalue_type(child0));
        codegen_rvalue_coerced_no_temps(child1, narrowed_rvalue_type(child1));
        codegen_binop(op, narrowed_rvalue_type(child0), narrowed_rvalue_type(child1));
      }
    } else if (op == ',') {
      codegen_rvalue(child0);
      stack_pop(reg_X); // discard the lhs value
      codegen_rvalue(child1);
    } else if (op == '=') {
      type = value_type(child0);
      codegen_lvalue(child0);
#ifdef SUPPORT_STRUCT_UNION
      if (is_struct_like(type)) {
        // Struct assignment, we copy the struct.
        stack_pop(reg_Y); // destination address
        codegen_aggregate_into(reg_Y, 0, child1, type_width(type, true, false), type);
      } else
#endif
      {
        codegen_rvalue_coerced_no_temps(child1, type); // so that the destination address is right below the value
        stack_pop(reg_X);
        stack_pop(reg_Y);
        write_mem_location(reg_Y, 0, reg_X, type_width(type, true, false));
      }
      stack_push(reg_X);
    } else if (op == AMP_EQ || op == BAR_EQ || op == CARET_EQ || op == LSHIFT_EQ || op == MINUS_EQ || op == PERCENT_EQ || op == PLUS_EQ || op == RSHIFT_EQ || op == SLASH_EQ || op == STAR_EQ) {
      codegen_compound_assignment(op, child0, child1);
    } else if (op == AMP_AMP || op == BAR_BAR) {
      lbl1 = alloc_label(0);
      lbl2 = alloc_label(0);
      // Jump to lbl1 as soon as an operand decides the result (== 0 for &&,
      // != 0 for ||), short-circuiting the rest. The result is normalized to
      // 0/1.
      codegen_rvalue_and_cmp_0(TERNARY(op == AMP_AMP, EQ, NE), lbl1, child0);
      codegen_rvalue_and_cmp_0(TERNARY(op == AMP_AMP, EQ, NE), lbl1, child1);
      // fall through, && => true, || => false
      mov_reg_imm(reg_X, TERNARY(op == AMP_AMP, 1, 0));
      jump(lbl2);
      def_label(lbl1);
      mov_reg_imm(reg_X, TERNARY(op == AMP_AMP, 0, 1)); // an operand decided
      def_label(lbl2);
      stack_push(reg_X);
    } else if (op == '(') {
      codegen_call(node);
    }
#ifdef SUPPORT_STRUCT_UNION
    else if (op == '.') {
      type = value_type(child0);
      if (is_struct_or_union_type(type)) {
        codegen_rvalue(child0); // for aggregates, the value is the address
        stack_pop(reg_Y);
        // union members are at the same offset: 0
        if (get_op(type) == STRUCT_KW) {
          add_reg_imm(reg_Y, struct_member_offset(type, child1));
        }
        type = get_child_(DECL, struct_member(type, child1), 1); // struct member type
        if (!is_aggregate_type(type)) {
          load_mem_location(reg_Y, reg_Y, 0, type_width(type, false, false), is_signed_numeric_type(type));
        }
        stack_push(reg_Y);
      } else {
        fatal_error("codegen_rvalue: . operator on non-struct type");
      }
    } else if (op == ARROW) {
      type = value_type(child0);
      if (get_op(type) == '*' && is_struct_or_union_type(get_child_('*', type, 1))) {
        type = get_child_('*', type, 1);
        codegen_rvalue(child0);
        stack_pop(reg_Y);
        // union members are at the same offset: 0
        if (get_op(type) == STRUCT_KW) {
          add_reg_imm(reg_Y, struct_member_offset(type, child1));
        }
        type = get_child_(DECL, struct_member(type, child1), 1); // struct member type
        if (!is_aggregate_type(type)) {
          load_mem_location(reg_Y, reg_Y, 0, type_width(type, false, false), is_signed_numeric_type(type));
        }
        stack_push(reg_Y);
      } else {
        fatal_error("codegen_rvalue: -> operator on non-struct pointer type");
      }
    }
#endif // SUPPORT_STRUCT_UNION
    else if (op == CAST) {
      codegen_rvalue_coerced(child1, get_child_(DECL, child0, 1));
    } else {
      fatal_error("codegen_rvalue: unknown rvalue with 2 children");
    }

  } else if (nb_children == 3) {

    if (op == '?') {
      codegen_ternary(node);
    } else {
      dump_node(node);
      fatal_error("codegen_rvalue: unexpected operator");
    }

  } else {
    dump_node(node);
    fatal_error("codegen_rvalue: unexpected operator");
  }
}

// Evaluate a condition expression and jump to lbl if the condition is true,
// fallthrough otherwise.
void codegen_rvalue_and_cmp_0(int cond, int lbl, ast node) {
#ifdef SUPPORT_STRUCT_UNION
  int save_fs = cgc_fs;
#endif
#ifdef SUPPORT_EMULATED_INT64
  // A 64-bit value used as a condition is reduced to an int truthiness word
  // (lo | hi) so the comparison-against-0 below operates on a scalar, not the
  // buffer address.
  if (is_int64_type(value_type(node))) {
    codegen_int64_truthy(node);
  } else
#endif
  codegen_rvalue(node);
  stack_pop(reg_X);
#ifdef SUPPORT_STRUCT_UNION
  // The node's value is immediately consumed by a conditional jump, so it can
  // be collected right away. This also ensures that temporaries don't pile up
  // during loops, where the condition is evaluated multiple times.
  reset_stack_to(save_fs);
#endif

  xor_reg_reg(reg_Y, reg_Y);
  jump_cond_reg_reg(cond, lbl, reg_X, reg_Y);
}

void handle_enum_struct_union_type_decl(ast type);

void codegen_enum(ast node) {
  ast name = get_child_opt_(ENUM_KW, IDENTIFIER, node, 1);
  ast cases = get_child_opt_(ENUM_KW, LIST, node, 2);
  ast cas;
  int binding;

  if (name != 0 && cases != 0) { // if enum has a name and members (not a reference to an existing type)
    binding = cgc_lookup_enum(get_val_(IDENTIFIER, name), cgc_globals);
    if (binding != 0) { fatal_error("codegen_enum: enum already declared"); }
    cgc_add_typedef(get_val_(IDENTIFIER, name), BINDING_TYPE_ENUM, node);
  }

  while (cases != 0) {
    cas = car_('=', cases);
    cgc_add_enum(get_val_(IDENTIFIER, get_child__('=', IDENTIFIER, cas, 0)), get_child_('=', cas, 1));
    cases = tail(cases);
  }
}

#ifdef SUPPORT_STRUCT_UNION

void codegen_struct_or_union(ast node, enum BINDING kind) {
  ast name = get_child(node, 1);
  ast members = get_child(node, 2);
  int binding;

  // if struct has a name and members (not a reference to an existing type)
  if (name != 0 && members != 0) {
    binding = cgc_lookup_binding_ident(kind, get_val_(IDENTIFIER, name), cgc_globals);
    if (binding != 0 && typedef_binding_type(binding) != node && get_child(typedef_binding_type(binding), 2) != members) {
      fatal_error("codegen_struct_or_union: struct/union already declared");
    }
    cgc_add_typedef(get_val_(IDENTIFIER, name), kind, node);
  }

  // Traverse the structure to find any other declarations.
  // This is not the right semantic because inner declarations are scoped to
  // this declaration, but it's probably good enough for TCC.
  while (members != 0) {
    handle_enum_struct_union_type_decl(get_child_(DECL, car_(DECL, members), 1));
    members = tail(members);
  }
}

#endif // SUPPORT_STRUCT_UNION

void handle_enum_struct_union_type_decl(ast type) {
  if (get_op(type) == ENUM_KW) {
    codegen_enum(type);
  }
#ifdef SUPPORT_STRUCT_UNION
  else if (get_op(type) == STRUCT_KW) {
    codegen_struct_or_union(type, BINDING_TYPE_STRUCT);
  } else if (get_op(type) == UNION_KW) {
    codegen_struct_or_union(type, BINDING_TYPE_UNION);
  }
#endif
  else if (get_op(type) == '*') {
    handle_enum_struct_union_type_decl(get_child_('*', type, 1));
  } else if (get_op(type) == '[') {
    handle_enum_struct_union_type_decl(get_child_('[', type, 0));
  }

  // If not an enum, struct, or union, do nothing
}

void codegen_initializer_string(int string_symbol, ast type, int base_reg, int offset) {
  char *string_start = symbol_buf(string_symbol);
  int i = 0;
  int str_len = symbol_len(string_symbol);
  int arr_len;

  // Only acceptable types are char[] or char*
  if (get_op(type) == '[' && get_op(get_child_('[', type, 0)) == CHAR_KW) {
    arr_len = get_child_('[', type, 1);
    if (str_len > arr_len) fatal_error("codegen_initializer: string initializer is too long for char[]");

    // Place the bytes of the string in the memory location allocated for the array
    for (; i < arr_len; i += 1) {
      mov_reg_imm(reg_X, TERNARY(i < str_len, string_start[i], 0));
      write_mem_location(base_reg, offset + i, reg_X, 1);
    }
  } else if (get_op(type) == '*' && get_op(get_child_('*', type, 1)) == CHAR_KW) {
    // Create the string and assign global variable to the pointer
    codegen_string(symbol_buf(string_symbol), symbol_buf_end(string_symbol));
    stack_pop(reg_X);
    mov_mem_reg(base_reg, offset, reg_X);
  } else {
    fatal_error("codegen_initializer: string initializer must be assigned to a char[] or char*");
  }
}

// Initialize a variable with an initializer
void codegen_initializer(bool local, ast init, ast type, int base_reg, int offset) {
#ifdef SUPPORT_COMPLEX_INITIALIZER
  ast members;
  ast inner_type;
  int arr_len;
  int inner_type_width;
#endif // SUPPORT_COMPLEX_INITIALIZER

  type = canonicalize_type(type);

  switch (get_op(init)) {
    case STRING:
      codegen_initializer_string(get_val_(STRING, init), type, base_reg, offset);
      break;

#ifdef SUPPORT_COMPLEX_INITIALIZER
    case INITIALIZER_LIST:
      init = get_child_(INITIALIZER_LIST, init, 0);
      // Acceptable types are:
      //  arrays
      //  structs
      //  union   (if the initializer list has only one element)
      //  scalars (if the initializer list has only one element)
      switch (get_op(type)) {
        case '[':
          inner_type = get_child_('[', type, 0);
          arr_len = get_child_('[', type, 1);
          inner_type_width = type_width(get_child_('[', type, 0), true, false);

          while (init != 0 && arr_len != 0) {
            codegen_initializer(local, car(init), inner_type, base_reg, offset);
            offset += inner_type_width;
            init = tail(init);
            arr_len -= 1; // decrement the number of elements left to initialize to make sure we don't overflow
          }

          if (init != 0) {
            fatal_error("codegen_initializer: too many elements in initializer list");
          }

          // If there are still elements to initialize, set them to 0.
          // If it's not a local variable, we don't need to initialize the
          // memory since the stack is zeroed during setup.
          if (local && arr_len > 0) initialize_memory(0, base_reg, offset, inner_type_width * arr_len);
          break;

#ifdef SUPPORT_STRUCT_UNION
        case STRUCT_KW:
          members = get_child_(STRUCT_KW, type, 2);
          while (init != 0 && members != 0) {
            inner_type = get_child_(DECL, car_(DECL, members), 1);
            codegen_initializer(local, car(init), inner_type, base_reg, offset);
            offset += type_width(inner_type, true, false);
            init = tail(init);
            members = tail(members);
          }

          // Initialize rest of the members to 0
          while (local && members != 0) {
            inner_type = get_child_(DECL, car_(DECL, members), 1);
            initialize_memory(0, base_reg, offset, type_width(inner_type, true, false));
            offset += type_width(inner_type, true, false);
            members = tail(members);
          }
          break;

        case UNION_KW:
          members = get_child_(STRUCT_KW, type, 2);
          if (tail(init) != 0) {
            fatal_error("codegen_initializer: union initializer list has more than one element");
          } else if (members == 0) {
            fatal_error("codegen_initializer: union has no members");
          }
          codegen_initializer(local, car(init), get_child_(DECL, car_(DECL, members), 1), base_reg, offset);
          break;
#endif // SUPPORT_STRUCT_UNION

        default:
          if (tail(init) != 0 // More than 1 element
           || get_op(car(init)) == INITIALIZER_LIST) { // Or nested initializer list
            fatal_error("codegen_initializer: scalar initializer list has more than one element");
          }
          // Single scalar/struct element wrapped in braces, same as a simple
          // scalar initializer.
          codegen_initializer(local, car(init), type, base_reg, offset);
          break;
      }

      break;

#endif // SUPPORT_COMPLEX_INITIALIZER

    default:
#ifdef SUPPORT_STRUCT_UNION
      if (is_struct_like(type)) {
        // Struct assignment, we copy the struct.
        codegen_aggregate_into(base_reg, offset, init, type_width(type, true, true), type);
      } else
#endif // SUPPORT_STRUCT_UNION
      if (get_op(type) != '[') {
        // The value is scalar (the type is neither an array nor a
        // struct/union/int64), so the temporaries do get flushed and the
        // SP-relative offset stays valid for the write below.
        codegen_rvalue_coerced_no_temps(init, type);
        stack_pop(reg_X);
        write_mem_location(base_reg, offset, reg_X, type_width(type, true, false));
      } else {
        fatal_error("codegen_initializer: cannot initialize array with scalar value");
      }
      break;
  }
}

#ifdef SUPPORT_COMPLEX_INITIALIZER

// Return size of initializer.
// If it's an initializer list, return the number of elements
// If it's a string, return the length of the string and delimiter.
int initializer_size(ast initializer) {
  int size = 0;

  switch (get_op(initializer)) {
    case INITIALIZER_LIST:
      initializer = get_child_(INITIALIZER_LIST, initializer, 0);
      while (initializer != 0) {
        size += 1;
        initializer = tail(initializer);
      }
      return size;

    case STRING:
      return symbol_len(get_val_(STRING, initializer)) + 1; // +1 for null terminator

    default:
      fatal_error("initializer_size: unknown initializer");
      return -1;
  }
}

void infer_array_length(ast type, ast init) {
  // Array declaration with no size
  if (get_op(type) == '[' && get_child_('[', type, 1) == 0) {
    if (init == 0) {
      fatal_error("Array declaration with no size must have an initializer");
    }
    set_child(type, 1, initializer_size(init));
  }
}

#else

#define infer_array_length(type, init)  // No-op

#endif

void codegen_glo_var_decl(ast node) {
  ast name = get_child__(DECL, IDENTIFIER, node, 0);
  ast type = get_child_(DECL, node, 1);
  ast init = get_child_(DECL, node, 2);
  int name_symbol = get_val_(IDENTIFIER, name);
  int binding = cgc_lookup_var(name_symbol, cgc_globals);

  if (get_op(type) == '(') {
    // Forward declaration
    binding = cgc_lookup_fun(name_symbol, cgc_globals);
    if (binding == 0) cgc_add_global_fun(name_symbol, alloc_label(symbol_buf(name_symbol)), type);

  } else {
    handle_enum_struct_union_type_decl(type);
    infer_array_length(type, init);

    if (binding == 0) {
      cgc_add_global(name_symbol, type_width(type, true, true), type, false);
      binding = cgc_globals;
    }

    if (init != 0) {
      START_INIT_BLOCK();
      codegen_initializer(false, init, type, reg_glo, var_binding_offset(binding));
      END_INIT_BLOCK();
    }
  }
}

// Compute the size of a local variable declaration in bytes
int compute_local_var_decl_size(ast type, ast init) {
  infer_array_length(type, init);

  if (is_aggregate_type(type)) { // Array/struct/union declaration
    return type_width(type, true, true);  // size in bytes (word aligned)
  } else {
    return WORD_SIZE;
  }
}

void codegen_local_var_decl(ast node) {
  ast name = get_child__(DECL, IDENTIFIER, node, 0);
  ast type = get_child_(DECL, node, 1);
  ast init = get_child_(DECL, node, 2);
  // For local variables, the smallest unit of memory is a word, so the size is in words
  int size = compute_local_var_decl_size(type, init) / WORD_SIZE;

  cgc_add_local_var(get_val_(IDENTIFIER, name), size, type);
  stack_grow(size); // Make room for the local variable

  if (init != 0) {
    // offset (cgc_fs - var_binding_offset(cgc_locals)) should be 0 since we just allocated the space
    codegen_initializer(true, init, type, reg_SP, 0);
  }
}

#ifdef SUPPORT_TYPE_SPECIFIERS

void codegen_static_local_var_decl(ast node) {
  ast name = get_child__(DECL, IDENTIFIER, node, 0);
  ast type = get_child_(DECL, node, 1);
  ast init = get_child_(DECL, node, 2);
  int size = compute_local_var_decl_size(type, init);
  int skip_init_lbl;

  cgc_add_global(get_val_(IDENTIFIER, name), size, type, true);

  if (init != 0) {
    // Skip over the initialization code that will run during program initialization
    skip_init_lbl = alloc_label("skip_init");
    jump(skip_init_lbl);
    START_INIT_BLOCK();
    codegen_initializer(false, init, type, reg_glo, var_binding_offset(cgc_locals));
    END_INIT_BLOCK();
    def_label(skip_init_lbl);
  }
}

#endif // SUPPORT_TYPE_SPECIFIERS

void codegen_local_var_decls(ast node) {
#ifdef SUPPORT_TYPE_SPECIFIERS
  bool is_static = false;

  switch (get_child_(DECLS, node, 1)) {
    // AUTO_KW and REGISTER_KW can simply be ignored.
    case STATIC_KW:
      is_static = true;
      break;
    case EXTERN_KW:
      fatal_error("Extern class specifier not supported");
      break;
  }
#endif // SUPPORT_TYPE_SPECIFIERS

  node = get_child__(DECLS, LIST, node, 0);
  while (node != 0) { // Multiple variable declarations
#ifdef SUPPORT_TYPE_SPECIFIERS
    if (is_static) {
      codegen_static_local_var_decl(car_(DECL, node));
    } else {
      codegen_local_var_decl(car_(DECL, node));
    }
#else
    codegen_local_var_decl(car_(DECL, node));
#endif // SUPPORT_TYPE_SPECIFIERS
    node = tail(node);
  }
}

void codegen_body(ast node) {
  ast stmt;

  while (node != 0) {
    stmt = get_child_('{', node, 0);
    if (get_op(stmt) == DECLS) { // Variable declaration
      codegen_local_var_decls(stmt);
    } else {
      codegen_statement(stmt);
    }
    node = get_child_opt_('{', '{', node, 1);
  }
}

void codegen_statement(ast node) {
  int op;
  int lbl1, lbl2, lbl3;
  int save_fs = cgc_fs;
  int save_locals = cgc_locals;
  int binding;

  if (node == 0) return;

  op = get_op(node);

  if (op == IF_KW) {

    lbl1 = alloc_label(0); // else statement
    lbl2 = alloc_label(0); // join point after if
    codegen_rvalue_and_cmp_0(EQ, lbl1, get_child_(IF_KW, node, 0));
    codegen_statement(get_child_(IF_KW, node, 1));
    jump(lbl2);
    def_label(lbl1);
    codegen_statement(get_child_(IF_KW, node, 2));
    def_label(lbl2);

  } else if (op == WHILE_KW) {

    lbl1 = alloc_label(0); // while statement start
    lbl2 = alloc_label(0); // join point after while

    cgc_add_enclosing_loop(cgc_fs, lbl2, lbl1);

    def_label(lbl1);
    codegen_rvalue_and_cmp_0(EQ, lbl2, get_child_(WHILE_KW, node, 0));
    codegen_statement(get_child_(WHILE_KW, node, 1));
    jump(lbl1);
    def_label(lbl2);

  } else if (op == FOR_KW) {

    lbl1 = alloc_label(0); // while statement start
    lbl2 = alloc_label(0); // join point after while
    lbl3 = alloc_label(0); // initial loop starting point

    cgc_add_enclosing_loop(cgc_fs, lbl2, lbl1);

    codegen_statement(get_child_(FOR_KW, node, 0)); // init
    jump(lbl3); // skip post loop action
    def_label(lbl1);
    codegen_statement(get_child_(FOR_KW, node, 2)); // post loop action
    def_label(lbl3);
    if (get_child_(FOR_KW, node, 1) != 0) {
      codegen_rvalue_and_cmp_0(EQ, lbl2, get_child_(FOR_KW, node, 1)); // test
    }
    // if no test, we always fall down to the body

    codegen_statement(get_child_(FOR_KW, node, 3));
    jump(lbl1);
    def_label(lbl2);

#ifdef SUPPORT_DO_WHILE

  } else if (op == DO_KW) {

    lbl1 = alloc_label(0); // do statement start
    lbl2 = alloc_label(0); // break point

    cgc_add_enclosing_loop(cgc_fs, lbl2, lbl1);
    def_label(lbl1);
    codegen_statement(get_child_(DO_KW, node, 0));
    codegen_rvalue_and_cmp_0(NE, lbl1, get_child_(DO_KW, node, 1));
    def_label(lbl2);

#endif // SUPPORT_DO_WHILE

  } else if (op == SWITCH_KW) {

    lbl1 = alloc_label(0); // lbl1: end of switch
    lbl2 = alloc_label(0); // lbl2: next case

#ifdef SUPPORT_EMULATED_INT64
    // The switch operand is kept on the stack and compared against each case.
    // For the comparison to work correctly between 64-bit and narrower types,
    // the switch operand must be coerced to the type of the case expression.
    cgc_add_enclosing_switch(cgc_fs, lbl1, lbl2, value_type(get_child_(SWITCH_KW, node, 0)));
#else
    cgc_add_enclosing_switch(cgc_fs, lbl1, lbl2);
#endif
    binding = cgc_locals;

    codegen_rvalue(get_child_(SWITCH_KW, node, 0));    // switch operand
    jump(lbl2);                                        // Jump to first case
    codegen_statement(get_child_(SWITCH_KW, node, 1)); // switch body

    // The switch can fall through in 2 distinct ways:
    //  1. The conditional block had no break statement
    //  2. No cases (excluding the default) matched
    //
    // In both cases, the switch operand needs to be removed from the stack.
    // But in the second case, we first need to jump to the default label if it
    // exists.
    //
    // The code is laid out as follows:
    //  [eval switch opnd]
    //  [cases]
    //  ...
    //   <- Control is here
    //  [jump to adjust stack]
    //  [jump to default if it exists]
    //  [adjust stack]
    //  [end of switch]

    // In case #1 control ends up here
    lbl3 = alloc_label(0);
    jump(lbl3);

    // In case #2 control ends up here
    lbl2 = switch_binding_next_case_lbl(binding); // Reload because the label is overwritten by CASE statements
    def_label(lbl2);
    // If the default statement is present, we jump to it. Otherwise, we'll fall
    // through to the end of the switch and remove the switch operand from the
    // stack.
    if (switch_binding_default_lbl(binding)) jump(switch_binding_default_lbl(binding));

    def_label(lbl3);

    // If we fell through the switch, break didn't restore the stack in its
    // original state so do it now.
    reset_stack_to(loop_or_switch_binding_fs(binding));

    def_label(lbl1); // End of switch label, break statements land here

  } else if (op == CASE_KW) {

    binding = cgc_lookup_enclosing_switch(cgc_locals);

    // Logic is as follows:
    //  if falling through:
    //    jump to statements
    //  else if top_of_stack == case_value:
    //   jump to statements
    // else:
    //   jump to next case
    if (binding != 0) {
      lbl1 = alloc_label(0);                  // skip case when falling through
      jump(lbl1);
      def_label(switch_binding_next_case_lbl(binding));       // false jump location of previous case
      switch_binding_next_case_lbl(binding) = alloc_label(0); // create false jump location for current case
#ifdef SUPPORT_EMULATED_INT64
      if (is_int64_type(switch_binding_expr_type(binding))) {
        // A scalar EQ can't compare 8-byte values: call eq_i64 on the switch
        // operand (kept at the top of the stack, not popped) and the case
        // expression, then branch on the 0/1 result like the scalar path
        // branches on the raw comparison.
        codegen_int64_case_eq(get_child_(CASE_KW, node, 0));
        xor_reg_reg(reg_Y, reg_Y);
        jump_cond_reg_reg(NE, lbl1, reg_X, reg_Y);
      } else
#endif
      {
      codegen_rvalue(get_child_(CASE_KW, node, 0)); // evaluate case expression and compare it
      stack_pop(reg_Y);                       // get case value
      stack_load(reg_X, cgc_fs);              // get switch operand without popping it
      jump_cond_reg_reg(EQ, lbl1, reg_X, reg_Y);
      }
      jump(switch_binding_next_case_lbl(binding)); // condition is false => jump to next case
      def_label(lbl1);                        // start of case conditional block
      codegen_statement(get_child_(CASE_KW, node, 1));  // case statement
    } else {
      fatal_error("case outside of switch");
    }

  } else if (op == DEFAULT_KW) {

    binding = cgc_lookup_enclosing_switch(cgc_locals);

    if (binding != 0) {
      if (switch_binding_default_lbl(binding)) fatal_error("default already defined in switch");
      switch_binding_default_lbl(binding) = alloc_label(0); // create label for default
      def_label(switch_binding_default_lbl(binding));       // default label
      codegen_statement(get_child_(DEFAULT_KW, node, 0)); // default statement
    } else {
      fatal_error("default outside of switch");
    }

  } else if (op == BREAK_KW) {

    binding = cgc_lookup_enclosing_loop_or_switch(cgc_locals);
    if (binding != 0) {
      // adjust stack and jump to break label
      stack_grow(loop_or_switch_binding_fs(binding) - cgc_fs);
      jump(loop_or_switch_binding_break_lbl(binding));
    } else {
      fatal_error("break is not in the body of a loop");
    }

  } else if (op == CONTINUE_KW) {

    binding = cgc_lookup_enclosing_loop(cgc_locals);
    if (binding != 0 && loop_binding_continue_lbl(binding) != 0) {
      // adjust stack and jump to continue label
      stack_grow(loop_or_switch_binding_fs(binding) - cgc_fs);
      jump(loop_binding_continue_lbl(binding));
    } else {
      fatal_error("continue is not in the body of a loop");
    }

  } else if (op == RETURN_KW) {

    if (get_child_(RETURN_KW, node, 0) != 0) {
#ifdef SUPPORT_STRUCT_UNION
      if (is_struct_like(current_fun_return_type)) {
        // Widening of scalar values to 64 bits done by codegen_aggregate_into
        binding = cgc_lookup_var(0, cgc_locals); // hidden parameter
        stack_load(reg_Y, var_binding_offset(binding)); // load hidden parameter address
        codegen_aggregate_into(reg_Y, 0, get_child_(RETURN_KW, node, 0), type_width(current_fun_return_type, true, true), current_fun_return_type);
        // The value of a struct/union expression is its address: leave the
        // caller-allocated buffer's address in reg_X.
        mov_reg_reg(reg_X, reg_Y);
      } else
#endif
      {
      // The returned value is converted to the function's return type, which
      // the callers trust to be correctly extended.
      codegen_rvalue_coerced(get_child_(RETURN_KW, node, 0), current_fun_return_type);
      stack_pop(reg_X); // return value is in reg_X
      }
    }

    // The cleanup code at the bottom isn't hit because of the ret, so cleaning here.
    stack_grow(-cgc_fs);

    ret();

  } else if (op == '{') {

    codegen_body(node);

#ifdef SUPPORT_GOTO
  } else if (op == ':') {

    binding = cgc_lookup_goto_label(get_val_(IDENTIFIER, get_child_(':', node, 0)), cgc_locals_fun);

    if (binding == 0) {
      cgc_add_goto_label(get_val_(IDENTIFIER, get_child_(':', node, 0)), alloc_goto_label());
      binding = cgc_locals_fun;
    }

    def_goto_label(goto_binding_lbl(binding));
    codegen_statement(get_child_(':', node, 1)); // labelled statement

  } else if (op == GOTO_KW) {

    codegen_goto(node);
#endif
  } else {

    codegen_rvalue(node);

  }

  reset_stack_to(save_fs);
  cgc_locals = save_locals;
}

#ifdef SUPPORT_STRUCT_UNION
// If the function returns a struct/union, we need to add a hidden parameter for
// the return value address and store the return type in a global variable so we
// can access it in the return statement.
void add_function_hidden_params(ast fun_return_type) {
  if (is_struct_like(fun_return_type)) {
    cgc_add_local_param(0, 1, pointer_type(fun_return_type, false));
  }
}
#endif // SUPPORT_STRUCT_UNION

void add_function_params(ast params) {
  ast decl, type;
  int ident;

  while (params != 0) {
    decl = car_(DECL, params);
    ident = get_val_(IDENTIFIER, get_child__(DECL, IDENTIFIER, decl, 0));
    type = get_child_(DECL, decl, 1);

    // Array to pointer decay
    if (get_op(type) == '[') { type = pointer_type(dereference_type(type), false); }

    if (cgc_lookup_var(ident, cgc_locals) != 0) fatal_error("add_function_params: duplicate parameter");

    cgc_add_local_param(ident, type_width(type, false, true) / WORD_SIZE, type);
    params = tail(params);
  }
}

#ifdef ONE_PASS_GENERATOR
// Initialize the function entry in the forward jump table
void init_forward_jump_table(int binding) {
#ifdef SAFE_MODE
  if (!is_label_defined(fun_binding_lbl(binding))) fatal_error("init_forward_jump_table: function not found");
#endif

  START_INIT_BLOCK();
  mov_reg_lbl(reg_X, fun_binding_lbl(binding));
  mov_mem_reg(reg_glo, fun_binding_glo_entry(binding), reg_X);

  // At this point, all labels should be defined, which means we can safely
  // output the code and overwrite the code buffer.

  assert_all_labels_defined(0); // In SAFE_MODE, this checks that all labels are defined
#ifdef PRINT_MEMORY_STATS
  code_alloc_max = TERNARY(code_alloc > code_alloc_max, code_alloc, code_alloc_max);
#endif
#ifndef ONE_PASS_GENERATOR_NO_EARLY_OUTPUT
  generate_exe();
  reset_code_buffer();
#endif

  END_INIT_BLOCK();
}
#else
// no-op
#define init_forward_jump_table(binding)
#endif

void codegen_glo_fun_decl(ast node) {
  ast decl = get_child__(FUN_DECL, DECL, node, 0);
  ast body = get_child_opt_(FUN_DECL, '{', node, 1);
  ast name_symbol = get_val_(IDENTIFIER, get_child__(DECL, IDENTIFIER, decl, 0));
  ast fun_type = get_child__(DECL, '(', decl, 1);
  ast params = get_child_opt_('(', LIST, fun_type, 1);
  ast fun_return_type = get_child_('(', fun_type, 0);
  int binding;

  cgc_locals_fun = 0; // init local bindings list

  if (get_op(fun_return_type) == '[') {
    fatal_error("Returning arrays from function not supported");
  }

  binding = cgc_lookup_fun(name_symbol, cgc_globals);

  if (binding == 0) {
    cgc_add_global_fun(name_symbol, alloc_label(symbol_buf(name_symbol)), fun_type);
    binding = cgc_globals;
  }

  // If the function is main
  if (name_symbol == MAIN_ID) {
    main_lbl = fun_binding_lbl(binding);
    // Check if main returns an exit code.
    switch (get_op(fun_return_type)) {
      case VOID_KW:
        main_returns = false;
        break;
      case INT_KW:
        main_returns = true;
        break;
      default:
         fatal_error("main has unsupported return type");
    }
  }

  // Poor man's debug info
#ifdef ADD_DEBUG_INFO
  debug_interrupt(); // Marker to helps us find the function in the disassembly
  codegen_string(symbol_buf(name_symbol), symbol_buf_end(name_symbol));
#endif

  def_label(fun_binding_lbl(binding));

  // if (fp_filepath[0] != 'p' || fp_filepath[1] != 'o' || fp_filepath[2] != 'r' || fp_filepath[3] != 't') {
  //   rt_debug(fp_filepath);
  //   rt_debug(":");
  //   rt_debug(symbol_buf(name_symbol));
  //   rt_debug("\n");
  // }

  cgc_fs = -1; // space for return address
  cgc_locals = 0;
  current_fun_return_type = fun_return_type;
#ifdef SUPPORT_STRUCT_UNION
  // Add hidden parameter for return value address if function returns a struct
  add_function_hidden_params(fun_return_type);
#endif
  add_function_params(params);
  cgc_fs = 0;

  codegen_body(body);

  // Drop everything on the stack and return to the caller
  reset_stack_to(0);
  ret();

  // Register the function in the forward jump table during initialization
  init_forward_jump_table(binding);
}

// For now, we don't do anything with the declarations in a typedef.
// The only thing we need to do is to call handle_enum_struct_union_type_decl
// on the type specifier, which is the same for all declarations.
void handle_typedef(ast node) {
  ast decls = get_child__(TYPEDEF_KW, LIST, node, 0);
  ast decl = car_(DECL, decls);
  ast type = get_child_(DECL, decl, 1);

  handle_enum_struct_union_type_decl(get_type_specifier(type));
}

void codegen_glo_decl(ast node) {
  ast decls;
  int op = get_op(node);

  if (op == DECLS) {
    // AUTO_KW and REGISTER_KW can simply be ignored. STATIC_KW is the default
    // storage class for global variables since pnut-sh only supports 1
    // translation unit.
#ifdef SUPPORT_TYPE_SPECIFIERS
    if (get_child_(DECLS, node, 1) == EXTERN_KW) fatal_error("Extern storage class specifier not supported");
#endif

    decls = get_child__(DECLS, LIST, node, 0); // Declaration list
    while (decls != 0) { // Multiple variable declarations
      codegen_glo_var_decl(car_(DECL, decls));
      decls = tail(decls); // Next variable declaration
    }
  } else if (op == FUN_DECL) {
    codegen_glo_fun_decl(node);
  } else if (op == TYPEDEF_KW) {
    handle_typedef(node);
  } else if (op == ENUM_KW
#ifdef SUPPORT_STRUCT_UNION
    || op == STRUCT_KW || op == UNION_KW
#endif
    ) {
    handle_enum_struct_union_type_decl(node);
  } else {
    dump_node(node);
    fatal_error("codegen_glo_decl: unexpected declaration");
  }
}

void rt_putchar() {
  push_reg(reg_X);            // Allocate buffer on stack containing the character
  mov_reg_imm(reg_X, 1);      // reg_X = file descriptor (stdout)
  mov_reg_reg(reg_Y, reg_SP); // reg_Y = buffer address
  mov_reg_imm(reg_Z, 1);      // reg_Z = buffer size
  os_write();
  pop_reg(reg_X);             // Deallocate buffer
}

void rt_debug(char* msg) {
  codegen_string(msg, msg + strlen(msg));
  mov_reg_imm(reg_X, 1);           // reg_X = file descriptor (stdout)
  pop_reg(reg_Y);                  // reg_Y = buffer address
  mov_reg_imm(reg_Z, strlen(msg)); // reg_Z = buffer size
  os_write();                      // Print the string
}

void rt_crash(char* msg) {
  rt_debug(msg);
  mov_reg_imm(reg_X, 42); // exit code
  os_exit();
}

#ifndef NO_BUILTIN_LIBC

void rt_fgetc(int fd_reg) {
  int success_lbl = alloc_label("rt_fgetc_success");
  push_reg(reg_X);            // Allocate buffer on stack, initialized with some random value
  mov_reg_reg(reg_X, fd_reg); // reg_X = file descriptor (stdin)
  mov_reg_reg(reg_Y, reg_SP); // reg_Y = buffer size
  mov_reg_imm(reg_Z, 1);      // reg_Z = buffer address
  os_read();                  // reg_X = number of bytes read, buffer[0] = character

  pop_reg(reg_Z);             // Get character from buffer and deallocate buffer
  mov_reg_imm(reg_Y, 0);      // If read returned 0, then we're at EOF (-1)
  jump_cond_reg_reg(NE, success_lbl, reg_X, reg_Y);
  mov_reg_imm(reg_Z, -1);     // mov  eax, -1  # -1 on EOF
  def_label(success_lbl);     // end label
  mov_reg_reg(reg_X, reg_Z);  // return value
}

void rt_fopen() {
  int fopen_success_lbl = alloc_label("fopen_success");

  mov_reg_mem(reg_X, reg_SP, WORD_SIZE);
  mov_reg_imm(reg_Y, 0); // mode
  mov_reg_imm(reg_Z, 0); // flag
  os_open();
  // If open fails, it returns -1, but we need to return NULL
  mov_reg_imm(reg_Y, 0);
  jump_cond_reg_reg(GE, fopen_success_lbl, reg_X, reg_Y);
  mov_reg_imm(reg_X, 0); // NULL
  def_label(fopen_success_lbl);
}

void rt_malloc() {
  int end_lbl = alloc_label("rt_malloc_success");

  mov_reg_mem(reg_Y, reg_glo, WORD_SIZE); // Bump pointer
  add_reg_reg(reg_X, reg_Y);              // New bump pointer
  mov_reg_mem(reg_Y, reg_glo, 0);         // Heap start
  add_reg_imm(reg_Y, RT_HEAP_SIZE);       // End of heap

  // Make sure the heap is large enough.
  // new bump pointer (reg_x) >= end of heap (reg_y)
  jump_cond_reg_reg(LE, end_lbl, reg_X, reg_Y);
  rt_crash("Heap overflow\n");

  def_label(end_lbl);
  mov_reg_mem(reg_Y, reg_glo, WORD_SIZE); // Old bump pointer
  mov_mem_reg(reg_glo, WORD_SIZE, reg_X); // Adjust the bump pointer
  mov_reg_reg(reg_X, reg_Y);              // Return the old bump pointer
}

#ifdef SUPPORT_STDIN_INPUT
void rt_isatty() {
  // Return 1 for stdin (fd 0), 0 otherwise
  // This is because pnut uses isatty(stdin) to determine whether to read from
  // stdin interactively, and uses isatty(stdout) to determine whether to output
  // colors, so it's important to return the "failsafe" value for each.

  int lbl1 = alloc_label(0); // false label
  int lbl2 = alloc_label(0); // end label

  // reg_X = fd
  mov_reg_imm(reg_Y, 0); // fd == 0
  jump_cond_reg_reg(EQ, lbl1, reg_X, reg_Y);
  mov_reg_imm(reg_X, 0); // false
  jump(lbl2);
  def_label(lbl1);
  mov_reg_imm(reg_X, 1); // true
  def_label(lbl2);
}
#endif

#endif

void codegen_builtin_movs(ast params) {
  int i = 0;
  int reg;
  while (params != 0) {
    switch (i) {
      case 0: reg = reg_X; break;
      case 1: reg = reg_Y; break;
      case 2: reg = reg_Z; break;
      default: fatal_error("declare_builtin: too many parameters");
    }
    mov_reg_mem(reg, reg_SP, WORD_SIZE * (i + 1)); // Get parameter from stack
    params = cdr(params);
    i += 1;
  }
}

int declare_builtin(char* name, bool variadic, ast return_type, ast params) {
  int lbl = alloc_label(name);
  ast params_start = params;
  // The params value is a list of types, we convert them to a list of declarations
  while (params != 0) {
    set_car(params, new_ast3(DECL, 0, car(params), 0));
    params = cdr(params);
  }

  return_type = function_type(return_type, params_start);
  if (variadic) return_type = make_variadic_func(return_type);
  cgc_add_global_fun(init_ident(IDENTIFIER, name), lbl, return_type);
  def_label(lbl);
  codegen_builtin_movs(params_start);
  return lbl;
}

void codegen_builtin() {
#ifdef ONE_PASS_GENERATOR
  int binding;
#endif

  // exit function
  exit_lbl = declare_builtin("exit", false, void_type, list1(int_type));
  os_exit();
  init_forward_jump_table(cgc_globals);

  // read function
  declare_builtin("read", false, int_type, list3(int_type, void_star_type, int_type));
  os_read();
  ret();
  init_forward_jump_table(cgc_globals);

  // write function
  declare_builtin("write", false, int_type, list3(int_type, void_star_type, int_type));
  os_write();
  ret();
  init_forward_jump_table(cgc_globals);

  // open function
  // Regarding the mode parameter, it is required if the flag allows the
  // creation of a new file. Otherwise, it may be omitted and is ignored by the
  // OS.
  // The manual says:
  // > If neither O_CREAT nor O_TMPFILE is specified in flags, then mode is
  // > ignored (and can thus be specified as 0, or simply omitted).  The mode
  // > argument must be supplied if O_CREAT or O_TMPFILE is specified in flags;
  // > if it is not supplied, some arbitrary bytes from the stack will be
  // > applied as the file mode.
  declare_builtin("open", true, int_type, list2(string_type, int_type));
  mov_reg_mem(reg_Z, reg_SP, 3*WORD_SIZE); // mode, if present
  os_open();
  ret();
  init_forward_jump_table(cgc_globals);

  // close function
  declare_builtin("close", false, int_type, list1(int_type));
#ifdef ONE_PASS_GENERATOR
  binding = cgc_globals; // Save the binding for the forward jump table
#endif
#ifndef NO_BUILTIN_LIBC
  // fclose is just like close because FILE * is just the file descriptor in the builtin libc
  declare_builtin("fclose", false, int_type, list1(int_type));
#endif
  os_close();
  ret();
  init_forward_jump_table(cgc_globals);
  init_forward_jump_table(binding);

  // seek function
  declare_builtin("lseek", false, int_type, list3(int_type, int_type, int_type));
  os_seek();
  ret();
  init_forward_jump_table(cgc_globals);

  // unlink function
  declare_builtin("unlink", false, int_type, list1(string_type));
  os_unlink();
  ret();
  init_forward_jump_table(cgc_globals);

  // mkdir function
  declare_builtin("mkdir", false, int_type, list2(string_type, int_type));
  os_mkdir();
  ret();
  init_forward_jump_table(cgc_globals);

  // chmod function
  declare_builtin("chmod", false, int_type, list2(string_type, int_type));
  os_chmod();
  ret();
  init_forward_jump_table(cgc_globals);

  // stat/access function
  declare_builtin("access", false, int_type, list2(string_type, int_type));
  os_access();
  ret();
  init_forward_jump_table(cgc_globals);

#ifndef NO_BUILTIN_LIBC
  // putchar function
  declare_builtin("putchar", false, void_type, list1(char_type));
  rt_putchar();
  ret();
  init_forward_jump_table(cgc_globals);

  // getchar function
  declare_builtin("getchar", false, char_type, 0);
  mov_reg_imm(reg_X, 0); // stdin
  rt_fgetc(reg_X);
  ret();
  init_forward_jump_table(cgc_globals);

  // fopen function
  declare_builtin("fopen", false, int_type, list2(string_type, string_type));
  rt_fopen();
  ret();
  init_forward_jump_table(cgc_globals);

  // fgetc function
  declare_builtin("fgetc", false, int_type, list1(int_type));
  rt_fgetc(reg_X);
  ret();
  init_forward_jump_table(cgc_globals);

  // malloc function
  declare_builtin("malloc", false, void_star_type, list1(int_type));
  rt_malloc();
  ret();
  init_forward_jump_table(cgc_globals);

  // free function (no-op)
  declare_builtin("free", false, void_type, list1(void_star_type));
  ret();
  init_forward_jump_table(cgc_globals);

  // printf function stub
  declare_builtin("printf", true, int_type, list1(string_type));
  rt_crash("printf is not supported yet.");
  ret();
  init_forward_jump_table(cgc_globals);

  // isatty function stub (always return 0)
  declare_builtin("isatty", true, int_type, list1(int_type));
#ifdef SUPPORT_STDIN_INPUT
  rt_isatty();
#else
  mov_reg_imm(reg_X, 0);
#endif
  ret();
  init_forward_jump_table(cgc_globals);
#endif
}

void init_memory_spaces(int glo_size) {
  glo_size = word_size_align(glo_size);
  // Allocate some space for the global variables.
  //
  // By default, the global variables are placed in a mmapped region, but not
  // all systems (buider-hex0 in particular) support this syscall so pnut can
  // also place globals on the stack.
#ifdef USE_STACK_FOR_GLOBALS
  int loop_lbl = alloc_label("glo_init_loop");
  mov_reg_reg(reg_Y, reg_SP); // reg_Y = end of global variables/heap
  stack_grow((glo_size + RT_HEAP_SIZE) / WORD_SIZE); // reg_SP = start of globals table/heap
  mov_reg_reg(reg_Z, reg_SP); // reg_Z = start of globals table/heap

  // Loop over the range [reg_Z, reg_Y)
  mov_reg_imm(reg_X, 0);                         // reg_X = 0
  def_label(loop_lbl);                           // loop:
  mov_mem_reg(reg_Z, 0, reg_X);                  //    *reg_Z = 0;
  add_reg_imm(reg_Z, WORD_SIZE);                 //    reg_Z += WORD_SIZE;
  jump_cond_reg_reg(LT, loop_lbl, reg_Z, reg_Y); //    if (reg_Z < reg_Y) goto loop;

  add_reg_imm(reg_Y, -glo_size); // reg_Y = start of global variables/end of heap
  mov_reg_reg(reg_glo, reg_Y);   // reg_glo = start of global variables/end of heap

  add_reg_imm(reg_Y, -RT_HEAP_SIZE); // reg_Y = start of heap
  mov_mem_reg(reg_glo, 0, reg_Y);    // Set init heap start
  mov_mem_reg(reg_glo, WORD_SIZE, reg_Y); // init bump pointer

#else
  // The global variables used to be on the stack, but because the stack has a
  // limited size, it is better to allocate a separate memory region so global
  // variables are not limited by the stack size.
  //
  // We then allocate a separate memory region for the heap. Having a separate
  // memory space for the heap makes it easier to detect out-of-bound accesses
  // on global variables.
  //
  // Regarding initialization, os_allocate_memory uses mmap with the
  // MAP_ANONYMOUS flag so the memory should already be zeroed.

  os_allocate_memory(glo_size);           // Returns the globals table start address in reg_X
  mov_reg_reg(reg_glo, reg_X);            // reg_glo = globals table start

  os_allocate_memory(RT_HEAP_SIZE);       // Returns the heap start address in reg_X
  mov_mem_reg(reg_glo, 0, reg_X);         // Set init heap start
  mov_mem_reg(reg_glo, WORD_SIZE, reg_X); // init bump pointer
#endif
}

void codegen_begin() {

  setup_lbl = alloc_label("setup");
  init_start_lbl = alloc_label("init_start");
  init_next_lbl = init_start_lbl;

  // Make room for heap start and malloc bump pointer.
  // reg_glo[0]: heap start
  // reg_glo[WORD_SIZE]: malloc bump pointer
  cgc_global_alloc += 2 * WORD_SIZE;

  one_literal = new_ast0(INTEGER, -1);
  int_type = new_ast0(INT_KW, 0);
#if defined(PARSE_NUMERIC_LITERAL_SUFFIX) || defined(SUPPORT_SIZEOF)
  uint_type = new_ast0(INT_KW, MK_TYPE_SPECIFIER(UNSIGNED_KW));
#endif
#if defined(PARSE_NUMERIC_LITERAL_SUFFIX)
  long_type = new_ast0(LONG_KW, 0);
  ulong_type = new_ast0(LONG_KW, MK_TYPE_SPECIFIER(UNSIGNED_KW));
#endif
  char_type = new_ast0(CHAR_KW, 0);
  string_type = pointer_type(new_ast0(CHAR_KW, 0), false);
  void_type = new_ast0(VOID_KW, 0);
  void_star_type = pointer_type(new_ast0(VOID_KW, 0), false);

#ifdef ONE_PASS_GENERATOR
  // Initialize the global variable table and heap for malloc
  init_memory_spaces(RT_GLO_SIZE);
  // Jump to the initialization code
  jump(init_start_lbl);
#else
  jump(setup_lbl);
#endif

  codegen_builtin();
}

void codegen_end() {
#ifndef ONE_PASS_GENERATOR
  def_label(setup_lbl);
  // Initialize the global variable table and heap for malloc
  init_memory_spaces(cgc_global_alloc);
  // Jump to the initialization code
  jump(init_start_lbl);
#endif

  def_label(init_next_lbl);
#if defined(USE_STACK_FOR_GLOBALS) && defined(ONE_PASS_GENERATOR)
  setup_proc_args(word_size_align(RT_GLO_SIZE + RT_HEAP_SIZE));
#elif defined(USE_STACK_FOR_GLOBALS)
  setup_proc_args(word_size_align(cgc_global_alloc + RT_HEAP_SIZE));
#else
  setup_proc_args(0);
#endif
#ifdef SAFE_MODE
  if (!main_lbl) fatal_error("main function not found");
#endif
  call(main_lbl);
  if (!main_returns) mov_reg_imm(reg_X, 0); // exit process with 0 if main returns void
  push_reg(reg_X); // exit process with result of main
  call(exit_lbl);

  assert_all_labels_defined(init_next_lbl);

  // Finish writing the code to the file
  generate_exe();

#ifdef ONE_PASS_GENERATOR
  // Check that the size we assumed for the ELF header and globals are correct.
  if (cgc_global_alloc >= RT_GLO_SIZE) fatal_error("Not enough space for global variables");
  if (code_address_base + code_alloc >= MAX_CODE_SIZE) fatal_error("codegen_end: code size too large, elf file is invalid.");
#endif

#ifdef PRINT_MEMORY_STATS
  printf("# string_pool_alloc=%d heap_alloc=%d code_alloc=%d code_alloc_max=%d\n", string_pool_alloc, heap_alloc, code_alloc, code_alloc_max);
#endif
}
