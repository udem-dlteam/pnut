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
  if (imm_obj <= 0) {
    emit_i32_le(-imm_obj);
  } else {
    // Check that the number doesn't overflow 64 bits
    if (heap[imm_obj + 1] != 0) fatal_error("emit_i32_le_large_imm: integer overflow");
    emit_i32_le(heap[imm_obj]);
  }
}

#if WORD_SIZE == 8
void emit_i64_le_large_imm(const int imm_obj) {
  if (imm_obj <= 0) {
    emit_i64_le(-imm_obj);
  } else {
    emit_i32_le(heap[imm_obj]);
    emit_i32_le(heap[imm_obj + 1]);
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
#ifdef SUPPORT_STRUCT_UNION
// Return type of the current function, used when returning a struct or union
ast current_fun_return_type = 0;
#endif

// Environment tracking
#include "env.c"

void grow_fs(const int words) {
  cgc_fs += words;
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

void push_reg(int src);
void pop_reg (int dst);

void jump(int lbl);
void jump_rel(int offset);
void call(int lbl);
void call_reg(int reg);
void ret();
void debug_interrupt();

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
    push_reg(other_reg);
    mov_reg_imm(other_reg, width);
    mul_reg_reg(reg, other_reg);
    pop_reg(other_reg);
  }
}

void div_for_pointer_arith(int reg, int width) {
  int reg_start = reg;

  if (width == 1) return;

  if (is_power_of_2(width)) {
    // sar_reg_reg does not work with reg_Y, so we need to shift the value to reg_X
    if (reg_start != reg_X) {
      push_reg(reg_X);                // Save reg_X
      mov_reg_reg(reg_X, reg_start);  // Move the value to reg_X
      reg = reg_X;
    } else {
      push_reg(reg_Y);                // Otherwise we still clobber reg_Y so save it
    }

    // At this point, reg is always reg_X, and reg_Y is free
    mov_reg_imm(reg_Y, power_of_2_log(width));
    sar_reg_reg(reg_X, reg_Y);

    // Now reg_X contains the result, and we move it back in reg_start if needed
    if (reg_start != reg_X) {
      mov_reg_reg(reg_start, reg_X);
      pop_reg(reg_X);
    } else {
      pop_reg(reg_Y); // Otherwise, we still need to restore reg_Y
    }
  } else {
    // div_reg_reg only works with reg_X on certain architectures, so we need to save it
    if (reg_start != reg_X) {
      push_reg(reg_X);
      reg = reg_X;
    } else {
      push_reg(reg_Y);
    }

    mov_reg_imm(reg_Y, width);
    div_reg_reg(reg_X, reg_Y);

    if (reg_start != reg_X) {
      mov_reg_reg(reg_start, reg_X);
      pop_reg(reg_X);
    } else {
      pop_reg(reg_Y);
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

int word_size_align(int n) {
  return (n + WORD_SIZE - 1) / WORD_SIZE * WORD_SIZE;
}

int align_to(int mul, int n) {
  return (n + mul - 1) / mul * mul;
}

void grow_stack(int words) {
  add_reg_imm(reg_SP, -words * WORD_SIZE);
}

// Like grow_stack, but takes bytes instead of words.
// To maintain alignment, the stack is grown by a multiple of WORD_SIZE (rounded
// up from the number of bytes).
void grow_stack_bytes(int bytes) {
  add_reg_imm(reg_SP, -word_size_align(bytes));
}

void drop_stack_words(int words) {
  grow_fs(-words);
  grow_stack(-words);
}

void rt_debug(char* msg);
void rt_crash(char* msg);

// Label definition

enum {
  GENERIC_LABEL,
  GOTO_LABEL,
};

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
    if (lbl != init_next_lbl && heap[lbl + 1] > 0) {
#ifdef UNDEFINED_LABELS_ARE_RUNTIME_ERRORS
      if (heap[lbl] == GENERIC_LABEL && heap[lbl + 2] != 0) {
        def_label(lbl);
        rt_debug("Function or label is not defined\n");
        rt_debug("name = ");
        rt_debug((char*) heap[lbl + 2]);
        rt_debug("\n");
        // TODO: This should crash but let's just return for now to see how far we can get
        ret();
      }
#else
      putstr("Label ");
      if (heap[lbl] == GENERIC_LABEL && heap[lbl + 2] != 0) {
        putstr((char*) heap[lbl + 2]);
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
  heap[lbl] = GENERIC_LABEL;
  heap[lbl + 1] = 0; // Address of label
  heap[lbl + 2] = (intptr_t) name; // Name of label
  heap[lbl + 3] = (intptr_t) fp_filepath;
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
  heap[lbl + 4] = line_number;
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
  heap[lbl] = GENERIC_LABEL;
  heap[lbl + 1] = 0; // Address of label
  add_label(lbl);
  return lbl;
}
#endif

#ifdef SUPPORT_GOTO

int alloc_goto_label() {
  int lbl = alloc_obj(3);
  heap[lbl] = GOTO_LABEL;
  heap[lbl + 1] = 0; // Address of label
  heap[lbl + 2] = 0; // cgc-fs of label
  add_label(lbl);
  return lbl;
}

#endif // SUPPORT_GOTO

bool is_label_defined(int lbl) {
  return heap[lbl + 1] < 0;
}

void use_label(int lbl) {

  int addr = heap[lbl + 1];

#ifdef SAFE_MODE
  if (heap[lbl] != GENERIC_LABEL) fatal_error("use_label expects generic label");
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
    // heap[lbl + 1] = [ patch address #1 ]
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
    heap[lbl + 1] = code_alloc;
  }
}

void def_label(int lbl) {

  int addr = heap[lbl + 1];
  int label_addr = code_alloc;
  int next_addr;

#ifdef SAFE_MODE
  if (heap[lbl] != GENERIC_LABEL) fatal_error("def_label expects generic label");
#endif

  if (addr < 0) {
#ifdef SAFE_MODE
    putstr("Label ");
    if (heap[lbl + 2] != 0) {
      putstr((char*) heap[lbl + 2]);
    } else {
      putint(lbl);
    }
    putstr(" previously defined at ");
    putstr((char*) heap[lbl + 3]);
#ifdef INCLUDE_LINE_NUMBER_ON_ERROR
    putstr(":");
    putint(heap[lbl + 4]);
#endif
    fatal_error(" being redefined");
#endif
  } else {
    heap[lbl + 1] = - (code_address_base + code_alloc); // define label's address
    while (addr != 0) {
      next_addr = code[addr - 1]; // get pointer to next patch address before we overwrite it
      code_alloc = addr - 4; // place code pointer to where use_label was called
      emit_i32_le(label_addr - addr); // replace placeholder with relative address
      addr = next_addr;
    }
    code_alloc = label_addr;
  }
}

#ifdef SUPPORT_GOTO

// Similar to use_label, but for gotos.
// The main difference is that it adjusts the stack and jumps, as opposed to
// simply emitting the address.
void jump_to_goto_label(int lbl) {

  int addr = heap[lbl + 1];
  int lbl_fs = heap[lbl + 2];
  int start_code_alloc = code_alloc;

#ifdef SAFE_MODE
  if (heap[lbl] != GOTO_LABEL) fatal_error("jump_to_goto_label expects goto label");
#endif

  if (addr < 0) {
    // label address is currently known
    grow_stack(lbl_fs - cgc_fs);
    start_code_alloc = code_alloc;
    jump_rel(0); // Generate dummy jump instruction to get instruction length
    addr = -addr - code_alloc; // compute relative address
    code_alloc = start_code_alloc;
    jump_rel(addr);
  } else {
    // label address is not yet known
    // placeholders for when we know the destination address and frame size
    grow_stack(0);
    jump_rel(0);
    code[code_alloc-1] = addr; // chain with previous patch address
    code[code_alloc-2] = cgc_fs; // save current frame size
    code[code_alloc-3] = start_code_alloc; // track initial code alloc so we can come back
    heap[lbl + 1] = code_alloc;
  }
}

void def_goto_label(int lbl) {

  int addr = heap[lbl + 1];
  int label_addr = code_alloc;
  int next_addr;
  int goto_fs;
  int start_code_alloc;

#ifdef SAFE_MODE
  if (heap[lbl] != GOTO_LABEL) fatal_error("def_goto_label expects goto label");
#endif

  if (addr < 0) {
    fatal_error("goto label defined more than once");
  } else {
    heap[lbl + 1] = -label_addr; // define label's address
    heap[lbl + 2] = cgc_fs;      // define label's frame size
    while (addr != 0) {
      next_addr = code[addr-1]; // get pointer to next patch address
      goto_fs = code[addr-2]; // get frame size at goto instruction
      code_alloc = code[addr-3]; // reset code pointer to start of jump_to_goto_label instruction
      grow_stack(cgc_fs - goto_fs); // adjust stack
      start_code_alloc = code_alloc;
      jump_rel(0); // Generate dummy jump instruction to get instruction length
      addr = label_addr - code_alloc; // compute relative address
      code_alloc = start_code_alloc;
      jump_rel(addr);
      addr = next_addr;
    }
    code_alloc = label_addr;
  }
}

#endif // SUPPORT_GOTO

ast int_type;
#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
ast uint_type;
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

#ifdef SUPPORT_STRUCT_UNION

bool is_struct_or_union_type(ast type) {
  int op = get_op(type);
  return op == STRUCT_KW || op == UNION_KW;
}

#endif // SUPPORT_STRUCT_UNION

// An aggregate type is either an array type or a struct/union type (that's not a reference)
bool is_aggregate_type(ast type) {
  int op = get_op(type);
#ifdef SUPPORT_STRUCT_UNION
  return op == '[' || op == STRUCT_KW || op == UNION_KW;
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
#if WORD_SIZE == 8
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
  res = heap[binding+3];

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

// Compute result type of a binary arithmetic operation.
ast arith_value_type(int op, ast left_type, ast right_type) {
  if (is_pointer_type(left_type) && is_pointer_type(right_type) && op == '-') {
    return int_type; // pointer - pointer -> integer
  } else if (is_pointer_type(left_type)) {
    // pointer + integer -> pointer
    return left_type;
  } else {
    // integer + pointer -> pointer or integer + integer -> integer
    // Returning right type for simplicity
    return right_type;
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
#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
    if (op == INTEGER || op == INTEGER_L || op == INTEGER_LL) {
      return int_type;
    } else if (op == INTEGER_U || op == INTEGER_UL || op == INTEGER_ULL) {
      return uint_type;
    }
#else
    if (op == INTEGER) {
      return int_type;
    }
#endif
    else if (op == CHARACTER) {
      return char_type;
    } else if (op == STRING) {
      return string_type;
    } else if (op == IDENTIFIER) {
      binding = resolve_identifier(get_val_(IDENTIFIER, node));
      switch (binding_kind(binding)) {
        case BINDING_PARAM_LOCAL:
        case BINDING_VAR_LOCAL:
          return heap[binding+4];
        case BINDING_VAR_GLOBAL:
          return heap[binding+4];
        case BINDING_ENUM_CST:
          return int_type;
        case BINDING_FUN:
          return heap[binding+5];
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
    } else if (op == '+' || op == '-' || op == '~' || op == MINUS_MINUS || op == PLUS_PLUS || op == MINUS_MINUS_POST || op == PLUS_PLUS_POST || op == PLUS_PLUS_PRE || op == MINUS_MINUS_PRE) {
      // Unary operation don't change the type
      return value_type(child0);
    }
#ifdef SUPPORT_SIZEOF
    else if (op == SIZEOF_KW) {
      return int_type; // sizeof always returns an integer
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
      return value_type(child0);
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
      // TODO: Check that the operands have compatible types?
      return value_type(child0);
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
      // We assume that the 2 cases have the same type.
      return value_type(child1);
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

void codegen_binop(int op, ast lhs, ast rhs) {
  ast left_type = value_type(lhs);
  ast right_type = value_type(rhs);
  bool left_is_numeric = is_numeric_type(left_type);
  bool right_is_numeric = is_numeric_type(right_type);
  // If any of the operands is unsigned, the result is unsigned
  bool is_signed = false;
  if (is_signed_numeric_type(left_type) && is_signed_numeric_type(right_type)) is_signed = true;

  pop_reg(reg_Y); // rhs operand
  pop_reg(reg_X); // lhs operand

  if      (op == '<')     { codegen_binop_cmp(TERNARY(is_signed, LT, LT_U)); }
  else if (op == '>')     { codegen_binop_cmp(TERNARY(is_signed, GT, GT_U)); }
  else if (op == LT_EQ)   { codegen_binop_cmp(TERNARY(is_signed, LE, LE_U)); }
  else if (op == GT_EQ)   { codegen_binop_cmp(TERNARY(is_signed, GE, GE_U)); }
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
    if (is_signed) imul_reg_reg(reg_X, reg_Y);
    else mul_reg_reg(reg_X, reg_Y);
  }
  else if (op == '/' || op == SLASH_EQ) {
    if (!left_is_numeric || !right_is_numeric) fatal_error("invalid operands to /");
    if (is_signed) idiv_reg_reg(reg_X, reg_Y);
    else div_reg_reg(reg_X, reg_Y);
  }
  else if (op == '%' || op == PERCENT_EQ) {
    if (!left_is_numeric || !right_is_numeric) fatal_error("invalid operands to %");
    if (is_signed) irem_reg_reg(reg_X, reg_Y);
    else rem_reg_reg(reg_X, reg_Y);
  }
  else if (op == RSHIFT || op == RSHIFT_EQ) {
    if (!left_is_numeric || !right_is_numeric) fatal_error("invalid operands to >>");
    // Whether the shift is arithmetic or logical depends only on the left
    // operand's type; the signedness of the shift count is irrelevant.
    if (is_signed_numeric_type(left_type)) sar_reg_reg(reg_X, reg_Y);
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
  else if (op == ',') {
    mov_reg_reg(reg_X, reg_Y); // Ignore lhs and keep rhs
  }
  else if (op == '[') {
    // Same as pointer addition + dereference of the result.
    codegen_binop_add(left_type, right_type); // Compute the address resulting from the addition
    left_type = arith_value_type('+', left_type, right_type); // The pointer type
    load_mem_location(reg_X, reg_X, 0, ref_type_width(left_type), is_signed_numeric_type(dereference_type(left_type)));
  } else {
    dump_op(op);
    fatal_error("codegen_binop: unknown op");
  }

  push_reg(reg_X);
}

void codegen_rvalue(ast node);
void codegen_statement(ast node);
int codegen_lvalue(ast node);

// Evaluate a condition expression and pop its value into reg_X, freeing any
// aggregate temporaries allocated during evaluation.
void codegen_rvalue_and_cmp_0(int cond, int lbl, ast node) {
#ifdef SUPPORT_STRUCT_UNION
  int save_fs = cgc_fs;
#endif
  codegen_rvalue(node);
  pop_reg(reg_X);
  grow_fs(-1);
#ifdef SUPPORT_STRUCT_UNION
  // The node's value is immediately consumed by a conditional jump, so it can
  // be collected right away. This also ensures that temporaries don't pile up
  // during loops, where the condition is evaluated multiple times.
  if (cgc_fs != save_fs) drop_stack_words(cgc_fs - save_fs);
#endif

  xor_reg_reg(reg_Y, reg_Y);
  jump_cond_reg_reg(cond, lbl, reg_X, reg_Y);
}


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
//  - When evaluating a ternary operator condition, since the individual arms
//    may allocate a different number of temporaries.
//    Handled by `codegen_ternary_arm`.
//  - When generating a function call, since the temporary would sit between the
//    arguments and/or function pointer (for indirect calls).
//    Handled by `codegen_rvalue_and_drop_temps`.
//  - When evaluating a local variable initializer, since temporary values would
//    offset the local variable's address which is assumed to not change.
//    Handled by `codegen_rvalue_and_drop_temps`.
//
// =============================================================================


// Evaluate an rvalue expression and pop its value into reg_X, freeing any
// aggregate temporaries allocated during evaluation unless the expression
// returns an aggregate value. In that case, the value on top of the stack is a
// pointer to the aggregate temporaries, and is handled case by case by the
// caller (struct assignment, function params, ternary operator arms, local
// variable initializers)
#ifdef SUPPORT_STRUCT_UNION
void codegen_rvalue_and_drop_temps(ast node) {
  int save_fs = cgc_fs;
  codegen_rvalue(node);
  if (cgc_fs != save_fs + 1 && !is_aggregate_type(value_type(node))) {
    pop_reg(reg_X);
    grow_fs(-1);
    drop_stack_words(cgc_fs - save_fs);
    push_reg(reg_X);
    grow_fs(1);
  }
}
#else
#define codegen_rvalue_and_drop_temps(node) codegen_rvalue(node)
#endif

int codegen_param(ast param) {
  int type = value_type(param);

#ifdef SUPPORT_STRUCT_UNION
  int left_width;
  int save_fs = cgc_fs;
  int temp_words;

  if (is_struct_or_union_type(type)) {
    left_width = codegen_lvalue(param);
    pop_reg(reg_X);
    grow_fs(-1);
    temp_words = cgc_fs - save_fs;
    grow_stack_bytes(word_size_align(left_width));
    grow_fs(word_size_align(left_width) / WORD_SIZE);
    copy_obj(reg_SP, 0, reg_X, 0, left_width);
    if (temp_words != 0) {
      // The argument was copied from a temporary (e.g. a struct-returning
      // call's result) that sits right below it on the stack. Slide the
      // argument copy down over the temporaries so that the argument words stay
      // contiguous (the callee addresses its parameters relative to the stack
      // pointer). The regions don't overlap because the temporary area contains
      // the source object, so it is at least as large as the argument.
      mov_reg_imm(reg_X, temp_words * WORD_SIZE);
      add_reg_reg(reg_X, reg_SP);
      copy_obj(reg_X, 0, reg_SP, 0, left_width);
      drop_stack_words(temp_words);
    }
  } else
#endif
  {
    // keep the argument words contiguous, flushing any leftover temporaries
    // below them on the stack.
    codegen_rvalue_and_drop_temps(param);
  }

  return type_width(type, false, true) / WORD_SIZE;
}

#ifdef SAFE_MODE
int codegen_params(ast params, ast params_type, bool allow_extra_params) {
#else
int codegen_params(ast params) {
#endif

  int fs = 0;

  if (params != 0) {
#ifdef SAFE_MODE
    if (!allow_extra_params && params_type == 0) {
      fatal_error("codegen_params: Function expects less parameters than provided");
    }

    // Check that the number of parameters is correct
    if (params_type != 0) params_type = tail(params_type);
#endif

#ifdef SAFE_MODE
    fs = codegen_params(tail(params), params_type, allow_extra_params);
#else
    fs = codegen_params(tail(params));
#endif
    fs += codegen_param(car(params));
  }
#ifdef SAFE_MODE
  else if (params_type != 0) {
    fatal_error("codegen_params: Function expects more parameters than provided");
  }
#endif

  return fs;
}

void emit_function_call(ast fun, int binding) {
  // Generate a fast path for direct calls
  if (binding != 0) {
#ifdef ONE_PASS_GENERATOR
    // When compiling in one pass mode, forward jumps must go through the jump table
    if (is_label_defined(fun_binding_lbl(binding))) {
      call(fun_binding_lbl(binding));
    } else {
      mov_reg_mem(reg_X, reg_glo, heap[binding+6]);
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
    codegen_rvalue_and_drop_temps(fun);
    pop_reg(reg_X);
    grow_fs(-1);
    call_reg(reg_X);
  }
}

void codegen_call(ast node) {
  ast fun = get_child_('(', node, 0);
  ast params = get_child_('(', node, 1);
  ast nb_params;
  int binding = 0;
#ifdef SUPPORT_STRUCT_UNION
  int buf_words = 0;
  ast fun_return_type = function_return_type(value_type(fun));

  if (is_struct_or_union_type(fun_return_type)) {
    // The function returns a struct/union: allocate the buffer in which the
    // callee will write the result. The buffer's address is passed as a
    // hidden argument, and is also the value of the call expression. The
    // buffer is freed at the end of the full expression.
    buf_words = type_width(fun_return_type, true, true) / WORD_SIZE;
    grow_stack(buf_words);
    grow_fs(buf_words);
  }
#endif

  // Check if the function is a direct call, find the binding if it is
  if (get_op(fun) == IDENTIFIER) {
    #ifdef ENABLE_PNUT_INLINE_INTERRUPT
    if (get_val_(IDENTIFIER, fun) == intern_str("PNUT_INLINE_INTERRUPT")) {
      debug_interrupt();
      push_reg(reg_X); // Dummy push to keep the stack balanced
      return;
    }
    #endif
    binding = resolve_identifier(get_val_(IDENTIFIER, fun));
    if (binding_kind(binding) != BINDING_FUN) binding = 0;
  }

#ifdef SAFE_MODE
  // Make sure fun has a type that can be called, either a function pointer or a function
  ast type = value_type(fun);
  if (!is_function_type(type)) {
    dump_node(type);
    fatal_error("Not a function or function pointer");
  }
  if (get_op(type) == '*') type = get_child_('*', type, 1); // Dereference function pointer
  // allow_extra_params is true if the function is called indirectly or if the function is variadic
  bool allow_extra_params = binding == 0;
  if (get_child_('(', type, 2)) allow_extra_params = true;
  nb_params = codegen_params(params, get_child_('(', type, 1), allow_extra_params);
#else
  nb_params = codegen_params(params);
#endif

#ifdef SUPPORT_STRUCT_UNION
  if (buf_words != 0) {
    // Push the buffer address as the hidden first argument (pushed last)
    // The arguments occupy nb_params words right above the buffer.
    mov_reg_imm(reg_X, nb_params * WORD_SIZE);
    add_reg_reg(reg_X, reg_SP);
    push_reg(reg_X);
    grow_fs(1);
    nb_params += 1;
  }
#endif

  emit_function_call(fun, binding);

  drop_stack_words(nb_params);

#ifdef SUPPORT_STRUCT_UNION
  // After popping the arguments, the result buffer is on top of the stack.
  // Its address is the value of the call expression.
  if (buf_words != 0) mov_reg_reg(reg_X, reg_SP);
#endif

  push_reg(reg_X);
}

// One arm of a ternary expression: evaluate the arm and leave its value in
// reg_X, freeing the arm's temporaries so that both arms leave the same stack
// shape. width != 0 means the ternary is aggregate-valued: the arm's value (a
// source address) is copied into the result buffer allocated right below the
// arm's temporaries, and reg_X is set to the buffer address.
// Returns the lvalue width when in lvalue context.
int codegen_ternary_arm(ast arm, bool lvalue_ctx, int width) {
  int save_fs = cgc_fs;
  int lvalue_width = 0;

  if (lvalue_ctx) {
    lvalue_width = codegen_lvalue(arm);
  } else {
    codegen_rvalue(arm); // for aggregates, the value is the address
  }
  pop_reg(reg_X);
  grow_fs(-1);
#ifdef SUPPORT_STRUCT_UNION
  if (width != 0) {
    mov_reg_imm(reg_Y, (cgc_fs - save_fs) * WORD_SIZE);
    add_reg_reg(reg_Y, reg_SP);
    copy_obj(reg_Y, 0, reg_X, 0, width);
    mov_reg_reg(reg_X, reg_Y);
  }
#endif
  if (cgc_fs != save_fs) drop_stack_words(cgc_fs - save_fs);
  return lvalue_width;
}

// Ternary expression, in rvalue or lvalue context. Each arm leaves its value
// in reg_X, which is pushed at the join point; the caller accounts for the
// value word (grow_fs(1) at the end of codegen_rvalue/codegen_lvalue).
// Aggregate-valued ternaries get a result buffer allocated before branching,
// since the arms may allocate different amounts of temporaries; the value is
// then the buffer address. Returns the width of the value when it matters
// (lvalue context or aggregate).
int codegen_ternary(ast node, bool lvalue_ctx) {
  int lbl1 = alloc_label(0); // false label
  int lbl2 = alloc_label(0); // end label
  int width = 0;
  int lvalue_width;
#ifdef SUPPORT_STRUCT_UNION
  ast type = value_type(node);

  if (is_struct_or_union_type(type)) {
    width = type_width(type, true, false);
    grow_stack_bytes(width);
    grow_fs(word_size_align(width) / WORD_SIZE);
  }
#endif

  codegen_rvalue_and_cmp_0(EQ, lbl1, get_child_('?', node, 0));
  lvalue_width = codegen_ternary_arm(get_child_('?', node, 1), lvalue_ctx, width); // value when true
  jump(lbl2);
  def_label(lbl1);
  codegen_ternary_arm(get_child_('?', node, 2), lvalue_ctx, width); // value when false
  def_label(lbl2);

  push_reg(reg_X);

#ifdef SUPPORT_STRUCT_UNION
  if (width != 0) lvalue_width = width;
#endif
  return lvalue_width;
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

  jump_to_goto_label(heap[binding + 3]); // Label
}

#endif // SUPPORT_GOTO

// Return the width of the lvalue
int codegen_lvalue(ast node) {
  int op = get_op(node);
  int nb_children = get_nb_children(node);
  int binding;
  int lvalue_width = 0;
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
          lvalue_width = cgc_fs - heap[binding+3];
          mov_reg_imm(reg_X, lvalue_width * WORD_SIZE);
          add_reg_reg(reg_X, reg_SP);
          push_reg(reg_X);
          break;
        case BINDING_VAR_GLOBAL:
          mov_reg_imm(reg_X, heap[binding+3]);
          add_reg_reg(reg_X, reg_glo);
          push_reg(reg_X);
          break;
        case BINDING_FUN:
          // Function pointers are stored in the forward jump table
#ifdef ONE_PASS_GENERATOR
          mov_reg_mem(reg_X, reg_glo, heap[binding+6]);
#else
          mov_reg_lbl(reg_X, heap[binding+4]);
#endif
          push_reg(reg_X);
          break;
        default:
          fatal_error("codegen_lvalue: identifier not found");
          break;
      }
      lvalue_width = type_width(heap[binding+4], true, false);
    } else {
      dump_node(node);
      fatal_error("codegen_lvalue: unexpected operator");
    }

  } else if (nb_children == 1) {

    if (op == '*') {
      codegen_rvalue(child0);
      grow_fs(-1);
      lvalue_width = ref_type_width(value_type(child0));
    } else {
      dump_node(node);
      fatal_error("codegen_lvalue: unexpected operator");
    }

  } else if (nb_children == 2) {

    if (op == '[') {
      type = value_type(child0);
      codegen_rvalue(child0);
      codegen_rvalue(child1);
      codegen_binop('+', child0, child1);
      grow_fs(-2);
      lvalue_width = ref_type_width(type);
    }
#ifdef SUPPORT_STRUCT_UNION
    else if (op == '.') {
      type = value_type(child0);
      if (is_struct_or_union_type(type)) {
        codegen_lvalue(child0);
        pop_reg(reg_X);
        // union members are at the same offset: 0
        if (get_op(type) == STRUCT_KW) {
          add_reg_imm(reg_X, struct_member_offset(type, child1));
        }
        push_reg(reg_X);
        grow_fs(-1);
        lvalue_width = type_width(get_child_(DECL, struct_member(type, child1), 1), true, false); // child 1 of member is the type
      } else {
        fatal_error("codegen_lvalue: . operator on non-struct type");
      }
    } else if (op == ARROW) {
      // Same as '.', but type must be a pointer
      type = value_type(child0);
      if (get_op(type) == '*' && is_struct_or_union_type(get_child_('*', type, 1))) {
        type = get_child_('*', type, 1);
        codegen_rvalue(child0);
        pop_reg(reg_X);
        // union members are at the same offset: 0
        if (get_op(type) == STRUCT_KW) {
          add_reg_imm(reg_X, struct_member_offset(type, child1));
        }
        push_reg(reg_X);
        grow_fs(-1);
        lvalue_width = type_width(get_child_(DECL, struct_member(type, child1), 1), true, false); // child 1 of member is the type
      } else {
        fatal_error("codegen_lvalue: -> operator on non-struct pointer type");
      }
    } else if (op == '(') {
      type = value_type(node);
      if (is_struct_or_union_type(type)) {
        // Call to a struct/union-returning function: the lvalue is the
        // temporary buffer in which the callee wrote the result. codegen_call
        // leaves the buffer address on the stack; that value word is
        // accounted for by the grow_fs(1) at the end of the function.
        codegen_call(node);
        lvalue_width = type_width(type, true, false);
      } else {
        fatal_error("codegen_lvalue: function call does not return a struct/union");
      }
    } else if (op == ',') {
      // A comma expression is only an lvalue when the rhs is an aggregate,
      // whose "lvalue" is the address of its value. The lhs value word stays
      // buried below the rhs temporaries and is freed at the end of the full
      // expression.
      codegen_rvalue_and_drop_temps(child0);
      lvalue_width = codegen_lvalue(child1);
      grow_fs(-1); // nets the rhs lvalue word with the final grow_fs(1)
    }
#endif // SUPPORT_STRUCT_UNION
    else if (op == CAST) {
      codegen_lvalue(child1);
      lvalue_width = type_width(child0, true, false);
      grow_fs(-1); // grow_fs is called at the end of the function, so we need to decrement it here
    } else {
      dump_node(node);
      fatal_error("codegen_lvalue: unexpected operator");
    }

  } else if (nb_children == 3) {

    if (op == '?') {

      // assume that lvalue_width is the same for both arms
      lvalue_width = codegen_ternary(node, true);

    } else {
      dump_node(node);
      fatal_error("codegen_lvalue: unexpected operator");
    }

  } else {
    dump_node(node);
    fatal_error("codegen_lvalue: unexpected operator");
  }

  if (lvalue_width == 0) {
    fatal_error("codegen_lvalue: lvalue_width == 0");
  }

  grow_fs(1);
  return lvalue_width;
}

void codegen_string(char *string_start, char *string_end) {
  int lbl = alloc_label(0);

  call(lbl);

  while (string_start != string_end) {
    emit_i8(*string_start);
    string_start += 1;
  }

  emit_i8(0);

  def_label(lbl);
}

void codegen_rvalue(ast node) {
  int op = get_op(node);
  int nb_children = get_nb_children(node);
  int binding;
  int lbl1, lbl2;
  int left_width;
  ast type1;
#ifdef SUPPORT_STRUCT_UNION
  ast type2;
  int save_fs;
#endif
  ast child0, child1;

  if (nb_children >= 1) child0 = get_child(node, 0);
  if (nb_children >= 2) child1 = get_child(node, 1);

  if (nb_children == 0) {
    if ( op == INTEGER
#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
      || op == INTEGER_L || op == INTEGER_LL || op == INTEGER_U || op == INTEGER_UL || op == INTEGER_ULL
#endif
       ) {
#ifdef SUPPORT_64_BIT_LITERALS
      mov_reg_large_imm(reg_X, get_val(node));
#else
      mov_reg_imm(reg_X, -get_val(node));
#endif
      push_reg(reg_X);
    } else if (op == CHARACTER) {
      mov_reg_imm(reg_X, get_val_(CHARACTER, node));
      push_reg(reg_X);
    } else if (op == IDENTIFIER) {
      binding = resolve_identifier(get_val_(IDENTIFIER, node));
      switch (binding_kind(binding)) {
        case BINDING_PARAM_LOCAL:
          left_width = cgc_fs - heap[binding+3];
          mov_reg_imm(reg_X, left_width * WORD_SIZE);
          add_reg_reg(reg_X, reg_SP);
          // structs/unions are allocated on the stack, so no need to dereference
          // For arrays, we need to dereference the pointer since they are passed as pointers
#ifdef SUPPORT_STRUCT_UNION
          if (get_op(heap[binding+4]) != STRUCT_KW && get_op(heap[binding+4]) != UNION_KW) {
            load_mem_location(reg_X, reg_X, 0, type_width(heap[binding+4], false, false), is_signed_numeric_type(heap[binding+4]));
          }
#else
          load_mem_location(reg_X, reg_X, 0, type_width(heap[binding+4], false, false), is_signed_numeric_type(heap[binding+4]));
#endif
          push_reg(reg_X);
          break;

        case BINDING_VAR_LOCAL:
          left_width = cgc_fs - heap[binding+3];
          mov_reg_imm(reg_X, left_width * WORD_SIZE);
          add_reg_reg(reg_X, reg_SP);
          // local arrays/structs/unions are allocated on the stack, so no need to dereference
          if (get_op(heap[binding+4]) != '['
#ifdef SUPPORT_STRUCT_UNION
           && get_op(heap[binding+4]) != STRUCT_KW && get_op(heap[binding+4]) != UNION_KW
#endif
            ) {
            load_mem_location(reg_X, reg_X, 0, type_width(heap[binding+4], false, false), is_signed_numeric_type(heap[binding+4]));
          }
          push_reg(reg_X);
          break;
        case BINDING_VAR_GLOBAL:
          mov_reg_imm(reg_X, heap[binding+3]);
          add_reg_reg(reg_X, reg_glo);
          // global arrays/structs/unions are also allocated on the stack, so no need to dereference
          if (get_op(heap[binding+4]) != '['
#ifdef SUPPORT_STRUCT_UNION
           && get_op(heap[binding+4]) != STRUCT_KW && get_op(heap[binding+4]) != UNION_KW
#endif
          ) {
            load_mem_location(reg_X, reg_X, 0, type_width(heap[binding+4], false, false), is_signed_numeric_type(heap[binding+4]));
          }
          push_reg(reg_X);
          break;
        case BINDING_ENUM_CST:
#ifdef SUPPORT_64_BIT_LITERALS
          mov_reg_large_imm(reg_X, get_val(heap[binding+3]));
#else
          mov_reg_imm(reg_X, -get_val_(INTEGER, heap[binding+3]));
#endif
          push_reg(reg_X);
          break;

        case BINDING_FUN:
#ifdef ONE_PASS_GENERATOR
          mov_reg_mem(reg_X, reg_glo, heap[binding+6]);
#else
          mov_reg_lbl(reg_X, heap[binding+4]);
#endif
          push_reg(reg_X);
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
      type1 = value_type(child0);
      codegen_rvalue(child0);
      grow_fs(-1);
      if (is_function_type(type1)) {
      } else if (is_pointer_type(type1)) {
        pop_reg(reg_X);
        load_mem_location(reg_X, reg_X, 0, ref_type_width(type1), is_signed_numeric_type(dereference_type(type1)));
        push_reg(reg_X);
      } else {
        fatal_error("codegen_rvalue: non-pointer is being dereferenced with *");
      }
    } else if (op == '+') {
      codegen_rvalue(child0);
      grow_fs(-1);
    } else if (op == '-') {
      codegen_rvalue(child0);
      pop_reg(reg_Y);
      grow_fs(-1);
      xor_reg_reg(reg_X, reg_X);
      sub_reg_reg(reg_X, reg_Y);
      push_reg(reg_X);
    } else if (op == '~') {
      codegen_rvalue(child0);
      pop_reg(reg_Y);
      grow_fs(-1);
      mov_reg_imm(reg_X, -1);
      xor_reg_reg(reg_X, reg_Y);
      push_reg(reg_X);
    } else if (op == '!') {
      xor_reg_reg(reg_X, reg_X);
      push_reg(reg_X);
      grow_fs(1);
      codegen_rvalue(child0);
      codegen_binop(EQ_EQ, new_ast0(INTEGER, 0), child0);
      grow_fs(-2);
    } else if (op == MINUS_MINUS_POST || op == PLUS_PLUS_POST) {
      left_width = codegen_lvalue(child0);
      pop_reg(reg_Y);
      load_mem_location(reg_X, reg_Y, 0, left_width, is_signed_numeric_type(value_type(child0)));
      push_reg(reg_X); // saves the original value of lvalue
      push_reg(reg_Y);
      push_reg(reg_X); // saves the value of lvalue to be modified
      mov_reg_imm(reg_X, 1); // Equivalent to calling codegen rvalue with INTEGER 1 (subtraction or addition handled in codegen_binop)
      push_reg(reg_X);
      codegen_binop(op, child0, new_ast0(INTEGER, 0)); // Pops two values off the stack and pushes the result
      pop_reg(reg_X); // result
      pop_reg(reg_Y); // address
      grow_fs(-1);
      write_mem_location(reg_Y, 0, reg_X, left_width); // Store the result in the address
    } else if (op == MINUS_MINUS_PRE || op == PLUS_PLUS_PRE) {
      left_width = codegen_lvalue(child0);
      pop_reg(reg_Y);
      push_reg(reg_Y);
      load_mem_location(reg_X, reg_Y, 0, left_width, is_signed_numeric_type(value_type(child0)));
      push_reg(reg_X);
      mov_reg_imm(reg_X, 1); // equivalent to calling codegen rvalue with INTEGER 1 (subtraction or addition handled in codegen_binop)
      push_reg(reg_X);
      codegen_binop(op, child0, new_ast0(INTEGER, 0)); // Pops two values off the stack and pushes the result
      pop_reg(reg_X); // result
      pop_reg(reg_Y); // address
      grow_fs(-1);
      write_mem_location(reg_Y, 0, reg_X, left_width); // store the result in the address
      push_reg(reg_X);
    } else if (op == '&') {
      codegen_lvalue(child0);
      grow_fs(-1);
    }
#ifdef SUPPORT_SIZEOF
    else if (op == SIZEOF_KW) {
      if (get_op(child0) == DECL) {
        mov_reg_imm(reg_X, type_width(get_child_(DECL, child0, 1), true, false));
      } else {
        mov_reg_imm(reg_X, type_width(value_type(child0), true, false));
      }
      push_reg(reg_X);
    }
#endif // SUPPORT_SIZEOF
    else {
      dump_node(node);
      fatal_error("codegen_rvalue: unexpected operator");
    }

  } else if (nb_children == 2) {
    if (op == '+' || op == '-' || op == '*' || op == '/' || op == '%' || op == '&' || op == '|' || op == '^' || op == LSHIFT || op == RSHIFT || op == '<' || op == '>' || op == EQ_EQ || op == EXCL_EQ || op == LT_EQ || op == GT_EQ || op == '[' || op == ',') {
      // codegen_binop expects each operand to be exactly one word on top of
      // the stack, so the temporaries of each operand are freed as soon as
      // its value is extracted.
      codegen_rvalue_and_drop_temps(child0);
      codegen_rvalue_and_drop_temps(child1);
#ifdef SUPPORT_STRUCT_UNION
      if (op == ',' && is_aggregate_type(value_type(child1))) {
        // The rhs value on top of the stack (a pointer to the aggregate) is
        // the result. The lhs word stays buried below the rhs temporaries
        // and is freed at the end of the full expression. codegen_binop's
        // pop/pop/push would corrupt the rhs temporary, so it is skipped.
        grow_fs(-1); // nets the buried lhs word with the final grow_fs(1)
      } else
#endif
      {
        codegen_binop(op, child0, child1);
        grow_fs(-2);
      }
    } else if (op == '=') {
      type1 = value_type(child0);
      left_width = codegen_lvalue(child0);
#ifdef SUPPORT_STRUCT_UNION
      if (is_struct_or_union_type(type1)) {
        // Struct assignment, we copy the struct.
        save_fs = cgc_fs;
        codegen_lvalue(child1);
        pop_reg(reg_X);
        grow_fs(-1);
        if (cgc_fs == save_fs) {
          pop_reg(reg_Y);
          grow_fs(-1);
        } else {
          // The rhs allocated temporaries (e.g. a struct-returning call) that
          // bury the destination address: load it from its stack position.
          // The buried word and the temporaries are freed at the end of the
          // full expression. The source must not be flushed before the copy,
          // so cgc_fs stays where it is.
          mov_reg_mem(reg_Y, reg_SP, (cgc_fs - save_fs) * WORD_SIZE);
        }
        copy_obj(reg_Y, 0, reg_X, 0, left_width);
      } else
#endif // SUPPORT_STRUCT_UNION
      {
        codegen_rvalue_and_drop_temps(child1); // so that the destination address is right below the value
        pop_reg(reg_X);
        pop_reg(reg_Y);
        grow_fs(-2);
        write_mem_location(reg_Y, 0, reg_X, left_width);
      }
      push_reg(reg_X);
    } else if (op == AMP_EQ || op == BAR_EQ || op == CARET_EQ || op == LSHIFT_EQ || op == MINUS_EQ || op == PERCENT_EQ || op == PLUS_EQ || op == RSHIFT_EQ || op == SLASH_EQ || op == STAR_EQ) {
      left_width = codegen_lvalue(child0);
      pop_reg(reg_Y);
      push_reg(reg_Y);
      load_mem_location(reg_X, reg_Y, 0, left_width, is_signed_numeric_type(value_type(child0)));
      push_reg(reg_X);
      grow_fs(1);
      codegen_rvalue_and_drop_temps(child1); // keep the operands and destination address adjacent
      codegen_binop(op, child0, child1);
      pop_reg(reg_X);
      pop_reg(reg_Y);
      grow_fs(-3);
      write_mem_location(reg_Y, 0, reg_X, left_width);
      push_reg(reg_X);
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
      push_reg(reg_X); // accounted by the final grow_fs(1)
    } else if (op == '(') {
      codegen_call(node);
    }
#ifdef SUPPORT_STRUCT_UNION
    else if (op == '.') {
      type1 = value_type(child0);
      if (is_struct_or_union_type(type1)) {
        type2 = get_child_(DECL, struct_member(type1, child1), 1);
        codegen_lvalue(child0);
        pop_reg(reg_Y);
        grow_fs(-1);
        // union members are at the same offset: 0
        if (get_op(type1) == STRUCT_KW) {
          add_reg_imm(reg_Y, struct_member_offset(type1, child1));
        }
        if (!is_aggregate_type(type2)) {
          load_mem_location(reg_Y, reg_Y, 0, type_width(type2, false, false), is_signed_numeric_type(type2));
        }
        push_reg(reg_Y);
      } else {
        fatal_error("codegen_rvalue: . operator on non-struct type");
      }
    } else if (op == ARROW) {
      type1 = value_type(child0);
      if (get_op(type1) == '*' && is_struct_or_union_type(get_child_('*', type1, 1))) {
        type1 = get_child_('*', type1, 1);
        type2 = get_child_(DECL, struct_member(type1, child1), 1);
        codegen_rvalue(child0);
        pop_reg(reg_Y);
        grow_fs(-1);
        // union members are at the same offset: 0
        if (get_op(type1) == STRUCT_KW) {
          add_reg_imm(reg_Y, struct_member_offset(type1, child1));
        }
        if (!is_aggregate_type(type2)) {
          load_mem_location(reg_Y, reg_Y, 0, type_width(type2, false, false), is_signed_numeric_type(type2));
        }
        push_reg(reg_Y);
      } else {
        fatal_error("codegen_rvalue: -> operator on non-struct pointer type");
      }
    }
#endif // SUPPORT_STRUCT_UNION
    else if (op == CAST) {
      codegen_rvalue(child1);
      // If the cast is to a value narrower than the width of the value, we need
      // to truncate the value. This is done by writing the value to the stack
      // and then reading it back, sign extending it if necessary.
      child0 = get_child_(DECL, child0, 1); // child 1 of cast is the type
      if (type_width(child0, false, false) < type_width(value_type(child1), false, false)) {
        load_mem_location(reg_X, reg_SP, 0, type_width(child0, false, false), is_signed_numeric_type(child0));
        grow_stack(-1);
        push_reg(reg_X);
      }
      grow_fs(-1); // grow_fs(1) is called by codegen_rvalue and at the end of the function
    } else {
      fatal_error("codegen_rvalue: unknown rvalue with 2 children");
    }

  } else if (nb_children == 3) {

    if (op == '?') {
      codegen_ternary(node, false);
    } else {
      dump_node(node);
      fatal_error("codegen_rvalue: unexpected operator");
    }

  } else {
    dump_node(node);
    fatal_error("codegen_rvalue: unexpected operator");
  }

  grow_fs(1);
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
    if (binding != 0 && heap[binding + 3] != node && get_child(heap[binding + 3], 2) != members) {
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
    pop_reg(reg_X);
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
#ifdef SUPPORT_STRUCT_UNION
  int save_fs;
#endif

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
          // The value is scalar (the type is neither an array nor a
          // struct/union), so the temporaries do get flushed and the
          // SP-relative offset stays valid for the write below.
          codegen_rvalue_and_drop_temps(car(init));
          pop_reg(reg_X);
          grow_fs(-1);
          write_mem_location(base_reg, offset, reg_X, type_width(type, true, false));
          break;
      }

      break;

#endif // SUPPORT_COMPLEX_INITIALIZER

    default:
#ifdef SUPPORT_STRUCT_UNION
      if (is_struct_or_union_type(type)) {
        // Struct assignment, we copy the struct.
        save_fs = cgc_fs;
        codegen_lvalue(init);
        pop_reg(reg_X);
        grow_fs(-1);
        // If the initializer allocated temporaries (e.g. a struct-returning
        // call), the stack pointer moved: adjust SP-relative destination
        // offsets, copy, then free the temporaries.
        if (base_reg == reg_SP) offset += (cgc_fs - save_fs) * WORD_SIZE;
        copy_obj(base_reg, offset, reg_X, 0, type_width(type, true, true));
        if (cgc_fs != save_fs) drop_stack_words(cgc_fs - save_fs);
      } else
#endif // SUPPORT_STRUCT_UNION
      if (get_op(type) != '[') {
        // The value is scalar (the type is neither an array nor a
        // struct/union), so the temporaries do get flushed and the
        // SP-relative offset stays valid for the write below.
        codegen_rvalue_and_drop_temps(init);
        pop_reg(reg_X);
        grow_fs(-1);
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
      codegen_initializer(false, init, type, reg_glo, heap[binding + 3]); // heap[binding + 3] = offset
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
  grow_stack(size); // Make room for the local variable

  if (init != 0) {
    // offset (cgc_fs - heap[cgc_locals + 3]) should be 0 since we just allocated the space
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
    codegen_initializer(false, init, type, reg_glo, heap[cgc_locals + 3]); // heap[cgc_locals + 3] = offset
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
  int save_fs = cgc_fs;
  int save_locals = cgc_locals;
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

  grow_stack(save_fs - cgc_fs);

  cgc_fs = save_fs;
  cgc_locals = save_locals;
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

    cgc_add_enclosing_switch(cgc_fs, lbl1, lbl2);
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
    lbl2 = heap[binding + 4]; // Reload because the label is overwritten by CASE statements
    def_label(lbl2);
    // If the default statement is present, we jump to it. Otherwise, we'll fall
    // through to the end of the switch and remove the switch operand from the
    // stack.
    if (heap[binding + 5]) jump(heap[binding + 5]);

    def_label(lbl3);

    // If we fell through the switch, break didn't restore the stack in its
    // original state so do it now.
    drop_stack_words(cgc_fs - heap[binding + 2]);

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
      def_label(heap[binding + 4]);           // false jump location of previous case
      heap[binding + 4] = alloc_label(0);     // create false jump location for current case
      codegen_rvalue(get_child_(CASE_KW, node, 0)); // evaluate case expression and compare it
      pop_reg(reg_Y); grow_fs(-1);
      mov_reg_mem(reg_X, reg_SP, 0);          // get switch operand without popping it
      jump_cond_reg_reg(EQ, lbl1, reg_X, reg_Y);
      jump(heap[binding + 4]);                // condition is false => jump to next case
      def_label(lbl1);                        // start of case conditional block
      codegen_statement(get_child_(CASE_KW, node, 1));  // case statement
    } else {
      fatal_error("case outside of switch");
    }

  } else if (op == DEFAULT_KW) {

    binding = cgc_lookup_enclosing_switch(cgc_locals);

    if (binding != 0) {
      if (heap[binding + 5]) fatal_error("default already defined in switch");
      heap[binding + 5] = alloc_label(0);                 // create label for default
      def_label(heap[binding + 5]);                       // default label
      codegen_statement(get_child_(DEFAULT_KW, node, 0)); // default statement
    } else {
      fatal_error("default outside of switch");
    }

  } else if (op == BREAK_KW) {

    binding = cgc_lookup_enclosing_loop_or_switch(cgc_locals);
    if (binding != 0) {
      // adjust stack and jump to break label
      grow_stack(heap[binding+2] - cgc_fs);
      jump(heap[binding+3]);
    } else {
      fatal_error("break is not in the body of a loop");
    }

  } else if (op == CONTINUE_KW) {

    binding = cgc_lookup_enclosing_loop(cgc_locals);
    if (binding != 0 && heap[binding+4] != 0) {
      // adjust stack and jump to continue label
      grow_stack(heap[binding+2] - cgc_fs);
      jump(heap[binding+4]);
    } else {
      fatal_error("continue is not in the body of a loop");
    }

  } else if (op == RETURN_KW) {

    if (get_child_(RETURN_KW, node, 0) != 0) {
      codegen_rvalue(get_child_(RETURN_KW, node, 0));
      pop_reg(reg_X);
      grow_fs(-1);
#ifdef SUPPORT_STRUCT_UNION
      if (is_struct_or_union_type(current_fun_return_type)) {
        // The value of a struct/union expression is its address: copy the
        // result into the caller-allocated buffer whose address is in the
        // hidden parameter, and leave that address in reg_X. Temporaries are
        // freed by the stack cleanup below.
        binding = cgc_lookup_var(0, cgc_locals); // hidden parameter
        mov_reg_mem(reg_Y, reg_SP, (cgc_fs - heap[binding+3]) * WORD_SIZE);
        copy_obj(reg_Y, 0, reg_X, 0, type_width(current_fun_return_type, true, false));
        mov_reg_reg(reg_X, reg_Y);
      }
#endif
    }

    // The cleanup code at the bottom isn't hit because of the ret, so cleaning here.
    grow_stack(-cgc_fs);

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

    def_goto_label(heap[binding + 3]);
    codegen_statement(get_child_(':', node, 1)); // labelled statement

  } else if (op == GOTO_KW) {

    codegen_goto(node);
#endif
  } else {

    codegen_rvalue(node);

  }

  drop_stack_words(cgc_fs - save_fs);
  cgc_locals = save_locals;
}

#ifdef SUPPORT_STRUCT_UNION
// If the function returns a struct/union, we need to add a hidden parameter for
// the return value address and store the return type in a global variable so we
// can access it in the return statement.
void add_function_hidden_params(ast fun_return_type) {
  current_fun_return_type = fun_return_type;
  if (is_struct_or_union_type(fun_return_type)) {
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
  mov_reg_lbl(reg_X, fun_binding_lbl(binding));   // heap[binding + 4] = label
  mov_mem_reg(reg_glo, heap[binding + 6], reg_X); // heap[binding + 6] = entry

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
  int save_locals_fun = cgc_locals_fun;

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
#ifdef SUPPORT_STRUCT_UNION
  // Add hidden parameter for return value address if function returns a struct
  add_function_hidden_params(fun_return_type);
#endif
  add_function_params(params);
  cgc_fs = 0;

  codegen_body(body);

  grow_stack(-cgc_fs);
  cgc_fs = 0;

  ret();

  // Register the function in the forward jump table during initialization
  init_forward_jump_table(binding);

  cgc_locals_fun = save_locals_fun;
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
  return_type = function_type(return_type, params);
  if (variadic) return_type = make_variadic_func(return_type);
  cgc_add_global_fun(init_ident(IDENTIFIER, name), lbl, return_type);
  def_label(lbl);
  codegen_builtin_movs(params);
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
  // Allocate some space for the global variables.
  //
  // By default, the global variables are placed in a mmapped region, but not
  // all systems (buider-hex0 in particular) support this syscall so pnut can
  // also place globals on the stack.
#ifdef USE_STACK_FOR_GLOBALS
  int loop_lbl = alloc_label("glo_init_loop");
  mov_reg_reg(reg_Y, reg_SP); // reg_Y = end of global variables/heap
  grow_stack_bytes(glo_size + RT_HEAP_SIZE); // reg_SP = start of globals table/heap
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

  int_type = new_ast0(INT_KW, 0);
#ifdef PARSE_NUMERIC_LITERAL_SUFFIX
  uint_type = new_ast0(INT_KW, MK_TYPE_SPECIFIER(UNSIGNED_KW));
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
  init_memory_spaces(word_size_align(cgc_global_alloc));
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
