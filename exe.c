// common part of machine code generators
const int word_size;

void generate_exe();

// 1MB heap
#define RT_HEAP_SIZE 104857600

#define MAX_CODE_SIZE 500000
int code[MAX_CODE_SIZE];
int code_alloc = 0;

void emit_i8(int a) {
  if (code_alloc >= MAX_CODE_SIZE) {
    fatal_error("code buffer overflow");
  }
  code[code_alloc] = (a & 0xff);
  code_alloc += 1;
}

void emit_2_i8(int a, int b) {
  emit_i8(a);
  emit_i8(b);
}

void emit_4_i8(int a, int b, int c, int d) {
  emit_2_i8(a, b);
  emit_2_i8(c, d);
}

void emit_i32_le(int n) {
  emit_4_i8(n, n >> 8, n >> 16, n >> 24);
}

void emit_i64_le(int n) {
  emit_i32_le(n);
  // Sign extend to 64 bits. Arithmetic shift by 31 gives -1 for negative numbers and 0 for positive numbers.
  emit_i32_le(n >> 31);
}

void emit_word_le(int n) {
  if (word_size == 4) {
    emit_i32_le(n);
  } else if (word_size == 8) {
    emit_i64_le(n);
  } else {
    fatal_error("emit_word_le: unknown word size");
  }
}

void write_i8(int n) {
  putchar(n & 0xff);
}

void write_2_i8(int a, int b) {
  write_i8(a);
  write_i8(b);
}

void write_4_i8(int a, int b, int c, int d) {
  write_2_i8(a, b);
  write_2_i8(c, d);
}

void write_i32_le(int n) {
  write_4_i8(n, n >> 8, n >> 16, n >> 24);
}

// If the main function returns a value
bool main_returns = false;

// Environment tracking
#include "env.c"

void grow_fs(int words) {
  cgc_fs += words;
}

const int reg_X;
const int reg_Y;
const int reg_Z;
const int reg_SP;
const int reg_glo;

void mov_reg_imm(int dst, int imm);
void mov_reg_reg(int dst, int src);
void mov_mem_reg(int base, int offset, int src);
void mov_mem8_reg(int base, int offset, int src);
void mov_reg_mem(int dst, int base, int offset);
void mov_reg_mem8(int dst, int base, int offset);

void add_reg_imm(int dst, int imm);
void add_reg_reg(int dst, int src);
void or_reg_reg (int dst, int src);
void and_reg_reg(int dst, int src);
void sub_reg_reg(int dst, int src);
void xor_reg_reg(int dst, int src);
void mul_reg_reg(int dst, int src);
void div_reg_reg(int dst, int src);
void rem_reg_reg(int dst, int src);
void shl_reg_reg(int dst, int src);
void sar_reg_reg(int dst, int src);

void push_reg(int src);
void pop_reg (int dst);

void jump(int lbl);
void jump_rel(int offset);
void call(int lbl);
void ret();

void dup(int reg) {
  pop_reg(reg);
  push_reg(reg);
  push_reg(reg);
  grow_fs(1);
}

void load_mem_location(int dst, int base, int offset, int width) {
  if (width == 1) {
    mov_reg_mem8(dst, base, offset);
  } else if (width == word_size) {
    mov_reg_mem(dst, base, offset);
  } else {
    fatal_error("load_mem_location: unknown width");
  }
}

// Write a value from a register to a memory location
void write_mem_location(int base, int offset, int src, int width) {
  if (width == 1) {
    mov_mem8_reg(base, offset, src);
  } else if (width == word_size) {
    mov_mem_reg(base, offset, src);
  } else {
    fatal_error("write_mem_location: unknown width");
  }
}

void copy_obj(int dst_base, int dst_offset, int src_base, int src_offset, int width) {
  int i;
  // move the words
  for (i = 0; i < width / word_size; i += 1) {
    mov_reg_mem(reg_Z, src_base, src_offset + i * word_size);
    mov_mem_reg(dst_base, dst_offset + i * word_size, reg_Z);
  }

  // then move the remaining bytes
  for (i = width - width % word_size; i < width; i += 1) {
    mov_reg_mem8(reg_Z, src_base, src_offset + i);
    mov_mem8_reg(dst_base, dst_offset + i, reg_Z);
  }
}

// Initialize a memory location with a value
void initialize_memory(int val, int base, int offset, int width) {
  int i;
  mov_reg_imm(reg_Z, val);
  for (i = 0; i < width / word_size; i += 1) {
    mov_mem_reg(base, offset + i * word_size, reg_Z);
  }
  for (i = width - width % word_size; i < width; i += 1) {
    mov_mem8_reg(base, offset + i, reg_Z);
  }
}

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
  int other_reg = reg == reg_X ? reg_Y : reg_X;

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
const int GE; // x >= y
const int LE; // x <= y
const int GT; // x > y

void jump_cond_reg_reg(int cond, int lbl, int reg1, int reg2);

void os_getchar();
void os_putchar();
void os_exit();
void os_fopen();
void os_fclose();
void os_fgetc();
void os_allocate_memory(int size);

void os_read();
void os_write();
void os_open();
void os_close();

void setup_proc_args(int global_vars_size);

#define cgc int

int setup_lbl;
int init_start_lbl;
int init_next_lbl;
int main_lbl;
int exit_lbl;
int getchar_lbl;
int putchar_lbl;
int fopen_lbl;
int fclose_lbl;
int fgetc_lbl;
int malloc_lbl;
int free_lbl;
int printf_lbl; // Stub

int read_lbl;
int write_lbl;
int open_lbl;
int close_lbl;

int round_up_to_word_size(int n) {
  return (n + word_size - 1) / word_size * word_size;
}

void grow_stack(int words) {
  add_reg_imm(reg_SP, -words * word_size);
}

// Like grow_stack, but takes bytes instead of words.
// To maintain alignment, the stack is grown by a multiple of word_size (rounded
// up from the number of bytes).
void grow_stack_bytes(int bytes) {
  add_reg_imm(reg_SP, -round_up_to_word_size(bytes));
}

// Label definition

enum {
  GENERIC_LABEL,
  GOTO_LABEL,
};

#ifdef SAFE_MODE
int labels[100000];
int labels_ix = 0;

void assert_all_labels_defined() {
  int i = 0;
  int lbl;
  // Check that all labels are defined
  for (; i < labels_ix; i++) {
    lbl = labels[i];
    if (heap[lbl + 1] > 0) {
      putstr("Label ");
      if (heap[lbl] == GENERIC_LABEL && heap[lbl + 2] != 0) {
        putstr((char*) heap[lbl + 2]);
      } else {
        putint(lbl);
      }
      putstr(" is not defined\n");
      exit(1);
    }
  }
}

void add_label(int lbl) {
  labels[labels_ix++] = lbl;
}

int alloc_label(char* name) {
  int lbl = alloc_obj(3);
  heap[lbl] = GENERIC_LABEL;
  heap[lbl + 1] = 0; // Address of label
  heap[lbl + 2] = (intptr_t) name; // Name of label
  add_label(lbl);
  return lbl;
}
#else

#define assert_all_labels_defined() // No-op
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

int alloc_goto_label() {
  int lbl = alloc_obj(3);
  heap[lbl] = GOTO_LABEL;
  heap[lbl + 1] = 0; // Address of label
  heap[lbl + 2] = 0; // cgc-fs of label
  add_label(lbl);
  return lbl;
}

void use_label(int lbl) {

  int addr = heap[lbl + 1];

  if (heap[lbl] != GENERIC_LABEL) fatal_error("use_label expects generic label");

  if (addr < 0) {
    // label address is currently known
    addr = -addr - (code_alloc + 4); // compute relative address
    emit_i32_le(addr);
  } else {
    // label address is not yet known
    emit_i32_le(0); // 32 bit placeholder for distance
    code[code_alloc-1] = addr; // chain with previous patch address
    heap[lbl + 1] = code_alloc;
  }
}

void def_label(int lbl) {

  int addr = heap[lbl + 1];
  int label_addr = code_alloc;
  int next;

  if (heap[lbl] != GENERIC_LABEL) fatal_error("def_label expects generic label");

  if (addr < 0) {
    fatal_error("label defined more than once");
  } else {
    heap[lbl + 1] = -label_addr; // define label's address
    while (addr != 0) {
      next = code[addr-1]; // get pointer to next patch address
      code_alloc = addr;
      addr = label_addr - addr; // compute relative address
      code_alloc -= 4;
      emit_i32_le(addr);
      addr = next;
    }
    code_alloc = label_addr;
  }
}

// Similar to use_label, but for gotos.
// The main difference is that it adjusts the stack and jumps, as opposed to
// simply emitting the address.
void jump_to_goto_label(int lbl) {

  int addr = heap[lbl + 1];
  int lbl_fs = heap[lbl + 2];
  int start_code_alloc = code_alloc;

  if (heap[lbl] != GOTO_LABEL) fatal_error("jump_to_goto_label expects goto label");

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
  int next;
  int goto_fs;
  int start_code_alloc;

  if (heap[lbl] != GOTO_LABEL) fatal_error("def_goto_label expects goto label");

  if (addr < 0) {
    fatal_error("goto label defined more than once");
  } else {
    heap[lbl + 1] = -label_addr; // define label's address
    heap[lbl + 2] = cgc_fs;      // define label's frame size
    while (addr != 0) {
      next = code[addr-1]; // get pointer to next patch address
      goto_fs = code[addr-2]; // get frame size at goto instruction
      code_alloc = code[addr-3]; // reset code pointer to start of jump_to_goto_label instruction
      grow_stack(cgc_fs - goto_fs); // adjust stack
      start_code_alloc = code_alloc;
      jump_rel(0); // Generate dummy jump instruction to get instruction length
      addr = label_addr - code_alloc; // compute relative address
      code_alloc = start_code_alloc;
      jump_rel(addr);
      addr = next;
    }
    code_alloc = label_addr;
  }
}

// Type, structure and union handling
int struct_union_size(ast struct_type);

// A pointer type is either an array type or a type with at least one star
bool is_pointer_type(ast type) {
  bool op = get_op(type);
  return op == '[' || op == '*';
}

bool is_struct_or_union_type(ast type) {
  int op = get_op(type);
  return op == STRUCT_KW || op == UNION_KW;
}

// An aggregate type is either an array type or a struct/union type (that's not a reference)
bool is_aggregate_type(ast type) {
  int op = get_op(type);
  return op == '[' || op == STRUCT_KW || op == UNION_KW;
}

bool is_not_pointer_type(ast type) {
  return !is_pointer_type(type);
}

// Size an object of the given type would occupy in memory (in bytes).
// If array_value is true, the size of the array is returned, otherwise the
// size of the pointer is returned.
// If word_align is true, the size is rounded up to the word size.
int type_width(ast type, bool array_value, bool word_align) {
  // Basic type kw
  switch (get_op(type)) {
    case '[':
      // In certain contexts, we want to know the static size of the array (i.e.
      // sizeof, in struct definitions, etc.) while in other contexts we care
      // about the pointer (i.e. when passing an array to a function, etc.)
      if (array_value) {
        return round_up_to_word_size(get_child_('[', type, 1) * type_width(get_child_('[', type, 0), true, false));
      } else {
        return word_size; // Array is a pointer to the first element
      }
    case '*':
      return word_size;
    case CHAR_KW:
      return word_align ? word_size : 1;
    case STRUCT_KW:
    case UNION_KW:
      return struct_union_size(type);
    case VOID_KW:
      fatal_error("type_width: void type");
      return 0;
    default:
      return word_size;
  }
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

  if (get_op(type) == STRUCT_KW && get_child_opt_(STRUCT_KW, LIST, type, 2) == 0) { // struct with empty def => reference
    binding = cgc_lookup_struct(get_val_(IDENTIFIER, get_child__(STRUCT_KW, IDENTIFIER, type, 1)), cgc_globals);
  } else if (get_op(type) == UNION_KW && get_child_opt_(UNION_KW, LIST, type, 2) == 0) { // union with empty def => reference
    binding = cgc_lookup_union(get_val_(IDENTIFIER, get_child__(UNION_KW, IDENTIFIER, type, 1)), cgc_globals);
  } else if (get_op(type) == ENUM_KW && get_child_opt_(ENUM_KW, LIST, type, 2) == 0) { // enum with empty def => reference
    binding = cgc_lookup_enum(get_val_(IDENTIFIER, get_child__(ENUM_KW, IDENTIFIER, type, 1)), cgc_globals);
  } else {
    return res;
  }

  if (binding == 0) {
    putstr("type="); putstr(STRING_BUF(get_val_(IDENTIFIER, get_child(type, 1)))); putchar('\n');
    fatal_error("canonicalize_type: type is not defined");
  }
  res = heap[binding+3];

  return res;
}

// Size of a struct or union type, rounded up to the word size
int struct_union_size(ast type) {
  ast members;
  ast member_type;
  int member_size;
  int sum_size = 0, max_size = 0;

  type = canonicalize_type(type);
  members = get_child(type, 2);

  while (members != 0) {
    member_type = get_child_(DECL, car_(DECL, members), 1);
    members = tail(members);
    member_size = type_width(member_type, true, true);
    sum_size += member_size;                            // Struct size is the sum of its members
    if (member_size > max_size) max_size = member_size; // Union size is the max of its members
  }

  // Don't need to round the size of a union to the word size since type_width already did
  return get_op(type) == STRUCT_KW ? sum_size : max_size;
}

// Find offset of struct member
int struct_member_offset_go(ast struct_type, ast member_ident) {
  ast members = get_child(canonicalize_type(struct_type), 2);
  int offset = 0;
  int sub_offset;
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
      offset += round_up_to_word_size(type_width(get_child_(DECL, decl, 1), true, true));
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
      if (ident != 0) return ident; // Found member in the anonymous struct
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

// Width of an object pointed to by a reference type.
int ref_type_width(ast type) {
  switch (get_op(type)) {
    case '[':
      return type_width(get_child_('[', type, 0), false, false); // size of inner type
    case '*':
      return type_width(get_child_('*', type, 1), false, false); // size of inner type;
    default:
      return word_size;
  }
}

ast int_type;
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
      putstr("type="); putint(get_op(type)); putchar('\n');
      fatal_error("dereference_type: non pointer is being dereferenced with *");
      return -1;
  }
}

int resolve_identifier(int ident_probe) {
  int binding = cgc_lookup_var(ident_probe, cgc_locals);
  if (binding != 0) return binding;

  binding = cgc_lookup_var(ident_probe, cgc_globals);
  if (binding != 0) return binding;

  binding = cgc_lookup_enum_value(ident_probe, cgc_globals);
  if (binding != 0) return binding;

  putstr("ident = "); putstr(STRING_BUF(ident_probe)); putchar('\n');
  fatal_error("identifier not found");
  return 0;
}

// Compute the type of an expression
ast value_type(ast node) {
  int op = get_op(node);
  int nb_children = get_nb_children(node);
  int binding;
  int ident;
  ast left_type, right_type;
  ast child0, child1;

  if (nb_children >= 1) child0 = get_child(node, 0);
  if (nb_children >= 2) child1 = get_child(node, 1);

  if (nb_children == 0) {
    if (op == INTEGER) {
      return int_type;
    } else if (op == CHARACTER) {
      return char_type;
    } else if (op == STRING) {
      return string_type;
    } else if (op == IDENTIFIER) {
      ident = get_val_(IDENTIFIER, node);
      binding = resolve_identifier(ident);
      switch (binding_kind(binding)) {
        case BINDING_PARAM_LOCAL:
        case BINDING_VAR_LOCAL:
          return heap[binding+4];
        case BINDING_VAR_GLOBAL:
          return heap[binding+4];
        case BINDING_ENUM_CST:
          return int_type;
        default:
          putstr("ident = ");
          putstr(STRING_BUF(ident));
          putchar('\n');
          fatal_error("value_type: unknown identifier");
          return -1;
      }

    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("value_type: unknown expression with nb_children == 0");
      return -1;
    }

  } else if (nb_children == 1) {

    if (op == '*') {
      left_type = value_type(child0);
      return dereference_type(left_type);
    } else if (op == '&') {
      left_type = value_type(child0);
      return pointer_type(left_type, false);
    } else if (op == '+' || op == '-' || op == '~' || op == '!' || op == MINUS_MINUS || op == PLUS_PLUS || op == MINUS_MINUS_POST || op == PLUS_PLUS_POST || op == PLUS_PLUS_PRE || op == MINUS_MINUS_PRE || op == PARENS) {
      // Unary operation don't change the type
      return value_type(child0);
    } else if (op == SIZEOF_KW) {
      return int_type; // sizeof always returns an integer
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("value_type: unexpected operator");
      return -1;
    }

  } else if (nb_children == 2) {

    if (op == '+' || op == '-' || op == '*' || op == '/' || op == '%' || op == '&' || op == '|' || op == '^'
     || op == LSHIFT || op == RSHIFT || op == '<' || op == '>' || op == EQ_EQ || op == EXCL_EQ || op == LT_EQ || op == GT_EQ) {
      left_type = value_type(child0);
      right_type = value_type(child1);
      if (is_pointer_type(left_type) && is_pointer_type(right_type) && op == '-') {
        return int_type; // Pointer - Pointer = Integer
      } else if (is_pointer_type(left_type)) {
        // if left is an array or a pointer, the type is also a pointer
        return left_type;
      } else {
        // if left is not a pointer, the type is the type of the right operand
        return right_type;
      }
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
        putstr("left_type="); putint(get_op(left_type)); putchar('\n');
        putstr("right_type="); putint(get_op(right_type)); putchar('\n');
        fatal_error("value_type: non pointer is being dereferenced as array");
        return -1;
      }
    } else if (op == '=' || op == AMP_EQ || op == BAR_EQ || op == CARET_EQ || op == LSHIFT_EQ || op == MINUS_EQ || op == PERCENT_EQ || op == PLUS_EQ || op == RSHIFT_EQ || op == SLASH_EQ || op == STAR_EQ) {
      return value_type(child0); // Only the left side is relevant here
    } else if (op == AMP_AMP || op == BAR_BAR) {
      // TODO: Check that the operands have compatible types?
      return value_type(child0);
    } else if (op == '(') {
      binding = cgc_lookup_fun(get_val_(IDENTIFIER, child0), cgc_globals);
      if (binding != 0) {
        return heap[binding+5];
      } else {
        putstr("ident = ");
        putstr(STRING_BUF(get_val_(IDENTIFIER, child0)));
        putchar('\n');
        fatal_error("value_type: function not found");
        return -1;
      }
    } else if (op == '.') {
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
    } else if (op == CAST) {
      return get_child_(DECL, child0, 1);
    } else {
      fatal_error("value_type: unknown expression with 2 children");
      return -1;
    }

  } else if (nb_children == 3) {

    if (op == '?') {
      // We assume that the 2 cases have the same type.
      return value_type(child1);
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("value_type: unknown expression with 3 children");
      return -1;
    }

  } else {
    putstr("op="); putint(op); putchar('\n');
    fatal_error("value_type: unknown expression with >4 children");
    return -1;
  }
}

void codegen_binop(int op, ast lhs, ast rhs) {

  int lbl1;
  int lbl2;
  int cond = -1;
  ast left_type = value_type(lhs);
  ast right_type = value_type(rhs);
  int width;

  pop_reg(reg_Y); // rhs operand
  pop_reg(reg_X); // lhs operand

  if      (op == '<')     cond = LT;
  else if (op == '>')     cond = GT;
  else if (op == EQ_EQ)   cond = EQ;
  else if (op == EXCL_EQ) cond = NE;
  else if (op == LT_EQ)   cond = LE;
  else if (op == GT_EQ)   cond = GE;

  if (cond != -1) {

    lbl1 = alloc_label(0);
    lbl2 = alloc_label(0);
    jump_cond_reg_reg(cond, lbl1, reg_X, reg_Y);
    xor_reg_reg(reg_X, reg_X);
    jump(lbl2);
    def_label(lbl1);
    mov_reg_imm(reg_X, 1);
    def_label(lbl2);

  } else {
    if (op == '+' || op == PLUS_EQ || op == PLUS_PLUS_PRE || op == PLUS_PLUS_POST) {
      // Check if one of the operands is a pointer
      // If so, multiply the other operand by the width of the pointer target object.

      if (is_pointer_type(left_type) && is_not_pointer_type(right_type)) {
        mul_for_pointer_arith(reg_Y, ref_type_width(left_type));
      }

      if (is_pointer_type(right_type) && is_not_pointer_type(left_type)) {
        mul_for_pointer_arith(reg_X, ref_type_width(right_type));
      }

      add_reg_reg(reg_X, reg_Y);
    }
    else if (op == '-' || op == MINUS_EQ || op == MINUS_MINUS_PRE || op == MINUS_MINUS_POST) {
      // Pointer subtraction is only valid if one of the operands is a pointer
      // When both operands are pointers, the result is the difference between the two pointers divided by the width of the target object.
      // When one operand is a pointer and the other is an integer, the result is the pointer minus the integer times the width of the target object.

      if (is_pointer_type(left_type) && is_pointer_type(right_type)) {
        sub_reg_reg(reg_X, reg_Y);
        div_for_pointer_arith(reg_X, ref_type_width(left_type));
      } else if (is_pointer_type(left_type)) {
        mul_for_pointer_arith(reg_Y, ref_type_width(left_type));
        sub_reg_reg(reg_X, reg_Y);
      } else if (is_pointer_type(right_type)) {
        mul_for_pointer_arith(reg_X, ref_type_width(right_type));
        sub_reg_reg(reg_X, reg_Y);
      } else {
        sub_reg_reg(reg_X, reg_Y);
      }
    }
    else if (op == '*' || op == STAR_EQ)      mul_reg_reg(reg_X, reg_Y);
    else if (op == '/' || op == SLASH_EQ)     div_reg_reg(reg_X, reg_Y);
    else if (op == '%' || op == PERCENT_EQ)   rem_reg_reg(reg_X, reg_Y);
    else if (op == '&' || op == AMP_EQ)       and_reg_reg(reg_X, reg_Y);
    else if (op == '|' || op == BAR_EQ)       or_reg_reg(reg_X, reg_Y);
    else if (op == '^' || op == CARET_EQ)     xor_reg_reg(reg_X, reg_Y);
    else if (op == LSHIFT || op == LSHIFT_EQ) shl_reg_reg(reg_X, reg_Y);
    else if (op == RSHIFT || op == RSHIFT_EQ) sar_reg_reg(reg_X, reg_Y);
    else if (op == ',')                       mov_reg_reg(reg_X, reg_Y); // Ignore lhs and keep rhs
    else if (op == '[') {
      // Same as pointer addition for address calculation
      if (is_pointer_type(left_type) && is_not_pointer_type(right_type)) {
        mul_for_pointer_arith(reg_Y, ref_type_width(left_type));
        width = ref_type_width(left_type);
      } else if (is_pointer_type(right_type) && is_not_pointer_type(left_type)) {
        mul_for_pointer_arith(reg_X, ref_type_width(right_type));
        width = ref_type_width(right_type);
      } else {
        fatal_error("codegen_binop: invalid array access operands");
        return;
      }

      add_reg_reg(reg_X, reg_Y);
      load_mem_location(reg_X, reg_X, 0, width);
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("codegen_binop: unknown op");
    }
  }

  push_reg(reg_X);
}

void codegen_rvalue(ast node);
void codegen_statement(ast node);
int codegen_lvalue(ast node);

int codegen_param(ast param) {
  int type = value_type(param);
  int left_width;

  if (is_struct_or_union_type(type)) {
    left_width = codegen_lvalue(param);
    pop_reg(reg_X);
    grow_fs(-1);
    grow_stack_bytes(round_up_to_word_size(left_width));
    grow_fs(round_up_to_word_size(left_width) / word_size);
    copy_obj(reg_SP, 0, reg_X, 0, left_width);
  } else {
    codegen_rvalue(param);
  }

  return type_width(type, false, true) / word_size;
}

int codegen_params(ast params) {

  int fs = 0;

  if (params != 0) {
    fs = codegen_params(get_child_opt_(LIST, LIST, params, 1));
    fs += codegen_param(get_child_(LIST, params, 0));
  }

  return fs;
}

void codegen_call(ast node) {
  ast fun_ident = get_child__('(', IDENTIFIER, node, 0);
  ast ident_probe = get_val_(IDENTIFIER, fun_ident);
  ast params = get_child(node, 1);
  ast nb_params = codegen_params(params);

  int binding = cgc_lookup_fun(ident_probe, cgc_globals);

  if (binding == 0) {
    putstr("ident = ");
    putstr(STRING_BUF(ident_probe));
    putchar('\n');
    fatal_error("codegen_call: function not found");
  }

  call(heap[binding+4]);

  grow_stack(-nb_params);
  grow_fs(-nb_params);

  push_reg(reg_X);
}

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
          mov_reg_imm(reg_X, (cgc_fs - heap[binding+3]) * word_size);
          add_reg_reg(reg_X, reg_SP);
          push_reg(reg_X);
          break;
        case BINDING_VAR_GLOBAL:
          mov_reg_imm(reg_X, heap[binding+3]);
          add_reg_reg(reg_X, reg_glo);
          push_reg(reg_X);
          break;
        default:
          fatal_error("codegen_lvalue: identifier not found");
          break;
      }
      lvalue_width = type_width(heap[binding+4], true, true);
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("codegen_lvalue: unknown lvalue with nb_children == 0");
    }

  } else if (nb_children == 1) {

    if (op == '*') {
      codegen_rvalue(child0);
      grow_fs(-1);
      lvalue_width = ref_type_width(value_type(child0));
    } else if (op == PARENS) {
      lvalue_width = codegen_lvalue(child0);
      grow_fs(-1);
    } else {
      putstr("op="); putint(op); putchar('\n');
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
    } else if (op == '.') {
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
        lvalue_width = type_width(get_child_(DECL, struct_member(type, child1), 1), true, true); // child 1 of member is the type
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
        lvalue_width = type_width(get_child_(DECL, struct_member(type, child1), 1), true, true); // child 1 of member is the type
      } else {
        fatal_error("codegen_lvalue: -> operator on non-struct pointer type");
      }
    } else if (op == CAST) {
      codegen_lvalue(child1);
      lvalue_width = type_width(child0, true, true);
      grow_fs(-1); // grow_fs is called at the end of the function, so we need to decrement it here
    } else {
      fatal_error("codegen_lvalue: unknown lvalue with 2 children");
    }

  } else {
    putstr("op="); putint(op); putchar('\n');
    fatal_error("codegen_lvalue: unknown lvalue with >2 children");
  }

  if (lvalue_width == 0) {
    fatal_error("codegen_lvalue: lvalue_width == 0");
  }

  grow_fs(1);
  return lvalue_width;
}

void codegen_string(int string_probe) {
  int lbl = alloc_label(0);
  char *string_start = STRING_BUF(string_probe);
  char *string_end = string_start + heap[string_probe + 4];

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
  ast type1, type2;
  ast child0, child1;

  if (nb_children >= 1) child0 = get_child(node, 0);
  if (nb_children >= 2) child1 = get_child(node, 1);

  if (nb_children == 0) {
    if (op == INTEGER) {
      mov_reg_imm(reg_X, -get_val_(INTEGER, node));
      push_reg(reg_X);
    } else if (op == CHARACTER) {
      mov_reg_imm(reg_X, get_val_(CHARACTER, node));
      push_reg(reg_X);
    } else if (op == IDENTIFIER) {
      binding = resolve_identifier(get_val_(IDENTIFIER, node));
      switch (binding_kind(binding)) {
        case BINDING_PARAM_LOCAL:
        case BINDING_VAR_LOCAL:
          mov_reg_imm(reg_X, (cgc_fs - heap[binding+3]) * word_size);
          add_reg_reg(reg_X, reg_SP);
          // local arrays/structs/unions are allocated on the stack, so no need to dereference
          if (get_op(heap[binding+4]) != '[' && get_op(heap[binding+4]) != STRUCT_KW && get_op(heap[binding+4]) != UNION_KW) {
            mov_reg_mem(reg_X, reg_X, 0);
          }
          push_reg(reg_X);
          break;
        case BINDING_VAR_GLOBAL:
          mov_reg_imm(reg_X, heap[binding+3]);
          add_reg_reg(reg_X, reg_glo);
          // global arrays/structs/unions are also allocated on the stack, so no need to dereference
          if (get_op(heap[binding+4]) != '[' && get_op(heap[binding+4]) != STRUCT_KW && get_op(heap[binding+4]) != UNION_KW) {
            mov_reg_mem(reg_X, reg_X, 0);
          }
          push_reg(reg_X);
          break;
        case BINDING_ENUM_CST:
          mov_reg_imm(reg_X, -get_val_(INTEGER, heap[binding+3]));
          push_reg(reg_X);
          break;

        default:
          putstr("ident = "); putstr(STRING_BUF(get_val_(IDENTIFIER, node))); putchar('\n');
          fatal_error("codegen_rvalue: identifier not found");
          break;
      }
    } else if (op == STRING) {
      codegen_string(get_val_(STRING, node));
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("codegen_rvalue: unknown rvalue with nb_children == 0");
    }

  } else if (nb_children == 1) {
    if (op == '*') {
      codegen_rvalue(child0);
      pop_reg(reg_Y);
      grow_fs(-1);
      if (is_pointer_type(value_type(child0))) {
        load_mem_location(reg_X, reg_Y, 0, ref_type_width(value_type(child0)));
      } else {
        fatal_error("codegen_rvalue: non-pointer is being dereferenced with *");
      }
      push_reg(reg_X);
    } else if (op == '+' || op == PARENS) {
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
    } else if (op == MINUS_MINUS_POST || op == PLUS_PLUS_POST){
      codegen_lvalue(child0);
      pop_reg(reg_Y);
      mov_reg_mem(reg_X, reg_Y, 0);
      push_reg(reg_X); // saves the original value of lvalue
      push_reg(reg_Y);
      push_reg(reg_X); // saves the value of lvalue to be modified
      mov_reg_imm(reg_X, 1); // Equivalent to calling codegen rvalue with INTEGER 1 (subtraction or addition handled in codegen_binop)
      push_reg(reg_X);
      codegen_binop(op, child0, new_ast0(INTEGER, 0)); // Pops two values off the stack and pushes the result
      pop_reg(reg_X); // result
      pop_reg(reg_Y); // address
      grow_fs(-1);
      mov_mem_reg(reg_Y, 0, reg_X); // Store the result in the address
    } else if (op == MINUS_MINUS_PRE || op == PLUS_PLUS_PRE) {
      codegen_lvalue(child0);
      pop_reg(reg_Y);
      push_reg(reg_Y);
      mov_reg_mem(reg_X, reg_Y, 0);
      push_reg(reg_X);
      grow_fs(1);
      mov_reg_imm(reg_X, 1); // equivalent to calling codegen rvalue with INTEGER 1 (subtraction or addition handled in codegen_binop)
      push_reg(reg_X);
      grow_fs(1);
      codegen_binop(op, child0, new_ast0(INTEGER, 0)); // Pops two values off the stack and pushes the result
      pop_reg(reg_X); // result
      pop_reg(reg_Y); // address
      grow_fs(-3);
      mov_mem_reg(reg_Y, 0, reg_X); //store the result in the address
      push_reg(reg_X);
    } else if (op == '&') {
      codegen_lvalue(child0);
      grow_fs(-1);
    } else if (op == SIZEOF_KW) {
      if (get_op(child0) == DECL) {
        mov_reg_imm(reg_X, type_width(get_child_(DECL, child0, 1), true, false));
      } else {
        mov_reg_imm(reg_X, type_width(value_type(child0), true, false));
      }
      push_reg(reg_X);
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("codegen_rvalue: unexpected operator");
    }

  } else if (nb_children == 2) {
    if (op == '+' || op == '-' || op == '*' || op == '/' || op == '%' || op == '&' || op == '|' || op == '^' || op == LSHIFT || op == RSHIFT || op == '<' || op == '>' || op == EQ_EQ || op == EXCL_EQ || op == LT_EQ || op == GT_EQ || op == '[' || op == ',') {
      codegen_rvalue(child0);
      codegen_rvalue(child1);
      codegen_binop(op, child0, child1);
      grow_fs(-2);
    } else if (op == '=') {
      type1 = value_type(child0);
      left_width = codegen_lvalue(child0);
      if (is_struct_or_union_type(type1)) {
        // Struct assignment, we copy the struct.
        codegen_lvalue(child1);
        pop_reg(reg_X);
        pop_reg(reg_Y);
        grow_fs(-2);
        copy_obj(reg_Y, 0, reg_X, 0, left_width);
      } else {
        codegen_rvalue(child1);
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
      load_mem_location(reg_X, reg_Y, 0, left_width);
      push_reg(reg_X);
      grow_fs(1);
      codegen_rvalue(child1);
      codegen_binop(op, child0, child1);
      pop_reg(reg_X);
      pop_reg(reg_Y);
      grow_fs(-3);
      write_mem_location(reg_Y, 0, reg_X, left_width);
      push_reg(reg_X);
    } else if (op == AMP_AMP || op == BAR_BAR) {
      lbl1 = alloc_label(0);
      codegen_rvalue(child0);
      pop_reg(reg_X);
      push_reg(reg_X);
      xor_reg_reg(reg_Y, reg_Y);
      if (op == AMP_AMP) {
        jump_cond_reg_reg(EQ, lbl1, reg_X, reg_Y);
      } else {
        jump_cond_reg_reg(NE, lbl1, reg_X, reg_Y);
      }
      pop_reg(reg_X); grow_fs(-1);
      codegen_rvalue(child1);
      grow_fs(-1);
      def_label(lbl1);
    } else if (op == '(') {
      codegen_call(node);
    } else if (op == '.') {
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
          load_mem_location(reg_Y, reg_Y, 0, type_width(type2, false, false));
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
          load_mem_location(reg_Y, reg_Y, 0, word_size);
        }
        push_reg(reg_Y);
      } else {
        fatal_error("codegen_rvalue: -> operator on non-struct pointer type");
      }
    } else if (op == CAST) {
      codegen_rvalue(child1);
      grow_fs(-1); // grow_fs(1) is called by codegen_rvalue and at the end of the function
    } else {
      fatal_error("codegen_rvalue: unknown rvalue with 2 children");
    }

  } else if (nb_children == 3) {

    if (op == '?') {
      lbl1 = alloc_label(0); // false label
      lbl2 = alloc_label(0); // end label
      codegen_rvalue(child0);
      pop_reg(reg_X);
      grow_fs(-1);
      xor_reg_reg(reg_Y, reg_Y);
      jump_cond_reg_reg(EQ, lbl1, reg_X, reg_Y);
      codegen_rvalue(child1); // value when true
      jump(lbl2);
      def_label(lbl1);
      grow_fs(-1); // here, the child#1 is not on the stack, so we adjust it
      codegen_rvalue(get_child(node, 2)); // value when false
      grow_fs(-1); // grow_fs(1) is called by codegen_rvalue and at the end of the function
      def_label(lbl2);
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("codegen_rvalue: unknown rvalue with 3 children");
    }

  } else {
    putstr("op="); putint(op); putchar('\n');
    fatal_error("codegen_rvalue: unknown rvalue with >4 children");
  }

  grow_fs(1);
}

void codegen_begin() {

  setup_lbl = alloc_label("setup");
  init_start_lbl = alloc_label("init_start");
  init_next_lbl = init_start_lbl;

  // Make room for heap start and malloc bump pointer.
  // reg_glo[0]: heap start
  // reg_glo[word_size]: malloc bump pointer
  cgc_global_alloc += 2 * word_size;

  int_type = new_ast0(INT_KW, 0);
  char_type = new_ast0(CHAR_KW, 0);
  string_type = pointer_type(new_ast0(CHAR_KW, 0), false);
  void_type = new_ast0(VOID_KW, 0);
  void_star_type = pointer_type(new_ast0(VOID_KW, 0), false);

  main_lbl = alloc_label("main");
  cgc_add_global_fun(init_ident(IDENTIFIER, "main"), main_lbl, void_type);

  exit_lbl = alloc_label("exit");
  cgc_add_global_fun(init_ident(IDENTIFIER, "exit"), exit_lbl, void_type);

  getchar_lbl = alloc_label("getchar");
  cgc_add_global_fun(init_ident(IDENTIFIER, "getchar"), getchar_lbl, char_type);

  putchar_lbl = alloc_label("putchar");
  cgc_add_global_fun(init_ident(IDENTIFIER, "putchar"), putchar_lbl, void_type);

  fopen_lbl = alloc_label("fopen");
  cgc_add_global_fun(init_ident(IDENTIFIER, "fopen"), fopen_lbl, int_type);

  fclose_lbl = alloc_label("fclose");
  cgc_add_global_fun(init_ident(IDENTIFIER, "fclose"), fclose_lbl, void_type);

  fgetc_lbl = alloc_label("fgetc");
  cgc_add_global_fun(init_ident(IDENTIFIER, "fgetc"), fgetc_lbl, char_type);

  malloc_lbl = alloc_label("malloc");
  cgc_add_global_fun(init_ident(IDENTIFIER, "malloc"), malloc_lbl, void_star_type);

  free_lbl = alloc_label("free");
  cgc_add_global_fun(init_ident(IDENTIFIER, "free"), free_lbl, char_type);

  read_lbl = alloc_label("read");
  cgc_add_global_fun(init_ident(IDENTIFIER, "read"), read_lbl, int_type);

  write_lbl = alloc_label("write");
  cgc_add_global_fun(init_ident(IDENTIFIER, "write"), write_lbl, int_type);

  open_lbl = alloc_label("open");
  cgc_add_global_fun(init_ident(IDENTIFIER, "open"), open_lbl, int_type);

  close_lbl = alloc_label("close");
  cgc_add_global_fun(init_ident(IDENTIFIER, "close"), close_lbl, int_type);

  printf_lbl = alloc_label("printf");
  cgc_add_global_fun(init_ident(IDENTIFIER, "printf"), printf_lbl, void_type);

  jump(setup_lbl);
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
    cgc_add_enum(get_val_(IDENTIFIER, get_child__('=', IDENTIFIER, cas, 0)), get_child__('=', INTEGER, cas, 1));
    cases = tail(cases);
  }
}

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

void handle_enum_struct_union_type_decl(ast type) {
  if (get_op(type) == ENUM_KW) {
    codegen_enum(type);
  } else if (get_op(type) == STRUCT_KW) {
    codegen_struct_or_union(type, BINDING_TYPE_STRUCT);
  } else if (get_op(type) == UNION_KW) {
    codegen_struct_or_union(type, BINDING_TYPE_UNION);
  }

  // If not an enum, struct, or union, do nothing
}

void codegen_initializer_string(int string_probe, ast type, int base_reg, int offset) {
  char *string_start = STRING_BUF(string_probe);
  int i = 0;
  int str_len = heap[string_probe + 4];
  int arr_len;

  // Only acceptable types are char[] or char*
  if (get_op(type) == '[' && get_op(get_child_('[', type, 0)) == CHAR_KW) {
    arr_len = get_child_('[', type, 1);
    if (str_len >= arr_len) fatal_error("codegen_initializer: string initializer is too long for char[]");

    // Place the bytes of the string in the memory location allocated for the array
    for (; i < arr_len; i += 1) {
      mov_reg_imm(reg_X, i < str_len ? string_start[i] : 0);
      write_mem_location(base_reg, offset + i, reg_X, 1);
    }
  } else if (get_op(type) == '*' && get_op(get_child_('*', type, 1)) == CHAR_KW) {
    // Create the string and assign global variable to the pointer
    codegen_string(string_probe);
    pop_reg(reg_X);
    write_mem_location(base_reg, offset, reg_X, word_size);
  } else {
    fatal_error("codegen_initializer: string initializer must be assigned to a char[] or char*");
  }
}

// Initialize a variable with an initializer
void codegen_initializer(bool local, ast init, ast type, int base_reg, int offset, bool in_array) {
  ast members;
  ast inner_type;
  int arr_len;
  int inner_type_width;

  type = canonicalize_type(type);

  switch (get_op(init)) {
    case STRING:
      codegen_initializer_string(get_val_(STRING, init), type, base_reg, offset);
      break;

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
            codegen_initializer(local, car(init), inner_type, base_reg, offset, true);
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

        case STRUCT_KW:
          members = get_child_(STRUCT_KW, type, 2);
          while (init != 0 && members != 0) {
            inner_type = get_child_(DECL, car_(DECL, members), 1);
            codegen_initializer(local, car(init), inner_type, base_reg, offset, false);
            offset += type_width(inner_type, true, true);
            init = tail(init);
            members = tail(members);
          }

          //  Initialize rest of the members to 0
          while (local && members != 0) {
            inner_type = get_child_(DECL, car_(DECL, members), 1);
            initialize_memory(0, base_reg, offset, type_width(inner_type, true, true));
            offset += type_width(inner_type, true, true);
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
          codegen_initializer(local, car(init), get_child_(DECL, car_(DECL, members), 1), base_reg, offset, false);
          break;

        default:
          if (tail(init) != 0 // More than 1 element
           || get_op(car(init)) == INITIALIZER_LIST) { // Or nested initializer list
            fatal_error("codegen_initializer: scalar initializer list has more than one element");
          }
          codegen_rvalue(car(init));
          pop_reg(reg_X);
          grow_fs(-1);
          write_mem_location(base_reg, offset, reg_X, type_width(type, true, !in_array));
          break;
      }

      break;

    default:
      if (is_struct_or_union_type(type)) {
        // Struct assignment, we copy the struct.
        codegen_lvalue(init);
        pop_reg(reg_X);
        grow_fs(-1);
        copy_obj(base_reg, offset, reg_X, 0, type_width(type, true, true));
      } else if (get_op(type) != '[') {
        codegen_rvalue(init);
        pop_reg(reg_X);
        grow_fs(-1);
        write_mem_location(base_reg, offset, reg_X, type_width(type, true, !in_array));
      } else {
        fatal_error("codegen_initializer: cannot initialize array with scalar value");
      }
      break;
  }
}

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
      return heap[get_val_(STRING, initializer) + 4] + 1;

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

void codegen_glo_var_decl(ast node) {
  ast name = get_child__(DECL, IDENTIFIER, node, 0);
  ast type = get_child_(DECL, node, 1);
  ast init = get_child_(DECL, node, 2);
  int name_probe = get_val_(IDENTIFIER, name);
  int binding = cgc_lookup_var(name_probe, cgc_globals);

  if (get_op(type) == '(') {
    // Forward declaration
    binding = cgc_lookup_fun(name_probe, cgc_globals);
    if (binding == 0) cgc_add_global_fun(name_probe, alloc_label(STRING_BUF(name_probe)), type);

  } else {
    handle_enum_struct_union_type_decl(type);
    infer_array_length(type, init);

    if (binding == 0) {
      cgc_add_global(name_probe, type_width(type, true, true), type);
      binding = cgc_globals;
    }

    if (init != 0) {
      def_label(init_next_lbl);
      init_next_lbl = alloc_label("init_next");
      codegen_initializer(false, init, type, reg_glo, heap[binding + 3], false); // heap[binding + 3] = offset
      jump(init_next_lbl);
    }
  }
}

void codegen_local_var_decl(ast node) {
  ast name = get_child__(DECL, IDENTIFIER, node, 0);
  ast type = get_child_(DECL, node, 1);
  ast init = get_child_(DECL, node, 2);
  int size;

  infer_array_length(type, init);

  if (is_aggregate_type(type)) { // Array/struct/union declaration
    size = type_width(type, true, true) / word_size;  // size in bytes (word aligned)
  } else {
    size = 1;
  }

  cgc_add_local_var(get_val_(IDENTIFIER, name), size, type);
  grow_stack(size); // Make room for the local variable

  if (init != 0) {
    // offset (cgc_fs - heap[cgc_locals + 3]) should be 0 since we just allocated the space
    codegen_initializer(true, init, type, reg_SP, 0, false);
  }
}

void codegen_body(ast node) {
  int save_fs = cgc_fs;
  int save_locals = cgc_locals;
  ast stmt;
  ast declarations;

  while (node != 0) {
    stmt = get_child_('{', node, 0);
    if (get_op(stmt) == DECLS) { // Variable declaration
      declarations = get_child__(DECLS, LIST, stmt, 0);
      while (declarations != 0) { // Multiple variable declarations
        codegen_local_var_decl(car_(DECL, declarations));
        declarations = tail(declarations);
      }
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
  int save_fs;
  int save_locals;
  int binding;

  if (node == 0) return;

  op = get_op(node);

  if (op == IF_KW) {

    lbl1 = alloc_label(0); // else statement
    lbl2 = alloc_label(0); // join point after if
    codegen_rvalue(get_child_(IF_KW, node, 0));
    pop_reg(reg_X);
    grow_fs(-1);
    xor_reg_reg(reg_Y, reg_Y);
    jump_cond_reg_reg(EQ, lbl1, reg_X, reg_Y);
    codegen_statement(get_child_(IF_KW, node, 1));
    jump(lbl2);
    def_label(lbl1);
    codegen_statement(get_child_(IF_KW, node, 2));
    def_label(lbl2);

  } else if (op == WHILE_KW) {

    lbl1 = alloc_label(0); // while statement start
    lbl2 = alloc_label(0); // join point after while

    save_fs = cgc_fs;
    save_locals = cgc_locals;

    cgc_add_enclosing_loop(cgc_fs, lbl2, lbl1);

    def_label(lbl1);
    codegen_rvalue(get_child_(WHILE_KW, node, 0));
    pop_reg(reg_X);
    grow_fs(-1);
    xor_reg_reg(reg_Y, reg_Y);
    jump_cond_reg_reg(EQ, lbl2, reg_X, reg_Y);
    codegen_statement(get_child_(WHILE_KW, node, 1));
    jump(lbl1);
    def_label(lbl2);

    cgc_fs = save_fs;
    cgc_locals = save_locals;

  } else if (op == FOR_KW) {

    lbl1 = alloc_label(0); // while statement start
    lbl2 = alloc_label(0); // join point after while
    lbl3 = alloc_label(0); // initial loop starting point

    save_fs = cgc_fs;
    save_locals = cgc_locals;

    cgc_add_enclosing_loop(cgc_fs, lbl2, lbl1);

    codegen_statement(get_child_(FOR_KW, node, 0)); // init
    jump(lbl3); // skip post loop action
    def_label(lbl1);
    codegen_statement(get_child_(FOR_KW, node, 2)); // post loop action
    def_label(lbl3);
    codegen_rvalue(get_child_(FOR_KW, node, 1)); // test
    pop_reg(reg_X);
    grow_fs(-1);
    xor_reg_reg(reg_Y, reg_Y);
    jump_cond_reg_reg(EQ, lbl2, reg_X, reg_Y);
    codegen_statement(get_child_(FOR_KW, node, 3));
    jump(lbl1);
    def_label(lbl2);

    cgc_fs = save_fs;
    cgc_locals = save_locals;

  } else if (op == DO_KW) {

    lbl1 = alloc_label(0); // do statement start
    lbl2 = alloc_label(0); // break point

    save_fs = cgc_fs;
    save_locals = cgc_locals;

    cgc_add_enclosing_loop(cgc_fs, lbl2, lbl1);
    def_label(lbl1);
    codegen_statement(get_child_(DO_KW, node, 0));
    codegen_rvalue(get_child_(DO_KW, node, 1));
    pop_reg(reg_X);
    grow_fs(-1);
    xor_reg_reg(reg_Y, reg_Y);
    jump_cond_reg_reg(NE, lbl1, reg_X, reg_Y);

    def_label(lbl2);

    cgc_fs = save_fs;
    cgc_locals = save_locals;

  } else if (op == SWITCH_KW) {

    save_fs = cgc_fs;
    save_locals = cgc_locals;

    lbl1 = alloc_label(0); // lbl1: end of switch
    lbl2 = alloc_label(0); // lbl2: next case

    cgc_add_enclosing_switch(cgc_fs, lbl1, lbl2);
    binding = cgc_locals;

    codegen_rvalue(get_child_(SWITCH_KW, node, 0));    // switch operand
    jump(lbl2);                                        // Jump to first case
    codegen_statement(get_child_(SWITCH_KW, node, 1)); // switch body

    // false jump location of last case
    // Reload because the label is updated when a new case is added
    lbl2 = heap[binding + 4];
    if (heap[lbl2 + 1] >= 0) {
      def_label(lbl2); // No case statement => jump to end of switch
    }

    // If we fell through the switch, we need to remove the switch operand.
    // This is done before lbl1 because the stack is adjusted before the break statement.
    grow_stack(-1);
    grow_fs(-1);

    def_label(lbl1); // End of switch label

    cgc_fs = save_fs;
    cgc_locals = save_locals;

  } else if (op == CASE_KW) {

    binding = cgc_lookup_enclosing_switch(cgc_locals);

    if (binding != 0) {
      lbl1 = alloc_label(0);                   // skip case when falling through
      jump(lbl1);
      def_label(heap[binding + 4]);           // false jump location of previous case
      heap[binding + 4] = alloc_label(0);     // create false jump location for current case
      dup(reg_X);                             // duplicate switch operand for the comparison
      codegen_rvalue(get_child_(CASE_KW, node, 0)); // evaluate case expression and compare it
      pop_reg(reg_Y); pop_reg(reg_X); grow_fs(-2);
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
      def_label(heap[binding + 4]);           // false jump location of previous case
      heap[binding + 4] = alloc_label(0);     // create label for next case (even if default catches all cases)
      codegen_statement(get_child_(DEFAULT_KW, node, 0));  // default statement
    } else {
      fatal_error("default outside of switch");
    }

  } else if (op == BREAK_KW) {

    binding = cgc_lookup_enclosing_loop_or_switch(cgc_locals);
    if (binding != 0) {
      grow_stack(heap[binding+2] - cgc_fs);
      jump(heap[binding+3]); // jump to break label
    } else {
      fatal_error("break is not in the body of a loop");
    }

  } else if (op == CONTINUE_KW) {

    binding = cgc_lookup_enclosing_loop(cgc_locals);
    if (binding != 0 && heap[binding+4] != 0) {
      grow_stack(heap[binding+2] - cgc_fs);
      jump(heap[binding+4]); // jump to continue label
    } else {
      fatal_error("continue is not in the body of a loop");
    }

  } else if (op == RETURN_KW) {

    if (get_child_(RETURN_KW, node, 0) != 0) {
      codegen_rvalue(get_child_(RETURN_KW, node, 0));
      pop_reg(reg_X);
      grow_fs(-1);
    }

    grow_stack(-cgc_fs);

    ret();

  } else if (op == '{') {

    codegen_body(node);

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

  } else {

    codegen_rvalue(node);
    pop_reg(reg_X);
    grow_fs(-1);

  }
}

void add_params(ast params) {
  ast decl, type;
  int ident;

  while (params != 0) {
    decl = car_(DECL, params);
    ident = get_val_(IDENTIFIER, get_child__(DECL, IDENTIFIER, decl, 0));
    type = get_child_(DECL, decl, 1);

    if (cgc_lookup_var(ident, cgc_locals) != 0) fatal_error("add_params: duplicate parameter");

    cgc_add_local_param(ident, type_width(type, false, true) / word_size, type);
    params = tail(params);
  }
}

void codegen_glo_fun_decl(ast node) {
  ast decl = get_child__(FUN_DECL, DECL, node, 0);
  ast body = get_child_opt_(FUN_DECL, '{', node, 1);
  ast name_probe = get_val_(IDENTIFIER, get_child__(DECL, IDENTIFIER, decl, 0));
  ast fun_type = get_child__(DECL, '(', decl, 1);
  ast params = get_child_opt_('(', LIST, fun_type, 1);
  ast fun_return_type = get_child_('(', fun_type, 0);
  int binding;
  int save_locals_fun = cgc_locals_fun;

  if (is_aggregate_type(fun_return_type)) {
    fatal_error("Returning arrays or structs from function not supported");
  }

  // If the function is main
  if (name_probe == MAIN_ID) {
    // Check if main returns an exit code.
    if (get_op(fun_return_type) != VOID_KW) main_returns = true;
  }

  binding = cgc_lookup_fun(name_probe, cgc_globals);

  if (binding == 0) {
    cgc_add_global_fun(name_probe, alloc_label(STRING_BUF(name_probe)), fun_type);
    binding = cgc_globals;
  }

  def_label(heap[binding+4]);

  cgc_fs = -1; // space for return address
  cgc_locals = 0;
  add_params(params);
  cgc_fs = 0;

  codegen_body(body);

  grow_stack(-cgc_fs);
  cgc_fs = 0;

  ret();

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
    decls = get_child__(DECLS, LIST, node, 0); // Declaration list
    while (decls != 0) { // Multiple variable declarations
      codegen_glo_var_decl(car_(DECL, decls));
      decls = tail(decls); // Next variable declaration
    }
  } else if (op == FUN_DECL) {
    codegen_glo_fun_decl(node);
  } else if (op == TYPEDEF_KW) {
    handle_typedef(node);
  } else if (op == ENUM_KW || op == STRUCT_KW || op == UNION_KW) {
    handle_enum_struct_union_type_decl(node);
  } else {
    putstr("op="); putint(op);
    putstr(" with "); putint(get_nb_children(node)); putstr(" children\n");
    fatal_error("codegen_glo_decl: unexpected declaration");
  }
}

void rt_debug(char* msg) {
  while (*msg != 0) {
    mov_reg_imm(reg_X, *msg);
    os_putchar();
    msg += 1;
  }
  mov_reg_imm(reg_X, '\n');
  os_putchar();
}

void rt_crash(char* msg) {
  rt_debug(msg);
  os_exit();
}

void rt_malloc() {
  int end_lbl = alloc_label("rt_malloc_success");

  mov_reg_mem(reg_Y, reg_glo, word_size); // Bump pointer
  add_reg_reg(reg_X, reg_Y);              // New bump pointer
  mov_reg_mem(reg_Y, reg_glo, 0);         // Heap start
  add_reg_imm(reg_Y, RT_HEAP_SIZE);       // End of heap

  // Make sure the heap is large enough.
  // new bump pointer (reg_x) >= end of heap (reg_y)
  jump_cond_reg_reg(LE, end_lbl, reg_X, reg_Y);
  rt_crash("Heap overflow");

  def_label(end_lbl);
  mov_reg_mem(reg_Y, reg_glo, word_size); // Old bump pointer
  mov_mem_reg(reg_glo, word_size, reg_X); // Adjust the bump pointer
  mov_reg_reg(reg_X, reg_Y);              // Return the old bump pointer
}

void rt_free() {
  // Free are NO-OP for now
  // This function cannot be empty or it will be considered a forward reference
  return;
  fatal_error("rt_free: free is no-op");
}

void codegen_end() {

  int glo_setup_loop_lbl = alloc_label("glo_setup_loop");

  def_label(setup_lbl);

  // Set to 0 the part of the stack that's used for global variables
  mov_reg_imm(reg_X, 0);              // reg_X = 0 constant
  mov_reg_reg(reg_Y, reg_SP);         // reg_Y = end of global variables (excluded)
  grow_stack_bytes(cgc_global_alloc); // Allocate space for global variables
  mov_reg_reg(reg_glo, reg_SP);       // reg_glo = start of global variables

  def_label(glo_setup_loop_lbl);      // Loop over words of global variables table
  mov_mem_reg(reg_glo, 0, reg_X);     // Set to 0
  add_reg_imm(reg_glo, word_size);    // Move to next entry
  jump_cond_reg_reg(LT, glo_setup_loop_lbl, reg_glo, reg_Y);

  mov_reg_reg(reg_glo, reg_SP); // Reset global variables pointer

  os_allocate_memory(RT_HEAP_SIZE);       // Returns the heap start address in reg_X
  mov_mem_reg(reg_glo, 0, reg_X);         // init heap start
  mov_mem_reg(reg_glo, word_size, reg_X); // init bump pointer

  jump(init_start_lbl);

  def_label(init_next_lbl);
  setup_proc_args(cgc_global_alloc);
  call(main_lbl);
  if (!main_returns) mov_reg_imm(reg_X, 0); // exit process with 0 if main returns void
  push_reg(reg_X); // exit process with result of main
  push_reg(reg_X); // dummy return address (exit never needs it)

  // exit function
  def_label(exit_lbl);
  mov_reg_mem(reg_X, reg_SP, word_size);
  os_exit();

  // getchar function
  def_label(getchar_lbl);
  os_getchar();
  ret();

  // putchar function
  def_label(putchar_lbl);
  mov_reg_mem(reg_X, reg_SP, word_size);
  os_putchar();
  ret();

  // fopen function
  def_label(fopen_lbl);
  mov_reg_mem(reg_X, reg_SP, word_size);
  os_fopen();
  ret();

  // fclose function
  def_label(fclose_lbl);
  mov_reg_mem(reg_X, reg_SP, word_size);
  os_fclose();
  ret();

  // fgetc function
  def_label(fgetc_lbl);
  mov_reg_mem(reg_X, reg_SP, word_size);
  os_fgetc();
  ret();

  // malloc function
  def_label(malloc_lbl);
  mov_reg_mem(reg_X, reg_SP, word_size);
  rt_malloc();
  ret();

  // free function
  def_label(free_lbl);
  mov_reg_mem(reg_X, reg_SP, word_size);
  rt_free();
  ret();

  // read function
  def_label(read_lbl);
  mov_reg_mem(reg_X, reg_SP, word_size);
  mov_reg_mem(reg_Y, reg_SP, 2*word_size);
  mov_reg_mem(reg_Z, reg_SP, 3*word_size);
  os_read();
  ret();

  // write function
  def_label(write_lbl);
  mov_reg_mem(reg_X, reg_SP, word_size);
  mov_reg_mem(reg_Y, reg_SP, 2*word_size);
  mov_reg_mem(reg_Z, reg_SP, 3*word_size);
  os_write();
  ret();

  // open function
  def_label(open_lbl);
  mov_reg_mem(reg_X, reg_SP, word_size);
  mov_reg_mem(reg_Y, reg_SP, 2*word_size);
  mov_reg_mem(reg_Z, reg_SP, 3*word_size);
  os_open();
  ret();

  // close function
  def_label(close_lbl);
  mov_reg_mem(reg_X, reg_SP, word_size);
  os_close();
  ret();

  // printf function stub
  def_label(printf_lbl);
  rt_crash("printf is not supported yet.");
  ret();

  assert_all_labels_defined();

  generate_exe();
}
