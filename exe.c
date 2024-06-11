// common part of machine code generators
const int word_size;

void generate_exe();

// 1MB heap
#define RT_HEAP_SIZE 1048576

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

const int char_width = 1;

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

int is_power_of_2(int n) {
  return n != 0 AND (n & (n - 1)) == 0;
}

void mul_for_pointer_arith(int reg, int type_width) {
  int i = 0;
  int other_reg = reg == reg_X ? reg_Y : reg_X;

  if (is_power_of_2(type_width)) {
    while (type_width > 1) {
      i += 1;
      type_width /= 2;
      add_reg_reg(reg, reg);
    }
  } else {
    push_reg(other_reg);
    mov_reg_imm(other_reg, type_width);
    mul_reg_reg(reg, other_reg);
    pop_reg(other_reg);
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

void setup_proc_args();

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

int cgc_fs = 0;
// Function bindings that follows lexical scoping rules
int cgc_locals = 0;
// Like cgc_locals, but with 1 scope for the entire function. Used for goto labels
int cgc_locals_fun = 0;
// Global bindings
int cgc_globals = 0;
// Bump allocator used to allocate static objects
int cgc_global_alloc = 0;

void grow_fs(int words) {
  cgc_fs += words;
}

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

int alloc_label() {
  int lbl = alloc_obj(2);
  heap[lbl] = GENERIC_LABEL;
  heap[lbl + 1] = 0; // Address of label
  return lbl;
}

int alloc_goto_label() {
  int lbl = alloc_obj(3);
  heap[lbl] = GOTO_LABEL;
  heap[lbl + 1] = 0; // Address of label
  heap[lbl + 2] = 0; // cgc-fs of label
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

enum BINDING {
  // Because function params, local and global variables all share the same
  // namespace, BINDING_PARAM_LOCAL, BINDING_VAR_LOCAL and BINDING_VAR_GLOBAL
  // must be kept together at the beginning.
  BINDING_PARAM_LOCAL,
  BINDING_VAR_LOCAL,
  BINDING_VAR_GLOBAL,
  BINDING_ENUM_CST,
  BINDING_LOOP,
  BINDING_SWITCH,
  BINDING_FUN,
  BINDING_GOTO_LABEL,
  BINDING_TYPE_STRUCT,
  BINDING_TYPE_UNION,
  BINDING_TYPE_ENUM,
};

void cgc_add_local_param(int ident, int size, ast type) {
  int binding = alloc_obj(6);
  heap[binding+0] = cgc_locals;
  heap[binding+1] = BINDING_PARAM_LOCAL;
  heap[binding+2] = ident;
  heap[binding+3] = size;
  heap[binding+4] = cgc_fs;
  heap[binding+5] = type;
  cgc_fs -= size;
  cgc_locals = binding;
}

void cgc_add_local(int ident, int size, ast type) {
  int binding = alloc_obj(6);
  cgc_fs += size;
  heap[binding+0] = cgc_locals;
  heap[binding+1] = BINDING_VAR_LOCAL;
  heap[binding+2] = ident;
  heap[binding+3] = size;
  heap[binding+4] = cgc_fs;
  heap[binding+5] = type;
  cgc_locals = binding;
}

void cgc_add_enclosing_loop(int loop_fs, int break_lbl, ast continue_lbl) {
  int binding = alloc_obj(5);
  heap[binding+0] = cgc_locals;
  heap[binding+1] = BINDING_LOOP;
  heap[binding+2] = loop_fs;
  heap[binding+3] = break_lbl;
  heap[binding+4] = continue_lbl;
  cgc_locals = binding;
}

void cgc_add_enclosing_switch(int loop_fs, int break_lbl) {
  int binding = alloc_obj(5);
  heap[binding+0] = cgc_locals;
  heap[binding+1] = BINDING_SWITCH;
  heap[binding+2] = loop_fs;
  heap[binding+3] = break_lbl;
  heap[binding+4] = 0;
  cgc_locals = binding;
}

void cgc_add_global(int ident, int size, int width, ast type) {
  int binding = alloc_obj(6);
  heap[binding+0] = cgc_globals;
  heap[binding+1] = BINDING_VAR_GLOBAL;
  heap[binding+2] = ident;
  heap[binding+3] = size;
  heap[binding+4] = cgc_global_alloc;
  heap[binding+5] = type;
  cgc_global_alloc += width;
  cgc_globals = binding;
}

void cgc_add_global_fun(int ident, int label, ast type) {
  int binding = alloc_obj(6);
  heap[binding+0] = cgc_globals;
  heap[binding+1] = BINDING_FUN;
  heap[binding+2] = ident;
  heap[binding+3] = 0;
  heap[binding+4] = label;
  heap[binding+5] = type;
  cgc_globals = binding;
}

void cgc_add_enum(int ident, int value) {
  int binding = alloc_obj(4);
  heap[binding+0] = cgc_globals;
  heap[binding+1] = BINDING_ENUM_CST;
  heap[binding+2] = ident;
  heap[binding+3] = value;
  cgc_globals = binding;
}

void cgc_add_goto_label(int ident, int lbl) {
  int binding = alloc_obj(5);
  heap[binding+0] = cgc_locals_fun;
  heap[binding+1] = BINDING_GOTO_LABEL;
  heap[binding+2] = ident;
  heap[binding+3] = lbl;
  cgc_locals_fun = binding;
}

void cgc_add_typedef(int ident, enum BINDING struct_or_union_or_enum, ast type) {
  int binding = alloc_obj(4);
  heap[binding+0] = cgc_globals;
  heap[binding+1] = struct_or_union_or_enum;
  heap[binding+2] = ident;
  heap[binding+3] = type;
  cgc_globals = binding;
}

int cgc_lookup_binding_ident(int binding_type, int ident, int env) {
  int binding = env;
  while (binding != 0) {
    if (heap[binding+1] == binding_type && heap[binding+2] == ident) {
      break;
    }
    binding = heap[binding];
  }
  return binding;
}

int cgc_lookup_last_binding(int binding_type, int env) {
  int binding = env;
  while (binding != 0) {
    if (heap[binding+1] == binding_type) {
      break;
    }
    binding = heap[binding];
  }
  return binding;
}

int cgc_lookup_var(int ident, int env) {
  int binding = env;
  while (binding != 0) {
    if (heap[binding+1] <= BINDING_VAR_GLOBAL && heap[binding+2] == ident) {
      break;
    }
    binding = heap[binding];
  }
  return binding;
}

int cgc_lookup_fun(int ident, int env) {
  return cgc_lookup_binding_ident(BINDING_FUN, ident, env);
}

int cgc_lookup_enclosing_loop(int env) {
  return cgc_lookup_last_binding(BINDING_LOOP, env);
}

int cgc_lookup_enclosing_loop_or_switch(int env) {
  int binding = env;
  while (binding != 0) {
    if (heap[binding+1] == BINDING_LOOP OR heap[binding+1] == BINDING_SWITCH) {
      break;
    }
    binding = heap[binding];
  }
  return binding;
}

int cgc_lookup_goto_label(int ident, int env) {
  return cgc_lookup_binding_ident(BINDING_GOTO_LABEL, ident, env);
}

int cgc_lookup_struct(int ident, int env) {
  return cgc_lookup_binding_ident(BINDING_TYPE_STRUCT, ident, env);
}

int cgc_lookup_union(int ident, int env) {
  return cgc_lookup_binding_ident(BINDING_TYPE_UNION, ident, env);
}

int cgc_lookup_enum(int ident, int env) {
  return cgc_lookup_binding_ident(BINDING_TYPE_ENUM, ident, env);
}

int cgc_lookup_enum_value(int ident, int env) {
  return cgc_lookup_binding_ident(BINDING_ENUM_CST, ident, env);
}

// A pointer type is either an array type or a type with at least one star
bool is_pointer_type(ast type) {
  return (get_op(type) == '[') | (get_val(type) != 0);
}

// An aggregate type is either an array type or a struct/union type (that's not a reference)
bool is_aggregate_type(ast type) {
  return get_op(type) == '['
    || ((get_op(type) == STRUCT_KW || get_op(type) == UNION_KW) && get_val(type) == 0);
}

bool is_type(ast type) {
  switch (get_op(type)) {
    case INT_KW:
    case CHAR_KW:
    case VOID_KW:
    case STRUCT_KW:
    case UNION_KW:
    case ENUM_KW:
    case '[':
      return true;
    default:
      return false;
  }
}

bool is_not_pointer_type(ast type) {
  return !is_pointer_type(type);
}

int struct_size(ast struct_type);
int type_width_ast(ast type, bool array_value, bool word_align);

// Size an object of the given type would occupy in memory (in bytes).
// If array_value is true, the size of the array is returned, otherwise the
// size of the pointer is returned.
// If word_align is true, the size is rounded up to the word size.
int type_width(ast type, int stars, bool array_value, bool word_align) {
  // All types have the same shape (kw, stars, ...) except for arrays so we
  // handle array types separately.
  if (get_op(type) == '[') {
    // In certain contexts, we want to know the static size of the array (i.e.
    // sizeof, in struct definitions, etc.) while in other contexts we care
    // about the pointer (i.e. when passing an array to a function, etc.)
    if (array_value) {
      return round_up_to_word_size(get_val(get_child(type, 0)) * type_width_ast(get_child(type, 1), true, false));
    } else {
      return word_size; // Array is a pointer to the first element
    }
  } else if (stars) {
    return word_size; // Pointer
  }

  // Basic type kw
  switch (get_op(type)) {
    case CHAR_KW:
      if (word_align)
        return word_size;
      else
        return char_width;
    case STRUCT_KW:
      return struct_size(type);
    case VOID_KW:
      fatal_error("type_width_ast: void type");
      return 0;
    default:
      return word_size;
  }
}

int type_width_ast(ast type, bool array_value, bool word_align) {
  return type_width(type, get_val(type), array_value, word_align);
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

  if (get_op(type) == STRUCT_KW AND get_child(type, 2) == 0) { // struct with empty def => reference
    binding = cgc_lookup_struct(get_val(get_child(type, 1)), cgc_globals);
    if (binding == 0) fatal_error("canonicalize_type: struct type not defined");
    res = heap[binding+3];
    if (get_val(type) != 0) { // Copy stars
      res = clone_ast(res);
      set_child(res, 0, get_child(type, 0));
    }
  } else if (get_op(type) == UNION_KW AND get_child(type, 2) == 0) { // union with empty def => reference
    binding = cgc_lookup_union(get_val(get_child(type, 1)), cgc_globals);
    if (binding == 0) fatal_error("canonicalize_type: union type not defined");
    res = heap[binding+3];
    if (get_val(type) != 0) { // Copy stars
      res = clone_ast(res);
      set_child(res, 0, get_child(type, 0));
    }
  } else if (get_op(type) == ENUM_KW AND get_child(type, 1) == 0) { // enum with empty def => reference
    binding = cgc_lookup_enum(get_val(get_child(type, 0)), cgc_globals);
    if (binding == 0) fatal_error("canonicalize_type: enum type not defined");
    res = heap[binding+3];
    if (get_val(type) != 0) { // Copy stars
      res = clone_ast(res);
      set_child(res, 0, get_child(type, 0));
    }
  }

  return res;
}

// Size of a struct type, rounded up to the word size
int struct_size(ast struct_type) {
  ast members;
  ast member_type;
  int size = 0;

  if (get_op(struct_type) != STRUCT_KW) fatal_error("struct_size: not a struct type");

  members = get_child(canonicalize_type(struct_type), 2);

  while (get_op(members) == ',') {
    member_type = get_child(members, 1);
    members = get_child(members, 2);
    size += type_width_ast(member_type, true, true);
  }

  return round_up_to_word_size(size);
}

// Return offset of struct member
int struct_member_offset(ast struct_type, ast member_ident) {
  ast members = get_child(canonicalize_type(struct_type), 2);
  ast member_type;
  int size = 0;

  while (get_op(members) == ',') {
    if (get_val(member_ident) == get_val(get_child(members, 0))) {
      return size;
    }

    member_type = get_child(members, 1);
    members = get_child(members, 2);
    size += round_up_to_word_size(type_width_ast(member_type, true, true));
  }

  fatal_error("struct_member_offset: member not found");
  return 0;
}

// Find a struct member
ast struct_member(ast struct_type, ast member_ident) {
  ast members = get_child(canonicalize_type(struct_type), 2);

  while (get_op(members) == ',') {
    if (get_val(member_ident) == get_val(get_child(members, 0)))
      return members;

    members = get_child(members, 2);
  }

  fatal_error("struct_member: member not found");
  return 0;
}

// Width of an object pointed to by a reference type.
int ref_type_width(ast type) {
  if (get_op(type) == '[') {
    return type_width_ast(get_child(type, 1), false, false); // size of inner type
  } else if (get_val(type) == 1) { // pointer *
    return type_width(type, 0, false, false); // size of inner type
  } else {
    return word_size;
  }
}

ast int_type;
ast char_type;
ast string_type;
ast void_type;
ast void_star_type;

// Compute the type of an expression
ast value_type(ast node) {
  int op = get_op(node);
  int nb_children = get_nb_children(node);
  int binding;
  int ident;

  ast left_type;
  ast right_type;

  if (nb_children == 0) {
    if (op == INTEGER) {
      return int_type;
    } else if (op == CHARACTER) {
      return char_type;
    } else if (op == STRING) {
      return string_type;
    } else if (op == IDENTIFIER) {
      ident = get_val(node);
      binding = cgc_lookup_var(ident, cgc_locals);
      if (binding != 0) {
        return heap[binding+5];
      } else {
        binding = cgc_lookup_var(ident, cgc_globals);
        if (binding != 0) {
          return heap[binding+5];
        } else {
          binding = cgc_lookup_enum_value(ident, cgc_globals);
          if (binding != 0) {
            return int_type; // Enums are always integers
          } else {
            putstr("ident = ");
            putstr(string_pool+get_val(ident));
            putchar('\n');
            fatal_error("value_type: identifier not found");
          }
        }
      }
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("value_type: unknown expression with nb_children == 0");
    }

  } else if (nb_children == 1) {

    if (op == '*') {
      left_type = value_type(get_child(node, 0));
      if (get_op(left_type) == '[') { // Array type
        return get_child(left_type, 1);
      } else if (get_val(left_type) != 0) { // Pointer type
        return new_ast0(get_op(left_type), get_val(left_type) - 1); // one less indirection
      } else {
        putstr("left_type="); putint(left_type); putchar('\n');
        fatal_error("pointer_width: non pointer is being dereferenced with *");
      }
    } else if (op == '&') {
      left_type = value_type(get_child(node, 0));
      if (get_op(left_type) == '[') {
        left_type = clone_ast(get_child(left_type, 1)); // Inner type
        set_val(left_type, get_val(left_type) + 1); // Increment star by 2, to account for the [ we just removed
      } else {
        left_type = clone_ast(left_type);
        set_val(left_type, get_val(left_type) + 1); // Increment star by 1
      }
      return left_type;
    } else if (op == '+' OR op == '-' OR op == '~' OR op == '!' OR op == MINUS_MINUS OR op == PLUS_PLUS) {
      // Unary operation don't change the type
      return value_type(get_child(node, 0));
    } else if (op == SIZEOF_KW) {
      return int_type; // sizeof always returns an integer
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("value_type: unexpected operator");
    }

  } else if (nb_children == 2) {

    if (op == '+' OR op == '-' OR op == '*' OR op == '/' OR op == '%' OR op == '&' OR op == '|' OR op == '^'
     OR op == LSHIFT OR op == RSHIFT OR op == '<' OR op == '>' OR op == EQ_EQ OR op == EXCL_EQ OR op == LT_EQ OR op == GT_EQ) {
      left_type = value_type(get_child(node, 0));
      right_type = value_type(get_child(node, 1));
      if (is_pointer_type(left_type)) {
        // if left is an array or a pointer, the type is also a pointer
        return left_type;
      } else {
        // if left is not a pointer, the type is the type of the right operand
        return right_type;
      }
    } else if (op == '[') {
      left_type = value_type(get_child(node, 0));
      right_type = value_type(get_child(node, 0));

      if (get_op(left_type) == '[') { // Array
        return get_child(left_type, 1); // array inner type
      } else if (get_val(left_type) != 0) { // Pointer
        return new_ast0(get_op(left_type), get_val(left_type) - 1); // one less indirection
      } else if (get_op(right_type) == '[') { // Array, but with the operands flipped (i.e. 0[arr] instead of arr[0])
        return get_child(right_type, 1); // array inner type
      } else if (get_val(right_type) != 0) {
        return new_ast0(get_op(right_type), get_val(right_type) - 1); // one less indirection
      } else {
        putstr("left_type="); putint(left_type); putchar('\n');
        fatal_error("value_type: non pointer is being dereferenced with *");
      }
    } else if (op == '=' OR op == AMP_EQ OR op == BAR_EQ OR op == CARET_EQ OR op == LSHIFT_EQ OR op == MINUS_EQ OR op == PERCENT_EQ OR op == PLUS_EQ OR op == RSHIFT_EQ OR op == SLASH_EQ OR op == STAR_EQ) {
      return value_type(get_child(node, 0)); // Only the left side is relevant here
    } else if (op == AMP_AMP OR op == BAR_BAR) {
      // TODO: Check that the operands have compatible types?
      return value_type(get_child(node, 0));
    } else if (op == '(') {
      binding = cgc_lookup_fun(get_val(get_child(node, 0)), cgc_globals);
      if (binding != 0) {
        return heap[binding+5];
      } else {
        putstr("ident = ");
        putstr(string_pool + get_val(get_val(get_child(node, 0))));
        putchar('\n');
        fatal_error("value_type: function not found");
      }
    } else if (op == '.') {
      left_type = value_type(get_child(node, 0));
      if (get_op(left_type) == STRUCT_KW AND get_val(left_type) == 0) {
        get_child(struct_member(left_type, get_child(node, 1)), 1); // child 1 of member is the type
      } else {
        fatal_error("value_type: . operator on non-struct pointer type");
      }
    } else if (op == ARROW) {
      // Same as '.', but left_type must be a pointer
      left_type = value_type(get_child(node, 0));
      if (get_op(left_type) == STRUCT_KW AND get_val(left_type) == 1) {
        get_child(struct_member(left_type, get_child(node, 1)), 1); // child 1 of member is the type
      } else {
        fatal_error("value_type: -> operator on non-struct pointer type");
      }
    } else {
      fatal_error("value_type: unknown expression");
    }

  } else if (nb_children == 3) {

    if (op == '?') {
      // We assume that the 2 cases have the same type.
      return value_type(get_child(node, 1));
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("value_type: unknown expression with 3 children");
    }

  } else {
    putstr("op="); putint(op); putchar('\n');
    fatal_error("value_type: unknown expression with >4 children");
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

    lbl1 = alloc_label();
    lbl2 = alloc_label();
    jump_cond_reg_reg(cond, lbl1, reg_X, reg_Y);
    xor_reg_reg(reg_X, reg_X);
    jump(lbl2);
    def_label(lbl1);
    mov_reg_imm(reg_X, 1);
    def_label(lbl2);

  } else {
    if      (op == '+' OR op == PLUS_EQ OR op == PLUS_PLUS_PRE OR  op == PLUS_PLUS_POST) {
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
    else if (op == '-' OR op == MINUS_EQ OR op == MINUS_MINUS_PRE OR op == MINUS_MINUS_POST) {
      // Pointer subtraction is only valid if one of the operands is a pointer
      // When both operands are pointers, the result is the difference between the two pointers divided by the width of the target object.
      // When one operand is a pointer and the other is an integer, the result is the pointer minus the integer times the width of the target object.

      if (is_pointer_type(left_type) && is_pointer_type(right_type)) {
        fatal_error("codegen_binop: subtraction between pointers not implemented");
      } else if (is_pointer_type(left_type)) {
        mul_for_pointer_arith(reg_Y, ref_type_width(left_type));
      } else if (is_pointer_type(right_type)) {
        mul_for_pointer_arith(reg_X, ref_type_width(right_type));
      }

      sub_reg_reg(reg_X, reg_Y);
    }
    else if (op == '*' OR op == STAR_EQ) mul_reg_reg(reg_X, reg_Y);
    else if (op == '/' OR op == SLASH_EQ) div_reg_reg(reg_X, reg_Y);
    else if (op == '%' OR op == PERCENT_EQ) rem_reg_reg(reg_X, reg_Y);
    else if (op == '&' OR op == AMP_EQ) and_reg_reg(reg_X, reg_Y);
    else if (op == '|' OR op == BAR_EQ) or_reg_reg(reg_X, reg_Y);
    else if (op == '^' OR op == CARET_EQ) xor_reg_reg(reg_X, reg_Y);
    else if (op == LSHIFT OR op == LSHIFT_EQ) shl_reg_reg(reg_X, reg_Y);
    else if (op == RSHIFT OR op == RSHIFT_EQ) sar_reg_reg(reg_X, reg_Y);
    else if (op == '[') {
      // Same as pointer addition for address calculation
      if (is_pointer_type(left_type) AND is_not_pointer_type(right_type)) {
        mul_for_pointer_arith(reg_Y, ref_type_width(left_type));
        width = ref_type_width(left_type);
      } else if (is_pointer_type(right_type) AND is_not_pointer_type(left_type)) {
        mul_for_pointer_arith(reg_X, ref_type_width(right_type));
        width = ref_type_width(right_type);
      } else {
        fatal_error("codegen_binop: invalid array access operands");
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

  if (get_op(type) == '[') {
    left_width = codegen_lvalue(param);
    pop_reg(reg_X);
    grow_fs(-1);
    grow_stack_bytes(round_up_to_word_size(left_width));
    grow_fs(round_up_to_word_size(left_width) / word_size);
    copy_obj(reg_SP, 0, reg_X, 0, left_width);
  } else if (get_op(type) == STRUCT_KW AND get_val(type) == 0) {
    left_width = codegen_lvalue(param);
    pop_reg(reg_X);
    grow_fs(-1);
    grow_stack_bytes(round_up_to_word_size(left_width));
    grow_fs(round_up_to_word_size(left_width) / word_size);
    copy_obj(reg_SP, 0, reg_X, 0, left_width);
  } else {
    codegen_rvalue(param);
  }

  return type_width_ast(type, false, true) / word_size;
}

int codegen_params(ast params) {

  int fs = 0;

  if (params != 0) {
    if (get_op(params) == ',') {
      fs = codegen_params(get_child(params, 1));
      fs += codegen_param(get_child(params, 0));
    } else {
      fs = codegen_param(params);
    }
  }

  return fs;
}

void codegen_call(ast node) {

  ast fun = get_child(node, 0);
  ast name = get_val(fun);
  ast params = get_child(node, 1);
  ast nb_params = codegen_params(params);

  int binding = cgc_lookup_fun(name, cgc_globals);
  int lbl;

  if (binding == 0) {
    lbl = alloc_label();
    cgc_add_global_fun(name, lbl, 0);
    binding = cgc_globals;
  }

  call(heap[binding+4]);

  grow_stack(-nb_params);
  grow_fs(-nb_params);

  push_reg(reg_X);
}

void codegen_goto(ast node) {

  ast label_ident = get_val(node);

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

  if (nb_children == 0) {
    if (op == IDENTIFIER) {
      binding = cgc_lookup_var(get_val(node), cgc_locals);
      if (binding != 0) {
        mov_reg_imm(reg_X, (cgc_fs - heap[binding+4]) * word_size);
        add_reg_reg(reg_X, reg_SP);
        push_reg(reg_X);
      } else {
        binding = cgc_lookup_var(get_val(node), cgc_globals);
        if (binding != 0) {
          mov_reg_imm(reg_X, heap[binding+4]);
          add_reg_reg(reg_X, reg_glo);
          push_reg(reg_X);
        } else {
          fatal_error("codegen_lvalue: identifier not found");
        }
      }
      lvalue_width = type_width_ast(heap[binding+5], true, true);
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("codegen_lvalue: unknown lvalue with nb_children == 0");
    }

  } else if (nb_children == 1) {

    if (op == '*') {
      codegen_rvalue(get_child(node, 0));
      grow_fs(-1);
      lvalue_width = ref_type_width(value_type(get_child(node, 0)));
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("codegen_lvalue: unexpected operator");
    }

  } else if (nb_children == 2) {

    if (op == '[') {
      type = value_type(get_child(node, 0));
      codegen_rvalue(get_child(node, 0));
      codegen_rvalue(get_child(node, 1));
      codegen_binop('+', get_child(node, 0), get_child(node, 1));
      grow_fs(-2);
      lvalue_width = ref_type_width(type);
    } else if (op == '.') {
      type = value_type(get_child(node, 0));
      if (get_op(type) == STRUCT_KW && get_val(type) == 0) {
        codegen_lvalue(get_child(node, 0));
        pop_reg(reg_X);
        add_reg_imm(reg_X, struct_member_offset(type, get_child(node, 1)));
        push_reg(reg_X);
        grow_fs(-1);
        lvalue_width = type_width_ast(get_child(struct_member(type, get_child(node, 1)), 1), true, true); // child 1 of member is the type
      } else {
        fatal_error("codegen_lvalue: -> operator on non-struct type");
      }
    } else if (op == ARROW) {
      // Same as '.', but type must be a pointer
      type = value_type(get_child(node, 0));
      if (get_op(type) == STRUCT_KW && get_val(type) == 1) {
        codegen_rvalue(get_child(node, 0));
        pop_reg(reg_X);
        add_reg_imm(reg_X, struct_member_offset(type, get_child(node, 1)));
        push_reg(reg_X);
        grow_fs(-1);
        lvalue_width = type_width_ast(get_child(struct_member(type, get_child(node, 1)), 1), true, true); // child 1 of member is the type
      } else {
        fatal_error("codegen_lvalue: -> operator on non-struct pointer type");
      }
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

void codegen_string(int start) {

  int lbl = alloc_label();
  int i = start;

  call(lbl);

  while (string_pool[i] != 0) {
    if (char_width == 1) {
      emit_i8(string_pool[i]);
    } else {
      emit_word_le(string_pool[i]);
    }
    i += 1;
  }


  if (char_width == 1) {
    emit_i8(0);
  } else {
    emit_word_le(0);
  }

  def_label(lbl);
}

void codegen_rvalue(ast node) {
  int op = get_op(node);
  int nb_children = get_nb_children(node);
  int binding;
  int ident;
  int lbl1;
  int lbl2;
  int left_width;
  ast type1;
  ast type2;

  if (nb_children == 0) {
    if (op == INTEGER) {
      mov_reg_imm(reg_X, -get_val(node));
      push_reg(reg_X);
    } else if (op == CHARACTER) {
      mov_reg_imm(reg_X, get_val(node));
      push_reg(reg_X);
    } else if (op == IDENTIFIER) {
      ident = get_val(node);
      binding = cgc_lookup_var(ident, cgc_locals);
      if (binding != 0) {
        mov_reg_imm(reg_X, (cgc_fs - heap[binding+4]) * word_size);
        add_reg_reg(reg_X, reg_SP);
        // local arrays are allocated on the stack, so no need to dereference
        // same thing for non-pointer structs and unions.
        if (get_op(heap[binding+5]) != '['
          && !(get_op(heap[binding+5]) == STRUCT_KW && get_val(heap[binding+5]) == 0)
          && !(get_op(heap[binding+5]) == UNION_KW && get_val(heap[binding+5]) == 0)) {
          mov_reg_mem(reg_X, reg_X, 0);
        }
        push_reg(reg_X);
      } else {
        binding = cgc_lookup_var(ident, cgc_globals);
        if (binding != 0) {
          mov_reg_imm(reg_X, heap[binding+4]);
          add_reg_reg(reg_X, reg_glo);
          // global arrays are allocated on the stack, so no need to dereference
          // same thing for non-pointer structs and unions.
          if (get_op(heap[binding+5]) != '['
            && !(get_op(heap[binding+5]) == STRUCT_KW && get_val(heap[binding+5]) == 0)
            && !(get_op(heap[binding+5]) == UNION_KW && get_val(heap[binding+5]) == 0)) {
            mov_reg_mem(reg_X, reg_X, 0);
          }
          push_reg(reg_X);
        } else {
          binding = cgc_lookup_enum_value(ident, cgc_globals);
          if (binding != 0) {
            mov_reg_imm(reg_X, -get_val(heap[binding+3]));
            push_reg(reg_X);
          } else {
            putstr("ident = "); putstr(string_pool+get_val(ident)); putchar('\n');
            fatal_error("codegen_rvalue: identifier not found");
          }
        }
      }
    } else if (op == STRING) {
      codegen_string(get_val(node));
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("codegen_rvalue: unknown rvalue with nb_children == 0");
    }

  } else if (nb_children == 1) {
    if (op == '*') {
      codegen_rvalue(get_child(node, 0));
      pop_reg(reg_Y);
      grow_fs(-1);
      if (is_pointer_type(value_type(get_child(node, 0)))) {
        load_mem_location(reg_X, reg_Y, 0, ref_type_width(value_type(get_child(node, 0))));
      } else {
        fatal_error("codegen_rvalue: non-pointer is being dereferenced with *");
      }
      push_reg(reg_X);
    } else if (op == '+') {
      codegen_rvalue(get_child(node, 0));
      pop_reg(reg_X);
      grow_fs(-1);
      push_reg(reg_X);
    } else if (op == '-') {
      codegen_rvalue(get_child(node, 0));
      pop_reg(reg_Y);
      grow_fs(-1);
      xor_reg_reg(reg_X, reg_X);
      sub_reg_reg(reg_X, reg_Y);
      push_reg(reg_X);
    } else if (op == '~') {
      codegen_rvalue(get_child(node, 0));
      pop_reg(reg_Y);
      grow_fs(-1);
      mov_reg_imm(reg_X, -1);
      xor_reg_reg(reg_X, reg_Y);
      push_reg(reg_X);
    } else if (op == '!') {
      xor_reg_reg(reg_X, reg_X);
      push_reg(reg_X);
      grow_fs(1);
      codegen_rvalue(get_child(node, 0));
      codegen_binop(EQ_EQ, new_ast0(INTEGER, 0), get_child(node, 0));
      grow_fs(-2);
    } else if ((op == MINUS_MINUS_POST) OR (op == PLUS_PLUS_POST)){
      codegen_lvalue(get_child(node, 0));
      pop_reg(reg_Y);
      mov_reg_mem(reg_X, reg_Y, 0);
      push_reg(reg_X); // saves the original value of lvalue
      push_reg(reg_Y);
      push_reg(reg_X); // saves the value of lvalue to be modified
      mov_reg_imm(reg_X, 1); // Equivalent to calling codegen rvalue with INTEGER 1 (subtraction or addition handled in codegen_binop)
      push_reg(reg_X);
      codegen_binop(op, get_child(node, 0), new_ast0(INTEGER, 0)); // Pops two values off the stack and pushes the result
      pop_reg(reg_X); // result
      pop_reg(reg_Y); // address
      grow_fs(-1);
      mov_mem_reg(reg_Y, 0, reg_X); // Store the result in the address
    } else if ((op == MINUS_MINUS_PRE) OR (op == PLUS_PLUS_PRE)) {
      codegen_lvalue(get_child(node, 0));
      pop_reg(reg_Y);
      push_reg(reg_Y);
      mov_reg_mem(reg_X, reg_Y, 0);
      push_reg(reg_X);
      grow_fs(1);
      mov_reg_imm(reg_X, 1); // equivalent to calling codegen rvalue with INTEGER 1 (subtraction or addition handled in codegen_binop)
      push_reg(reg_X);
      grow_fs(1);
      codegen_binop(op, get_child(node, 0), new_ast0(INTEGER, 0)); // Pops two values off the stack and pushes the result
      pop_reg(reg_X); // result
      pop_reg(reg_Y); // address
      grow_fs(-3);
      mov_mem_reg(reg_Y, 0, reg_X); //store the result in the address
      push_reg(reg_X);
    } else if (op == '&') {
      codegen_lvalue(get_child(node, 0));
      grow_fs(-1);
    } else if (op == SIZEOF_KW) {
      if (is_type(get_child(node, 0))) {
        mov_reg_imm(reg_X, type_width_ast(get_child(node, 0), true, false));
      } else {
        mov_reg_imm(reg_X, type_width_ast(value_type(get_child(node, 0)), true, false));
      }
      push_reg(reg_X);
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("codegen_rvalue: unexpected operator");
    }

  } else if (nb_children == 2) {
    if (op == '+' OR op == '-' OR op == '*' OR op == '/' OR op == '%' OR op == '&' OR op == '|' OR op == '^' OR op == LSHIFT OR op == RSHIFT OR op == '<' OR op == '>' OR op == EQ_EQ OR op == EXCL_EQ OR op == LT_EQ OR op == GT_EQ OR op == '[') {
      codegen_rvalue(get_child(node, 0));
      codegen_rvalue(get_child(node, 1));
      codegen_binop(op, get_child(node, 0), get_child(node, 1));
      grow_fs(-2);
    } else if (op == '=') {
      type1 = value_type(get_child(node, 0));
      left_width = codegen_lvalue(get_child(node, 0));
      if (get_op(type1) == STRUCT_KW && get_val(type1) == 0) {
        // Struct assignment, we copy the struct.
        codegen_lvalue(get_child(node, 1));
        pop_reg(reg_X);
        pop_reg(reg_Y);
        grow_fs(-2);
        copy_obj(reg_Y, 0, reg_X, 0, left_width);
      } else {
        codegen_rvalue(get_child(node, 1));
        pop_reg(reg_X);
        pop_reg(reg_Y);
        grow_fs(-2);
        write_mem_location(reg_Y, 0, reg_X, left_width);
      }
      push_reg(reg_X);
    } else if (op == AMP_EQ OR op == BAR_EQ OR op == CARET_EQ OR op == LSHIFT_EQ OR op == MINUS_EQ OR op == PERCENT_EQ OR op == PLUS_EQ OR op == RSHIFT_EQ OR op == SLASH_EQ OR op == STAR_EQ) {
      left_width = codegen_lvalue(get_child(node, 0));
      pop_reg(reg_Y);
      push_reg(reg_Y);
      load_mem_location(reg_X, reg_Y, 0, left_width);
      push_reg(reg_X);
      grow_fs(1);
      codegen_rvalue(get_child(node, 1));
      codegen_binop(op, get_child(node, 0), get_child(node, 1));
      pop_reg(reg_X);
      pop_reg(reg_Y);
      grow_fs(-3);
      write_mem_location(reg_Y, 0, reg_X, left_width);
      push_reg(reg_X);
    } else if (op == AMP_AMP OR op == BAR_BAR) {
      lbl1 = alloc_label();
      codegen_rvalue(get_child(node, 0));
      pop_reg(reg_X);
      push_reg(reg_X);
      xor_reg_reg(reg_Y, reg_Y);
      if (op == AMP_AMP) {
        jump_cond_reg_reg(EQ, lbl1, reg_X, reg_Y);
      } else {
        jump_cond_reg_reg(NE, lbl1, reg_X, reg_Y);
      }
      pop_reg(reg_X); grow_fs(-1);
      codegen_rvalue(get_child(node, 1));
      grow_fs(-1);
      def_label(lbl1);
    } else if (op == '(') {
      codegen_call(node);
    } else if (op == '.') {
      type1 = value_type(get_child(node, 0));
      if (get_op(type1) == STRUCT_KW && get_val(type1) == 0) {
        type2 = get_child(struct_member(type1, get_child(node, 1)), 1);
        codegen_lvalue(get_child(node, 0));
        pop_reg(reg_Y);
        add_reg_imm(reg_Y, struct_member_offset(type1, get_child(node, 1)));
        grow_fs(-1);
        if (!is_aggregate_type(type2)) {
          load_mem_location(reg_X, reg_Y, 0, word_size);
          push_reg(reg_X);
        }
      } else {
        fatal_error("codegen_rvalue: -> operator on non-struct type");
      }
    } else if (op == ARROW) {
      type1 = value_type(get_child(node, 0));
      if (get_op(type1) == STRUCT_KW && get_val(type1) == 1) {
        type2 = get_child(struct_member(type1, get_child(node, 1)), 1);
        codegen_rvalue(get_child(node, 0));
        pop_reg(reg_Y);
        grow_fs(-1);
        if (!is_aggregate_type(type2)) {
          load_mem_location(reg_X, reg_Y, struct_member_offset(type1, get_child(node, 1)), word_size);
        }
        push_reg(reg_X);
      } else {
        fatal_error("codegen_rvalue: -> operator on non-struct pointer type");
      }
    } else {
      fatal_error("codegen_rvalue: unknown rvalue with 2 children");
    }

  } else if (nb_children == 3) {

    if (op == '?') {
      lbl1 = alloc_label(); // false label
      lbl2 = alloc_label(); // end label
      codegen_rvalue(get_child(node, 0));
      pop_reg(reg_X);
      grow_fs(-1);
      xor_reg_reg(reg_Y, reg_Y);
      jump_cond_reg_reg(EQ, lbl1, reg_X, reg_Y);
      codegen_rvalue(get_child(node, 1)); // value when true
      jump(lbl2);
      def_label(lbl1);
      grow_fs(-1); // here, the child#1 is not evaluated, so we adjust the stack
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

  setup_lbl = alloc_label();
  init_start_lbl = alloc_label();
  init_next_lbl = init_start_lbl;

  // Make room for heap start and malloc bump pointer.
  // reg_glo[0]: heap start
  // reg_glo[word_size]: malloc bump pointer
  cgc_global_alloc += 2 * word_size;

  int_type = new_ast0(INT_KW, 0);
  char_type = new_ast0(CHAR_KW, 0);
  string_type = new_ast0(CHAR_KW, 1);
  void_type = new_ast0(VOID_KW, 0);
  void_star_type = new_ast0(VOID_KW, 1);

  main_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "main"), main_lbl, void_type);

  exit_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "exit"), exit_lbl, void_type);

  getchar_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "getchar"), getchar_lbl, char_type);

  putchar_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "putchar"), putchar_lbl, void_type);

  fopen_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "fopen"), fopen_lbl, int_type);

  fclose_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "fclose"), fclose_lbl, void_type);

  fgetc_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "fgetc"), fgetc_lbl, char_type);

  malloc_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "malloc"), malloc_lbl, void_star_type);

  free_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "free"), free_lbl, char_type);

  jump(setup_lbl);
}

void codegen_enum(ast node) {
  ast name = get_child(node, 1);
  ast cases = get_child(node, 2);
  int binding;

  while (get_op(cases) == ',') {
    cgc_add_enum(get_val(get_child(cases, 0)), get_child(cases, 1));
    cases = get_child(cases, 2);
  }

  if (name != 0 && get_child(node, 2) != 0) { // if enum has a name and members (not a reference to an existing type)
    binding = cgc_lookup_enum(get_val(name), cgc_globals);
    if (binding != 0) { fatal_error("codegen_enum: enum already declared"); }
    cgc_add_typedef(get_val(name), BINDING_TYPE_ENUM, node);
  }
}

void codegen_struct(ast node) {
  ast name = get_child(node, 1);
  int binding;

  if (name != 0 && get_child(node, 2) != 0) { // if struct has a name and members (not a reference to an existing type)
    binding = cgc_lookup_struct(get_val(name), cgc_globals);
    if (binding != 0 AND heap[binding + 3] != node) { fatal_error("codegen_struct: struct already declared"); }
    cgc_add_typedef(get_val(name), BINDING_TYPE_STRUCT, node);
  }
}

void handle_enum_struct_union_type_decl(ast type) {
  if (get_op(type) == ENUM_KW) {
    codegen_enum(type);
  } else if (get_op(type) == STRUCT_KW) {
    codegen_struct(type);
  } else if (get_op(type) == UNION_KW) {
    fatal_error("handle_enum_struct_union_type_decl: union not supported");
  }

  // If not an enum, struct, or union, do nothing
}

void codegen_glo_var_decl(ast node) {

  ast name = get_child(node, 0);
  ast type = get_child(node, 1);
  ast init = get_child(node, 2);
  int size;
  int binding = cgc_lookup_var(name, cgc_globals);

  if (get_op(type) == '[') { // Array declaration
    size = get_val(get_child(type, 0));
  } else {
    // All non-array types have size 1
    size = 1;
  }

  handle_enum_struct_union_type_decl(type);

  if (binding == 0) {
    cgc_add_global(name, size, type_width_ast(type, true, false), type);
    binding = cgc_globals;
  }

  if (get_op(type) != '[') { // not array declaration

    def_label(init_next_lbl);
    init_next_lbl = alloc_label();

    if (init != 0) {
      codegen_rvalue(init);
    } else {
      xor_reg_reg(reg_X, reg_X);
      push_reg(reg_X);
      grow_fs(1);
    }

    pop_reg(reg_X);
    grow_fs(-1);

    mov_mem_reg(reg_glo, heap[binding+4], reg_X);

    jump(init_next_lbl);
  }
}

void codegen_body(ast node) {

  ast x;
  int save_fs = cgc_fs;
  int save_locals = cgc_locals;
  ast name;
  ast type;
  ast init;
  int size;

  if (node != 0) {

    while (get_op(node) == '{') {
      x = get_child(node, 0);
      if (get_op(x) == VAR_DECL) {

        name = get_child(x, 0);
        type = get_child(x, 1);
        init = get_child(x, 2);

        if (get_op(type) == '[') { // Array declaration
          size = type_width_ast(type, true, true);  // size in bytes (word aligned)
          grow_stack_bytes(size);
          size /= word_size; // size in words
        } else if (get_op(type) == STRUCT_KW && get_val(type) == 0) {
          size = struct_size(type); // size in bytes (word aligned)
          grow_stack_bytes(size);
          size /= word_size; // size in words
        } else {
          // All non-array types are represented as a word, even if they are smaller
          if (init != 0) {
            codegen_rvalue(init);
            grow_fs(-1);
          } else {
	          xor_reg_reg(reg_X, reg_X);
            push_reg(reg_X);
          }
          size = 1;
        }

        cgc_add_local(name, size, type);

      } else {
        codegen_statement(x);
      }
      node = get_child(node, 1);
    }

    grow_stack(save_fs - cgc_fs);

    cgc_fs = save_fs;
    cgc_locals = save_locals;
  }
}

void codegen_statement(ast node) {

  int op;
  int lbl1;
  int lbl2;
  int lbl3;
  int save_fs;
  int save_locals;
  int binding;
  ast patterns;

  if (node == 0) return;

  op = get_op(node);

  if (op == IF_KW) {

    lbl1 = alloc_label(); // else statement
    lbl2 = alloc_label(); // join point after if
    codegen_rvalue(get_child(node, 0));
    pop_reg(reg_X);
    grow_fs(-1);
    xor_reg_reg(reg_Y, reg_Y);
    jump_cond_reg_reg(EQ, lbl1, reg_X, reg_Y);
    codegen_statement(get_child(node, 1));
    jump(lbl2);
    def_label(lbl1);
    codegen_statement(get_child(node, 2));
    def_label(lbl2);

  } else if (op == WHILE_KW) {

    lbl1 = alloc_label(); // while statement start
    lbl2 = alloc_label(); // join point after while

    save_fs = cgc_fs;
    save_locals = cgc_locals;

    cgc_add_enclosing_loop(cgc_fs, lbl2, lbl1);

    def_label(lbl1);
    codegen_rvalue(get_child(node, 0));
    pop_reg(reg_X);
    grow_fs(-1);
    xor_reg_reg(reg_Y, reg_Y);
    jump_cond_reg_reg(EQ, lbl2, reg_X, reg_Y);
    codegen_statement(get_child(node, 1));
    jump(lbl1);
    def_label(lbl2);

    cgc_fs = save_fs;
    cgc_locals = save_locals;

  } else if (op == FOR_KW) {

    lbl1 = alloc_label(); // while statement start
    lbl2 = alloc_label(); // join point after while
    lbl3 = alloc_label(); // initial loop starting point

    save_fs = cgc_fs;
    save_locals = cgc_locals;

    cgc_add_enclosing_loop(cgc_fs, lbl2, lbl1);

    codegen_statement(get_child(node, 0)); // init
    jump(lbl3); // skip post loop action
    def_label(lbl1);
    codegen_statement(get_child(node, 2)); // post loop action
    def_label(lbl3);
    codegen_rvalue(get_child(node, 1)); // test
    pop_reg(reg_X);
    grow_fs(-1);
    xor_reg_reg(reg_Y, reg_Y);
    jump_cond_reg_reg(EQ, lbl2, reg_X, reg_Y);
    codegen_statement(get_child(node, 3));
    jump(lbl1);
    def_label(lbl2);

    cgc_fs = save_fs;
    cgc_locals = save_locals;

  } else if (op == SWITCH_KW) {

    save_fs = cgc_fs;
    save_locals = cgc_locals;

    lbl1 = alloc_label(); // lbl1: end of switch
    lbl2 = alloc_label(); // lbl2: next case
    // lbl3: conditional block

    cgc_add_enclosing_switch(cgc_fs, lbl1);

    codegen_rvalue(get_child(node, 0)); // switch operand

    jump(lbl2); // Jump to first case

    node = get_child(node, 1); // switch body
    if (node == 0 || get_op(node) != '{') fatal_error("comp_statement: switch without body");

    // We iterate through the body of the switch.
    while (get_op(node) == '{') {

      patterns = get_child(node, 0);

      if (get_op(patterns) == CASE_KW) {

        lbl3 = alloc_label(); // conditional block label
        // sequence of switch cases are chained together in the CASE_KW AST nodes, so we iterate through them
        while (get_op(patterns) == CASE_KW) {
          // If falling through from a previous conditional block, we don't want
          // to test the case and simply want to execute the conditional block
          // so we skip the case test.
          jump(lbl3);
          def_label(lbl2);
          // create label for next case
          lbl2 = alloc_label();
          // duplicate switch operand for the comparison
          pop_reg(reg_X); push_reg(reg_X); push_reg(reg_X); grow_fs(1);
          // Get the value of the case and compare it to the switch operand
          codegen_rvalue(get_child(patterns, 0));
          pop_reg(reg_Y); pop_reg(reg_X); grow_fs(-2);
          jump_cond_reg_reg(EQ, lbl3, reg_X, reg_Y);
          jump(lbl2); // jump to next case if condition is false

          patterns = get_child(patterns, 1); // next case
        }

        def_label(lbl3); // conditional block start here
        codegen_statement(patterns); // patterns now points to first statement of the block

      } else if (get_op(patterns) == DEFAULT_KW) {
        lbl3 = alloc_label(); // conditional block label
        def_label(lbl2);
        lbl2 = alloc_label(); // next case
        def_label(lbl3); // conditional block start
        codegen_statement(get_child(patterns, 0)); // default node points to first statement of the block
      } else {
        codegen_statement(get_child(node, 0));
      }

      node = get_child(node, 1);
    }

    // if we fell through the switch, we need to remove the switch operand.
    // When exiting the switch with a break statement, the stack has already been adjusted.
    grow_stack(-1);
    grow_fs(-1);

    def_label(lbl1); // End of switch label

    cgc_fs = save_fs;
    cgc_locals = save_locals;

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
    if (binding != 0 AND heap[binding+4] != 0) {
      grow_stack(heap[binding+2] - cgc_fs);
      jump(heap[binding+4]); // jump to continue label
    } else {
      fatal_error("continue is not in the body of a loop");
    }

  } else if (op == RETURN_KW) {

    if (get_child(node, 0) != 0) {
      codegen_rvalue(get_child(node, 0));
      pop_reg(reg_X);
      grow_fs(-1);
    }

    grow_stack(-cgc_fs);

    ret();

  } else if (op == '{') {

    codegen_body(node);

  } else if (op == ':') {

    binding = cgc_lookup_goto_label(get_val(get_child(node, 0)), cgc_locals_fun);

    if (binding == 0) {
      cgc_add_goto_label(get_val(get_child(node, 0)), alloc_goto_label());
      binding = cgc_locals_fun;
    }

    def_goto_label(heap[binding + 3]);
    codegen_statement(get_child(node, 1)); // labelled statement

  } else if (op == GOTO_KW) {

    codegen_goto(node);

  } else {

    codegen_rvalue(node);
    pop_reg(reg_X);
    grow_fs(-1);

  }
}

void add_params(ast params) {

  ast decl;
  int ident;
  ast type;

  if (params != 0) {
    decl = get_child(params, 0);
    ident = get_child(decl, 0); // TODO: ident is not really a child
    type = get_child(decl, 1);

    if (cgc_lookup_var(ident, cgc_locals) != 0) {
      fatal_error("add_params: duplicate parameter");
    }

    cgc_add_local_param(ident, type_width_ast(type, false, true) / word_size, type);

    add_params(get_child(params, 1));
  }
}

void codegen_glo_fun_decl(ast node) {

  ast name = get_child(node, 0);
  ast fun_type = get_child(node, 1);
  ast params = get_child(node, 2);
  ast body = get_child(node, 3);
  int lbl;
  int binding;
  int save_locals_fun = cgc_locals_fun;

  if (get_op(fun_type) == STRUCT_KW && get_val(fun_type) == 0) {
    fatal_error("add_params: returning structs from function not supported");
  } else if (get_op(fun_type) == '[') {
    fatal_error("add_params: returning arrays from function not supported");
  }

  binding = cgc_lookup_fun(name, cgc_globals);

  if (binding == 0) {
    lbl = alloc_label();
    cgc_add_global_fun(name, lbl, fun_type);
    binding = cgc_globals;
  }

  if (body != 0) {

    lbl = heap[binding+4];

    def_label(lbl);

    cgc_fs = -1; // space for return address
    cgc_locals = 0;
    add_params(params);
    cgc_fs = 0;

    codegen_body(body);

    grow_stack(-cgc_fs);
    cgc_fs = 0;

    ret();
  }

  cgc_locals_fun = save_locals_fun;
}

void codegen_glo_decl(ast node) {

  int op = get_op(node);

  if (op == VAR_DECL) {
    codegen_glo_var_decl(node);
  } else if (op == FUN_DECL) {
    codegen_glo_fun_decl(node);
  } else if (op == TYPEDEF_KW) {
    handle_enum_struct_union_type_decl(get_child(node, 1));
  } else if (op == ENUM_KW OR op == STRUCT_KW OR op == UNION_KW) {
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
  int end_lbl = alloc_label();

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
}

void codegen_end() {

  def_label(setup_lbl);
  grow_stack_bytes(cgc_global_alloc);
  mov_reg_reg(reg_glo, reg_SP);

  os_allocate_memory(RT_HEAP_SIZE);       // Returns the heap start address in reg_X
  mov_mem_reg(reg_glo, 0, reg_X);         // init heap start
  mov_mem_reg(reg_glo, word_size, reg_X); // init bump pointer

  jump(init_start_lbl);

  def_label(init_next_lbl);
  setup_proc_args(cgc_global_alloc);
  call(main_lbl);
  os_exit();
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

  generate_exe();
}
