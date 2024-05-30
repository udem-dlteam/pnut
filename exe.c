// common part of machine code generators
const int word_size;

void generate_exe();

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

// Label definition

// TODO: generalize (this is currently specialized for x86 32 bit relative addressing)

enum {
  GENERIC_LABEL,
};

int alloc_label() {
  int lbl = alloc_obj(2);
  heap[lbl] = GENERIC_LABEL;
  heap[lbl + 1] = 0; // Address of label
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

const int char_width = 1;

const int reg_X;
const int reg_Y;
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

void load_mem_operand(int dst, int base, int offset, int width) {
  if (width == 1) {
    mov_reg_mem8(dst, base, offset);
  } else {
    mov_reg_mem(dst, base, offset);
  }
}

void write_mem_operand(int base, int offset, int src, int width) {
  if (width == 1) {
    mov_mem8_reg(base, offset, src);
  } else {
    mov_mem_reg(base, offset, src);
  }
}

void shift_for_pointer_arith(int reg, int type_width) {
  int i = 0;
  while (type_width > 1) {
    i += 1;
    type_width /= 2;
    add_reg_reg(reg, reg);
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

int cgc_fs = 0;
int cgc_locals = 0;
int cgc_globals = 0;
int cgc_global_alloc = 0;

enum {
  BINDING_PARAM_LOCAL,
  BINDING_VAR_LOCAL,
  BINDING_VAR_GLOBAL,
  BINDING_ENUM,
  BINDING_LOOP,
  BINDING_FUN,
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

void cgc_add_global(int ident, int size, int width, ast type) {
  int binding = alloc_obj(7);
  heap[binding+0] = cgc_globals;
  heap[binding+1] = BINDING_VAR_GLOBAL;
  heap[binding+2] = ident;
  heap[binding+3] = size;
  heap[binding+4] = cgc_global_alloc;
  heap[binding+5] = type;
  heap[binding+6] = width;
  cgc_global_alloc += size * width;
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
  heap[binding+1] = BINDING_ENUM;
  heap[binding+2] = ident;
  heap[binding+3] = value;
  cgc_globals = binding;
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
  int binding = env;
  while (binding != 0) {
    if (heap[binding+1] == BINDING_FUN && heap[binding+2] == ident) {
      break;
    }
    binding = heap[binding];
  }
  return binding;
}

int cgc_lookup_enclosing_loop(int env) {
  int binding = env;
  while (binding != 0) {
    if (heap[binding+1] == BINDING_LOOP) {
      break;
    }
    binding = heap[binding];
  }
  return binding;
}

int cgc_lookup_enum(int ident, int env) {
  int binding = env;
  while (binding != 0) {
    if (heap[binding+1] == BINDING_ENUM && heap[binding+2] == ident) {
      break;
    }
    binding = heap[binding];
  }
  return binding;
}

// A pointer type is either an array type or a type with at least one star
bool is_pointer_type(ast type) {
  return (get_op(type) == '[') | (get_val(type) != 0);
}

bool is_not_pointer_type(ast type) {
  return !is_pointer_type(type);
}

// Width of an object pointed to by a reference type.
int ref_type_width(ast type) {
  ast inner_type;

  if (get_op(type) == '[') {
    inner_type = get_child(type, 1);
    // For array of char (and not a pointer), width is 1.
    if (get_op(inner_type) == CHAR_KW AND get_val(inner_type) == 0) return char_width; // char[]
    else return word_size;
  } else if (get_op(type) == CHAR_KW AND get_val(type) == 1) { // char*
    return char_width;
  } else {
    return word_size;
  }
}

ast int_type;
ast char_type;
ast string_type;
ast void_type;

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
          binding = cgc_lookup_enum(ident, cgc_globals);
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
      // TODO: Check that it's a pointable object?
      return word_size; // always return an address
    } else if (op == '+' OR op == '-' OR op == '~' OR op == '!' OR op == MINUS_MINUS OR op == PLUS_PLUS) {
      // Unary operation don't change the type
      return value_type(get_child(node, 0));
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
    } else {
      fatal_error("value_type: unknown expression");
    }

  } else if (nb_children == 3) {

    if (op == '?') {
      fatal_error("value_type: ternary operator not supported");
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

      if (is_pointer_type(left_type) & is_not_pointer_type(right_type)) {
        shift_for_pointer_arith(reg_Y, ref_type_width(left_type));
      }

      if (is_pointer_type(right_type) & is_not_pointer_type(left_type)) {
        shift_for_pointer_arith(reg_X, ref_type_width(right_type));
      }

      add_reg_reg(reg_X, reg_Y);
    }
    else if (op == '-' OR op == MINUS_EQ OR op == MINUS_MINUS_PRE OR op == MINUS_MINUS_POST) {
      // Pointer subtraction is only valid if one of the operands is a pointer
      // When both operands are pointers, the result is the difference between the two pointers divided by the width of the target object.
      // When one operand is a pointer and the other is an integer, the result is the pointer minus the integer times the width of the target object.

      if (1) {
        if (is_pointer_type(left_type) & is_pointer_type(right_type)) {
          fatal_error("codegen_binop: subtraction between pointers not implemented");
        } else if (is_pointer_type(left_type)) {
          shift_for_pointer_arith(reg_Y, ref_type_width(left_type));
        } else if (is_pointer_type(right_type)) {
          shift_for_pointer_arith(reg_X, ref_type_width(right_type));
        }
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
        shift_for_pointer_arith(reg_Y, ref_type_width(left_type));
        width = ref_type_width(left_type);
      } else if (is_pointer_type(right_type) AND is_not_pointer_type(left_type)) {
        shift_for_pointer_arith(reg_X, ref_type_width(right_type));
        width = ref_type_width(right_type);
      } else {
        fatal_error("codegen_binop: invalid array access operands");
      }

      add_reg_reg(reg_X, reg_Y);
      load_mem_operand(reg_X, reg_X, 0, width);
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("codegen_binop: unknown op");
    }
  }

  push_reg(reg_X);
}

void grow_fs(int words) {
  cgc_fs += words;
}

int round_up_to_word_size(int n) {
  return (n + word_size - 1) / word_size * word_size;
}

void grow_stack(int words) {
  if (words != 0)
    add_reg_imm(reg_SP, -words * word_size);
}

// Like grow_stack, but takes bytes instead of words.
// To maintain alignment, the stack is grown by a multiple of word_size (rounded
// up from the number of bytes).
void grow_stack_bytes(int bytes) {
  add_reg_imm(reg_SP, -round_up_to_word_size(bytes));
}

#ifndef PNUT_CC
void codegen_rvalue(ast node);
void codegen_statement(ast node);
#endif

int codegen_params(ast params) {

  int nb_params = 0;

  if (params != 0) {
    if (get_op(params) == ',') {
      nb_params = 1 + codegen_params(get_child(params, 1));
      codegen_rvalue(get_child(params, 0));
    } else {
      nb_params = 1;
      codegen_rvalue(params);
    }
  }

  return nb_params;
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

// Return the width of the lvalue
int codegen_lvalue(ast node) {

  int op = get_op(node);
  int nb_children = get_nb_children(node);
  int binding;
  int lvalue_width = word_size;

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
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("codegen_lvalue: unknown lvalue with nb_children == 0");
    }

  } else if (nb_children == 1) {

    if (op == '*') {
      codegen_rvalue(get_child(node, 0));
      grow_fs(-1);
    } else {
      putstr("op="); putint(op); putchar('\n');
      fatal_error("codegen_lvalue: unexpected operator");
    }

  } else if (nb_children == 2) {

    if (op == '[') {
      codegen_rvalue(get_child(node, 0));
      codegen_rvalue(get_child(node, 1));
      codegen_binop('+', get_child(node, 0), get_child(node, 1));
      grow_fs(-2);
    } else {
      fatal_error("codegen_lvalue: unknown lvalue");
    }

  } else {
    putstr("op="); putint(op); putchar('\n');
    fatal_error("codegen_lvalue: unknown lvalue with >2 children");
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
  int lbl;
  int left_width;

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
        // There are 3 different cases for the type of the lvalue:
        // 1. Array type: the value is stored on the stack, and reg_X already points to it
        // 2. Pointer type: the pointer (to the heap) is stored on the stack, and it needs to be dereferenced
        // 3. Non-pointer type: the value is stored directly in the stack, and it needs to be loaded
        if (get_op(heap[binding+5]) != '[') {
          mov_reg_mem(reg_X, reg_X, 0);
        }
        push_reg(reg_X);
      } else {
        binding = cgc_lookup_var(ident, cgc_globals);
        if (binding != 0) {
          mov_reg_imm(reg_X, heap[binding+4]);
          add_reg_reg(reg_X, reg_glo);
          if (get_op(heap[binding+5]) != '[') {
            mov_reg_mem(reg_X, reg_X, 0);
          }
          push_reg(reg_X);
        } else {
          binding = cgc_lookup_enum(ident, cgc_globals);
          if (binding != 0) {
            mov_reg_imm(reg_X, -get_val(heap[binding+3]));
            push_reg(reg_X);
          } else {
            printf("ident = %s\n", string_pool+get_val(ident));
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
      load_mem_operand(reg_X, reg_Y, 0, ref_type_width(value_type(get_child(node, 0))));
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
      left_width = codegen_lvalue(get_child(node, 0));
      codegen_rvalue(get_child(node, 1));
      pop_reg(reg_X);
      pop_reg(reg_Y);
      grow_fs(-2);
      write_mem_operand(reg_Y, 0, reg_X, left_width);
      push_reg(reg_X);
    } else if (op == AMP_EQ OR op == BAR_EQ OR op == CARET_EQ OR op == LSHIFT_EQ OR op == MINUS_EQ OR op == PERCENT_EQ OR op == PLUS_EQ OR op == RSHIFT_EQ OR op == SLASH_EQ OR op == STAR_EQ) {
      left_width = codegen_lvalue(get_child(node, 0));
      pop_reg(reg_Y);
      push_reg(reg_Y);
      load_mem_operand(reg_X, reg_Y, 0, left_width);
      push_reg(reg_X);
      grow_fs(1);
      codegen_rvalue(get_child(node, 1));
      codegen_binop(op, get_child(node, 0), get_child(node, 1));
      pop_reg(reg_X);
      pop_reg(reg_Y);
      grow_fs(-3);
      write_mem_operand(reg_Y, 0, reg_X, left_width);
      push_reg(reg_X);
    } else if (op == AMP_AMP OR op == BAR_BAR) {
      lbl = alloc_label();
      codegen_rvalue(get_child(node, 0));
      pop_reg(reg_X);
      grow_fs(-1);
      push_reg(reg_X);
      xor_reg_reg(reg_Y, reg_Y);
      if (op == AMP_AMP) {
        jump_cond_reg_reg(EQ, lbl, reg_X, reg_Y);
      } else {
        jump_cond_reg_reg(NE, lbl, reg_X, reg_Y);
      }
      pop_reg(reg_X);
      codegen_rvalue(get_child(node, 1));
      grow_fs(-1);
      def_label(lbl);
    } else if (op == '(') {
      codegen_call(node);
    } else {
      fatal_error("codegen_rvalue: unknown rvalue");
    }

  } else if (nb_children == 3) {

    if (op == '?') {
      fatal_error("codegen_rvalue: ternary operator not supported");
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


  int_type = new_ast0(INT_KW, 0);
  char_type = new_ast0(CHAR_KW, 0);
  string_type = new_ast0(CHAR_KW, 1);
  void_type = new_ast0(VOID_KW, 0);

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

  jump(setup_lbl);
}

void codegen_glo_var_decl(ast node) {

  ast name = get_child(node, 0);
  ast type = get_child(node, 1);
  ast init = get_child(node, 2);
  int size;
  int width = ref_type_width(type);
  int binding = cgc_lookup_var(name, cgc_globals);

  if (get_op(type) == '[') { // Array declaration
    size = get_val(get_child(type, 0));
  } else {
    // All non-array types are represented as a word, even if they are smaller
    size = 1;
  }

  if (binding == 0) {
    cgc_add_global(name, size, width, type);
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
          size = get_val(get_child(type, 0));
          cgc_add_local(name, size, type);
          grow_stack_bytes(size * ref_type_width(type));
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
          cgc_add_local(name, size, type);
        }

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

  } else if (op == BREAK_KW) {

    binding = cgc_lookup_enclosing_loop(cgc_locals);
    if (binding != 0) {
      grow_stack(heap[binding+2] - cgc_fs);
      jump(heap[binding+3]); // jump to break label
    } else {
      fatal_error("break is not in the body of a loop");
    }

  } else if (op == CONTINUE_KW) {

    binding = cgc_lookup_enclosing_loop(cgc_locals);
    if (binding != 0) {
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

    cgc_add_local_param(ident, 1, type);

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

  if (body != 0) {

    binding = cgc_lookup_fun(name, cgc_globals);
    if (binding == 0) {
      lbl = alloc_label();
      cgc_add_global_fun(name, lbl, fun_type);
      binding = cgc_globals;
    }

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
}

void codegen_enum(ast node) {
  ast cases = get_child(node, 1);

  while (get_op(cases) == ',') {
    cgc_add_enum(get_val(get_child(cases, 0)), get_child(cases, 1));
    cases = get_child(cases, 2);
  }
}

void codegen_glo_decl(ast node) {

  int op = get_op(node);

  if (op == VAR_DECL) {
    codegen_glo_var_decl(node);
  } else if (op == FUN_DECL) {
    codegen_glo_fun_decl(node);
  } else if (op == ENUM_KW) {
    codegen_enum(node);
  } else {
    putstr("op="); putint(op);
    putstr(" with "); putint(get_nb_children(node)); putstr(" children\n");
    fatal_error("codegen_glo_decl: unexpected declaration");
  }
}

void codegen_end() {

  def_label(setup_lbl);
  grow_stack_bytes(cgc_global_alloc);
  mov_reg_reg(reg_glo, reg_SP);

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

  generate_exe();
}
