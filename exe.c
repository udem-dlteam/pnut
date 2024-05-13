// common part of machine code generators
void generate_exe();

int code[100000];
int code_alloc = 0;

void emit_i8(int a) {
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
    emit_i32_le(0); //TODO: Emit the next 32 bits w/out overflow no longs atm and linux 64 bit int = 4 bytes

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

int alloc_label() {
  int lbl = alloc_obj(1);
  heap[lbl] = 0;
  return lbl;
}

void use_label(int lbl) {

  int addr = heap[lbl];

  if (addr < 0) {
    // label address is currently known
    addr = -addr - (code_alloc + 4); // compute relative address
    emit_i32_le(addr);
  } else {
    // label address is not yet known
    emit_i32_le(0); // 32 bit placeholder for distance
    code[code_alloc-1] = addr; // chain with previous patch address
    heap[lbl] = code_alloc;
  }
}

void def_label(int lbl) {

  int addr = heap[lbl];
  int label_addr = code_alloc;
  int next;

  if (addr < 0) {
    fatal_error("label multiply defined");
  } else {
    heap[lbl] = -label_addr; // define label's address
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

const int word_size;

const int reg_X;
const int reg_Y;
const int reg_SP;
const int reg_glo;

void mov_reg_imm(int dst, int imm);
void mov_reg_reg(int dst, int src);
void mov_mem_reg(int base, int offset, int src);
void mov_reg_mem(int dst, int base, int offset);

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
void call(int lbl);
void ret();

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

void cgc_add_local_param(int ident, int size, ast type) {
  int binding = alloc_obj(5);
  heap[binding+0] = cgc_locals;
  heap[binding+1] = ident;
  heap[binding+2] = size;
  heap[binding+3] = cgc_fs;
  heap[binding+4] = type;
  cgc_fs -= size;
  cgc_locals = binding;
}

void cgc_add_local(int ident, int size, ast type) {
  int binding = alloc_obj(5);
  cgc_fs += size;
  heap[binding+0] = cgc_locals;
  heap[binding+1] = ident;
  heap[binding+2] = size;
  heap[binding+3] = cgc_fs;
  heap[binding+4] = type;
  cgc_locals = binding;
}

void cgc_add_enclosing_loop(int loop_fs, int break_lbl, ast continue_lbl) {
  int binding = alloc_obj(5);
  heap[binding+0] = cgc_locals;
  heap[binding+1] = 0;
  heap[binding+2] = loop_fs;
  heap[binding+3] = break_lbl;
  heap[binding+4] = continue_lbl;
  cgc_locals = binding;
}

void cgc_add_global(int ident, int size, ast type) {
  int binding = alloc_obj(5);
  heap[binding+0] = cgc_globals;
  heap[binding+1] = ident;
  heap[binding+2] = size;
  heap[binding+3] = cgc_global_alloc;
  heap[binding+4] = type;
  cgc_global_alloc += size;
  cgc_globals = binding;
}

void cgc_add_global_fun(int ident, int label) {
  int binding = alloc_obj(4);
  heap[binding+0] = cgc_globals;
  heap[binding+1] = ident;
  heap[binding+2] = 0;
  heap[binding+3] = label;
  cgc_globals = binding;
}

int cgc_lookup_var(int ident, int env) {
  int binding = env;
  while (binding != 0) {
    if (heap[binding+1] == ident && heap[binding+2] != 0) {
      break;
    }
    binding = heap[binding];
  }
  return binding;
}

int cgc_lookup_fun(int ident, int env) {
  int binding = env;
  while (binding != 0) {
    if (heap[binding+1] == ident && heap[binding+2] == 0) {
      break;
    }
    binding = heap[binding];
  }
  return binding;
}

int cgc_lookup_enclosing_loop(int env) {
  int binding = env;
  while (binding != 0) {
    if (heap[binding+1] == 0) {
      break;
    }
    binding = heap[binding];
  }
  return binding;
}

void codegen_binop(int op) {

  int lbl1;
  int lbl2;
  int cond = -1;

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
    if      (op == '+' OR op == PLUS_EQ) add_reg_reg(reg_X, reg_Y);
    else if (op == '-' OR op == MINUS_EQ) sub_reg_reg(reg_X, reg_Y);
    else if (op == '*' OR op == STAR_EQ) mul_reg_reg(reg_X, reg_Y);
    else if (op == '/' OR op == SLASH_EQ) div_reg_reg(reg_X, reg_Y);
    else if (op == '%' OR op == PERCENT_EQ) rem_reg_reg(reg_X, reg_Y);
    else if (op == '&' OR op == AMP_EQ) and_reg_reg(reg_X, reg_Y);
    else if (op == '|' OR op == BAR_EQ) or_reg_reg(reg_X, reg_Y);
    else if (op == '^' OR op == CARET_EQ) xor_reg_reg(reg_X, reg_Y);
    else if (op == LSHIFT OR op == LSHIFT_EQ) shl_reg_reg(reg_X, reg_Y);
    else if (op == RSHIFT OR op == RSHIFT_EQ) sar_reg_reg(reg_X, reg_Y);
    else if (op == '[') {
      add_reg_reg(reg_Y, reg_Y);
      add_reg_reg(reg_Y, reg_Y);
      if (word_size == 8) add_reg_reg(reg_Y, reg_Y);
      add_reg_reg(reg_X, reg_Y);
      mov_reg_mem(reg_X, reg_X, 0);
    } else {
      printf("op=%d %c", op, op);
      fatal_error("codegen_binop: unknown op");
    }
  }

  push_reg(reg_X);
}

void grow_fs(int words) {
  cgc_fs += words;
}

void grow_stack(int words) {
  add_reg_imm(reg_SP, -words * word_size);
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
    cgc_add_global_fun(name, lbl);
    binding = cgc_globals;
  }

  call(heap[binding+3]);

  grow_stack(-nb_params);
  grow_fs(-nb_params);

  push_reg(reg_X);
}

void codegen_lvalue(ast node) {

  int op = get_op(node);
  int nb_children = get_nb_children(node);
  int binding;

  if (nb_children == 0) {

    if (op == IDENTIFIER) {
      binding = cgc_lookup_var(get_val(node), cgc_locals);
      if (binding != 0) {
        mov_reg_imm(reg_X, (cgc_fs - heap[binding+3]) * word_size);
        add_reg_reg(reg_X, reg_SP);
        push_reg(reg_X);
      } else {
        binding = cgc_lookup_var(get_val(node), cgc_globals);
        if (binding != 0) {
          mov_reg_imm(reg_X, heap[binding+3] * word_size);
          add_reg_reg(reg_X, reg_glo);
          push_reg(reg_X);
        } else {
          fatal_error("codegen_lvalue: identifier not found");
        }
      }
    } else {
      printf("op=%d %c", op, op);
      fatal_error("codegen_lvalue: unknown lvalue with nb_children == 0");
    }

  } else if (nb_children == 1) {

    if (op == '*') {
      codegen_rvalue(get_child(node, 0));
      grow_fs(-1);
    } else {
      printf("1: op=%d %c", op, op);
      fatal_error("codegen_lvalue: unexpected operator");
    }

  } else if (nb_children == 2) {

    if (op == '[') {
      codegen_rvalue(get_child(node, 0));
      codegen_rvalue(get_child(node, 1));
      pop_reg(reg_Y);
      add_reg_reg(reg_Y, reg_Y);
      add_reg_reg(reg_Y, reg_Y);
      if (word_size == 8) add_reg_reg(reg_Y, reg_Y);
      push_reg(reg_Y);
      codegen_binop('+');
      grow_fs(-2);
    } else {
      fatal_error("codegen_lvalue: unknown lvalue");
    }

  } else {
    printf("op=%d %c\n", op, op);
    fatal_error("codegen_lvalue: unknown lvalue with >2 children");
  }

  grow_fs(1);
}

void codegen_string(int start) {// TODO render generic to word_le

  int lbl = alloc_label();
  int i = start;

  call(lbl);

  while (string_pool[i] != 0) {
    emit_i64_le(string_pool[i]);
    i += 1;
  }

  emit_i64_le(0);

  def_label(lbl);
}

void codegen_c_string(char* start) {

  int lbl = alloc_label();
  int i = 0;

  call(lbl);

  while (start[i] != 0) {
    emit_i8(start[i]);
    i += 1;
  }

  emit_i8(0);

  def_label(lbl);
}


void codegen_rvalue(ast node) {

  int op = get_op(node);
  int nb_children = get_nb_children(node);
  int binding;
  int ident;
  int lbl;

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
        mov_reg_imm(reg_X, (cgc_fs - heap[binding+3]) * word_size);
        add_reg_reg(reg_X, reg_SP);
        if (get_op(heap[binding+4]) != '[') {
          mov_reg_mem(reg_X, reg_X, 0);
        }
        push_reg(reg_X);
      } else {
        binding = cgc_lookup_var(ident, cgc_globals);
        if (binding != 0) {
          mov_reg_imm(reg_X, heap[binding+3] * word_size);
          add_reg_reg(reg_X, reg_glo);
          if (get_op(heap[binding+4]) != '[') {
            mov_reg_mem(reg_X, reg_X, 0);
          }
          push_reg(reg_X);
        } else {
          printf("ident = %s\n", string_pool+get_val(ident));
          fatal_error("codegen_rvalue: identifier not found");
        }
      }
    } else if (op == STRING) {
      codegen_string(get_val(node));
    } else {
      printf("op=%d %c", op, op);
      fatal_error("codegen_rvalue: unknown rvalue with nb_children == 0");
    }

  } else if (nb_children == 1) {

    if (op == '*') {
      codegen_rvalue(get_child(node, 0));
      pop_reg(reg_Y);
      grow_fs(-1);
      mov_reg_mem(reg_X, reg_Y, 0);
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
      codegen_binop(EQ_EQ);
      grow_fs(-2);
    } else if (op == MINUS_MINUS) {
      // TODO
    } else if (op == PLUS_PLUS) {
      // TODO
    } else if (op == '&') {
      codegen_lvalue(get_child(node, 0));
      grow_fs(-1);
    } else {
      printf("1: op=%d %c", op, op);
      fatal_error("codegen_rvalue: unexpected operator");
    }

  } else if (nb_children == 2) {

    if (op == '+' OR op == '-' OR op == '*' OR op == '/' OR op == '%' OR op == '&' OR op == '|' OR op == '^' OR op == LSHIFT OR op == RSHIFT OR op == '<' OR op == '>' OR op == EQ_EQ OR op == EXCL_EQ OR op == LT_EQ OR op == GT_EQ OR op == '[') {
      codegen_rvalue(get_child(node, 0));
      codegen_rvalue(get_child(node, 1));
      codegen_binop(op);
      grow_fs(-2);
    } else if (op == '=') {
      codegen_lvalue(get_child(node, 0));
      codegen_rvalue(get_child(node, 1));
      pop_reg(reg_X);
      pop_reg(reg_Y);
      grow_fs(-2);
      mov_mem_reg(reg_Y, 0, reg_X);
      push_reg(reg_X);
    } else if (op == AMP_EQ OR op == BAR_EQ OR op == CARET_EQ OR op == LSHIFT_EQ OR op == MINUS_EQ OR op == PERCENT_EQ OR op == PLUS_EQ OR op == RSHIFT_EQ OR op == SLASH_EQ OR op == STAR_EQ) {
      codegen_lvalue(get_child(node, 0));
      pop_reg(reg_Y);
      push_reg(reg_Y);
      mov_reg_mem(reg_X, reg_Y, 0);
      push_reg(reg_X);
      grow_fs(1);
      codegen_rvalue(get_child(node, 1));
      codegen_binop(op);
      pop_reg(reg_X);
      pop_reg(reg_Y);
      grow_fs(-3);
      mov_mem_reg(reg_Y, 0, reg_X);
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
      printf("op=%d %c\n", op, op);
      fatal_error("codegen_rvalue: unknown rvalue with 3 children");
    }

  } else {
    printf("op=%d %c\n", op, op);
    fatal_error("codegen_rvalue: unknown rvalue with >4 children");
  }

  grow_fs(1);
}

void codegen_begin() {

  setup_lbl = alloc_label();
  init_start_lbl = alloc_label();
  init_next_lbl = init_start_lbl;

  main_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "main"), main_lbl);

  exit_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "exit"), exit_lbl);

  getchar_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "getchar"), getchar_lbl);

  putchar_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "putchar"), putchar_lbl);

  fopen_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "fopen"), fopen_lbl);

  fclose_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "fclose"), fclose_lbl);

  fgetc_lbl = alloc_label();
  cgc_add_global_fun(init_ident(IDENTIFIER, "fgetc"), fgetc_lbl);

  jump(setup_lbl);
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
    size = 1;
  }

  if (binding == 0) {
    cgc_add_global(name, size, type);
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

    mov_mem_reg(reg_glo, heap[binding+3] * word_size, reg_X);

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
          grow_stack(size);
        } else {
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

    // TODO

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
  ast params = get_child(node, 2);
  ast body = get_child(node, 3);
  int lbl;
  int binding;

  if (body != 0) {

    binding = cgc_lookup_fun(name, cgc_globals);
    if (binding == 0) {
      lbl = alloc_label();
      cgc_add_global_fun(name, lbl);
      binding = cgc_globals;
    }

    lbl = heap[binding+3];

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

void codegen_glo_decl(ast node) {

  int op = get_op(node);

  if (op == VAR_DECL) {
    codegen_glo_var_decl(node);
  } else if (op == FUN_DECL) {
    codegen_glo_fun_decl(node);
  } else {
    printf("op=%d %c with %d children\n", op, op, get_nb_children(node));
    fatal_error("codegen_glo_decl: unexpected declaration");
  }
}

void codegen_end() {

  def_label(setup_lbl);

  grow_stack(cgc_global_alloc);
  mov_reg_reg(reg_glo, reg_SP);

  jump(init_start_lbl);

  def_label(init_next_lbl);
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

  //fopen function
  def_label(fopen_lbl);
  mov_reg_mem(reg_X, reg_SP, 2*word_size);
  os_fopen();
  ret();

  //fclose function
  def_label(fclose_lbl);
  mov_reg_mem(reg_X, reg_SP, word_size);
  os_fclose();
  ret();

  //fgetc function
  def_label(fgetc_lbl);
  mov_reg_mem(reg_X, reg_SP, word_size);
  os_fgetc();
  ret();

  generate_exe();
}
