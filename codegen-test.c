#ifndef SIX_CC

#include <stdio.h>
#include <stdlib.h>
typedef FILE *FILE_ptr;
typedef int *int_ptr;
typedef char *char_ptr;

#endif

int code[10000];
int code_alloc = 0;


void emit_i8(int a) {
  code[code_alloc++] = (a & 0xff);
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

void write_Elf32_Ehdr() {
  write_4_i8(0x7f, 0x45, 0x4c, 0x46); /* e_ident */
  write_4_i8(0x01, 0x01, 0x01, 0x00);
  write_4_i8(0x00, 0x00, 0x00, 0x00);
  write_4_i8(0x00, 0x00, 0x00, 0x00);
  write_2_i8(0x02, 0x00);             /* e_type */
  write_2_i8(0x03, 0x00);             /* e_machine */
  write_4_i8(0x01, 0x00, 0x00, 0x00); /* e_version */
  write_4_i8(0x54, 0x80, 0x04, 0x08); /* e_entry */
  write_4_i8(0x34, 0x00, 0x00, 0x00); /* e_phoff */
  write_4_i8(0x00, 0x00, 0x00, 0x00); /* e_shoff */
  write_4_i8(0x00, 0x00, 0x00, 0x00); /* e_flags */
  write_2_i8(0x34, 0x00);             /* e_ehsize */
  write_2_i8(0x20, 0x00);             /* e_phentsize */
  write_2_i8(0x01, 0x00);             /* e_phnum */
  write_2_i8(0x00, 0x00);             /* e_shentsize */
  write_2_i8(0x00, 0x00);             /* e_shnum */
  write_2_i8(0x00, 0x00);             /* e_shstrndx */
}

void write_Elf32_Phdr() {
  write_i32_le(1);                 /* p_type */
  write_i32_le(0);                 /* p_offset */
  write_i32_le(0x08048000);        /* p_vaddr */
  write_i32_le(0x08048000);        /* p_paddr */
  write_i32_le(0x54 + code_alloc); /* p_filesz */
  write_i32_le(0x54 + code_alloc); /* p_memsz */
  write_i32_le(5);                 /* p_flags */
  write_i32_le(0x1000);            /* p_align */
}

void write_elf() {

  int i = 0;

  write_Elf32_Ehdr();
  write_Elf32_Phdr();

  while (i < code_alloc) {
     write_i8(code[i++]);
  }
}

int AX = 0;
int CX = 1;
int DX = 2;
int BX = 3;
int SP = 4;
int BP = 5;
int SI = 6;
int DI = 7;

int x86_64 = 0;

void rex_prefix() {
  if (x86_64) emit_i8(0x48); /* REX.W */
}

void op_reg_reg(int opcode, int dst, int src) {
  rex_prefix();
  emit_i8(opcode);
  emit_i8(0xc0 + 8*src + dst); /* ModR/M */
}

/* probably not essential */
void inc_reg(int dst) { rex_prefix(); emit_2_i8(0xff, 0xc0 + dst); }
void dec_reg(int dst) { rex_prefix(); emit_2_i8(0xff, 0xc8 + dst); }
void xchg_reg_reg(int dst, int src) { op_reg_reg(0x87, dst, src); }
void test_reg_reg(int dst, int src) { op_reg_reg(0x85, dst, src); }
void jcond_short(int cond, int n) { emit_2_i8(0x70 + cond, n); }

void not_reg(int dst) { rex_prefix(); emit_2_i8(0xf7, 0xd0 + dst); }
void neg_reg(int dst) { rex_prefix(); emit_2_i8(0xf7, 0xd8 + dst); }

void shl_reg_cl(int dst) { rex_prefix(); emit_2_i8(0xd3, 0xe0 + dst); }
void shr_reg_cl(int dst) { rex_prefix(); emit_2_i8(0xd3, 0xe8 + dst); }
void sar_reg_cl(int dst) { rex_prefix(); emit_2_i8(0xd3, 0xf8 + dst); }

void imul_reg_reg(int dst, int src) {
  rex_prefix();
  emit_2_i8(0x0f, 0xaf);
  emit_i8(0xc0 + 8*dst + src); /* ModR/M */
}

void idiv_reg(int src) {
  rex_prefix();
  emit_2_i8(0xf7, 0xf8 + src);
}

void mov_reg_reg(int dst, int src) { op_reg_reg(0x89, dst, src); }
void add_reg_reg(int dst, int src) { op_reg_reg(0x01, dst, src); }
void or_reg_reg (int dst, int src) { op_reg_reg(0x09, dst, src); }
void and_reg_reg(int dst, int src) { op_reg_reg(0x21, dst, src); }
void sub_reg_reg(int dst, int src) { op_reg_reg(0x29, dst, src); }
void xor_reg_reg(int dst, int src) { op_reg_reg(0x31, dst, src); }
void cmp_reg_reg(int dst, int src) { op_reg_reg(0x39, dst, src); }

void mov_reg_i32(int dst, int n) { emit_i8(0xb8 + dst); emit_i32_le(n); }
void push_reg(int src) { emit_i8(0x50 + src); }
void pop_reg (int dst) { emit_i8(0x58 + dst); }

void ret() { emit_i8(0xc3); }

void cdq_cqo() { rex_prefix(); emit_i8(0x99); }

int EQ = 0x4;
int NE = 0x5;
int LT = 0xc;
int GE = 0xd;
int LE = 0xe;
int GT = 0xf;

void jcond(int cond, int n) { emit_2_i8(0x0f, 0x80 + cond); emit_i32_le(n); }

void int_i8(int n) { emit_2_i8(0xcd, n); } /* int <n> */

void linux32_getchar() {
  mov_reg_i32(AX, 0);    /* mov  eax, 0 */
  push_reg(AX);          /* push eax      # buffer to read byte */
  mov_reg_i32(BX, 0);    /* mov  ebx, 0   # ebx = 0 = STDIN */
  mov_reg_i32(DX, 1);    /* mov  edx, 1   # edx = 1 = number of bytes to read */
  mov_reg_reg(CX, SP);   /* mov  ecx, esp # to the stack */
  mov_reg_i32(AX, 3);    /* mov  eax, 3   # SYS_READ */
  int_i8(0x80);          /* int  0x80     # system call */
  test_reg_reg(AX, AX);  /* test eax, eax */
  pop_reg(AX);           /* pop  eax */
  jcond(NE, 2+x86_64);   /* jne  .+2      # skip dec */
  dec_reg(AX);           /* dec  eax      # -1 on EOF */
}

void linux32_putchar() {
  push_reg(AX);         /* push eax      # buffer to write byte */
  mov_reg_i32(BX, 1);   /* mov  ebx, 1   # ebx = 1 = STDOUT */
  mov_reg_i32(DX, 1);   /* mov  edx, 1   # edx = 1 = number of bytes to write */
  mov_reg_reg(CX, SP);  /* mov  ecx, esp # from the stack */
  mov_reg_i32(AX, 4);   /* mov  eax, 4   # SYS_WRITE */
  int_i8(0x80);         /* int  0x80     # system call */
  pop_reg(AX);          /* pop  eax */
}

void linux32_exit() {
  mov_reg_reg(BX, AX);  /* mov  ebx, eax */
  mov_reg_i32(AX, 1);   /* mov  eax, 1   # SYS_EXIT */
  int_i8(0x80);         /* int  0x80     # system call */
}

void linux32_print_msg(char_ptr msg) {
  char c;
  char_ptr p = msg;
  while ((c = *(p++)) != 0) {
    mov_reg_i32(AX, c);  /* mov  eax, c */
    linux32_putchar();   /* putchar */
  }
}

void binop(int op) {

  int result_reg = AX;
  int cond;

  pop_reg(CX); /* rhs operand */
  pop_reg(AX); /* lhs operand */

  if      (op == '+') add_reg_reg(AX, CX);
  else if (op == '-') sub_reg_reg(AX, CX);
  else if (op == '*') imul_reg_reg(AX, CX);
  else if (op == '/') { cdq_cqo(); idiv_reg(CX); }
  else if (op == '%') { cdq_cqo(); idiv_reg(CX); result_reg = DX; }
  else if (op == '{') shl_reg_cl(AX);
  else if (op == '}') sar_reg_cl(AX);
  else if (op == '&') and_reg_reg(AX, CX);
  else if (op == '|') or_reg_reg(AX, CX);
  else if (op == '^') xor_reg_reg(AX, CX);
  else {
    cmp_reg_reg(AX, CX);
    mov_reg_i32(AX, 1);
    if      (op == '<') cond = LT;
    else if (op == '>') cond = GT;
    else if (op == '=') cond = EQ;
    else if (op == '!') cond = NE;
    else if (op == '[') cond = LE;
    else                cond = GE;
    jcond(cond, 5+x86_64);
    mov_reg_i32(AX, 0);
  }

  push_reg(result_reg);
}

void codegen() {

  linux32_print_msg("hello world!\n");

  /* test evaluation of 49 % 5 */

  mov_reg_i32(AX, 19);
  push_reg(AX);

  mov_reg_i32(AX, 3);
  push_reg(AX);

  binop('/');

  pop_reg(AX);
  linux32_exit(); /* exit process with code 0 */
}

int main() {
  codegen();
  write_elf();
  return 0;
}
