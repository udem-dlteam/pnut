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
  code[code_alloc++] = (a & 255);
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
  putchar(n & 255);
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
  write_4_i8(#x7f, #x45, #x4c, #x46); /* e_ident */
  write_4_i8(#x01, #x01, #x01, #x00);
  write_4_i8(#x00, #x00, #x00, #x00);
  write_4_i8(#x00, #x00, #x00, #x00);
  write_2_i8(#x02, #x00);             /* e_type */
  write_2_i8(#x03, #x00);             /* e_machine */
  write_4_i8(#x01, #x00, #x00, #x00); /* e_version */
  write_4_i8(#x54, #x80, #x04, #x08); /* e_entry */
  write_4_i8(#x34, #x00, #x00, #x00); /* e_phoff */
  write_4_i8(#x00, #x00, #x00, #x00); /* e_shoff */
  write_4_i8(#x00, #x00, #x00, #x00); /* e_flags */
  write_2_i8(#x34, #x00);             /* e_ehsize */
  write_2_i8(#x20, #x00);             /* e_phentsize */
  write_2_i8(#x01, #x00);             /* e_phnum */
  write_2_i8(#x00, #x00);             /* e_shentsize */
  write_2_i8(#x00, #x00);             /* e_shnum */
  write_2_i8(#x00, #x00);             /* e_shstrndx */
}

void write_Elf32_Phdr() {
  write_i32_le(1);                 /* p_type */
  write_i32_le(0);                 /* p_offset */
  write_i32_le(#x08048000);        /* p_vaddr */
  write_i32_le(#x08048000);        /* p_paddr */
  write_i32_le(#x54 + code_alloc); /* p_filesz */
  write_i32_le(#x54 + code_alloc); /* p_memsz */
  write_i32_le(5);                 /* p_flags */
  write_i32_le(#x1000);            /* p_align */
}

void write_elf() {

  int i = 0;

  write_Elf32_Ehdr();
  write_Elf32_Phdr();

  while (i < code_alloc) {
     write_i8(code[i++]);
  }
}

int X86_AX = 0;
int X86_CX = 1;
int X86_DX = 2;
int X86_BX = 3;
int X86_SP = 4;
int X86_BP = 5;
int X86_SI = 6;
int X86_DI = 7;

void x86_op_reg_reg(int opcode, int dst, int src) {
  /*  emit_i8(#x48);  REX.W */
  emit_i8(opcode);
  emit_i8(#xc0 + 8*src + dst); /* ModR/M */
}

void x86_mov_reg_reg(int dst, int src) { x86_op_reg_reg(#x89, dst, src); }
void x86_add_reg_reg(int dst, int src) { x86_op_reg_reg(#x01, dst, src); }
void x86_or_reg_reg (int dst, int src) { x86_op_reg_reg(#x09, dst, src); }
void x86_adc_reg_reg(int dst, int src) { x86_op_reg_reg(#x11, dst, src); }
void x86_sbb_reg_reg(int dst, int src) { x86_op_reg_reg(#x19, dst, src); }
void x86_and_reg_reg(int dst, int src) { x86_op_reg_reg(#x21, dst, src); }
void x86_sub_reg_reg(int dst, int src) { x86_op_reg_reg(#x29, dst, src); }
void x86_xor_reg_reg(int dst, int src) { x86_op_reg_reg(#x31, dst, src); }
void x86_cmp_reg_reg(int dst, int src) { x86_op_reg_reg(#x39, dst, src); }

void x86_test_reg_reg(int dst, int src) { x86_op_reg_reg(#x85, dst, src); }
void x86_xchg_reg_reg(int dst, int src) { x86_op_reg_reg(#x87, dst, src); }

void x86_mov_reg_i32(int dst, int n) { emit_i8(#xb8 + dst); emit_i32_le(n); }
void x86_push_reg(int src) { emit_i8(#x50 + src); }
void x86_pop_reg (int dst) { emit_i8(#x58 + dst); }

void x86_dec_eax() { emit_i8(#x48); } /* dec eax */
void x86_test_eax_eax() { emit_2_i8(#x85, #xc0); } /* test eax, eax */
void x86_jne_short(int n) { emit_2_i8(#x75, n); } /* jne .+$1 */
void x86_int_i8(int n) { emit_2_i8(#xcd, n); } /* int <i8> */
void x86_ret() { emit_i8(#xc3); } /* ret */

void x86_linux32_getchar() {
  x86_mov_reg_i32(X86_AX, 0); /* mov  eax, 0 */
  x86_push_reg(X86_AX);       /* push eax      # buffer to read byte */
  x86_mov_reg_i32(X86_BX, 0); /* mov  ebx, 0   # ebx = 0 = STDIN */
  x86_mov_reg_i32(X86_DX, 1); /* mov  edx, 1   # edx = 1 = number of bytes to read */
  x86_mov_reg_reg(X86_CX, X86_SP);  /* mov  ecx, esp # to the stack */
  x86_mov_reg_i32(X86_AX, 3); /* mov  eax, 3   # SYS_READ */
  x86_int_i8(#x80);           /* int  0x80     # system call */
  x86_test_eax_eax();         /* test eax, eax */
  x86_pop_reg(X86_AX);        /* pop  eax */
  x86_jne_short(1);           /* jne  .+1      # skip dec */
  x86_dec_eax();              /* dec  eax      # -1 on EOF */
}

void x86_linux32_putchar() {
  x86_push_reg(X86_AX);       /* push eax      # buffer to write byte */
  x86_mov_reg_i32(X86_BX, 1); /* mov  ebx, 1   # ebx = 1 = STDOUT */
  x86_mov_reg_i32(X86_DX, 1); /* mov  edx, 1   # edx = 1 = number of bytes to write */
  x86_mov_reg_reg(X86_CX, X86_SP);  /* mov  ecx, esp # from the stack */
  x86_mov_reg_i32(X86_AX, 4); /* mov  eax, 4   # SYS_WRITE */
  x86_int_i8(#x80);           /* int  0x80     # system call */
  x86_pop_reg(X86_AX);        /* pop  eax */
}

void x86_linux32_exit() {
  x86_mov_reg_reg(X86_BX, X86_AX);  /* mov  ebx, eax */
  x86_mov_reg_i32(X86_AX, 1);       /* mov  eax, 1   # SYS_EXIT */
  x86_int_i8(#x80);                 /* int  0x80     # system call */
}

void x86_linux32_print_msg(char_ptr msg) {
  char c;
  char_ptr p = msg;
  while ((c = *(p++)) != 0) {
    x86_mov_reg_i32(X86_AX, c);    /* mov  eax, c */
    x86_linux32_putchar(); /* putchar */
  }
}

void codegen() {
  x86_linux32_print_msg("hello world!\n");
  x86_mov_reg_i32(X86_AX, 0); /* mov  eax, 0 */
  x86_linux32_exit(); /* exit process with code 0 */
}

int main() {
  codegen();
  write_elf();
  return 0;
}
