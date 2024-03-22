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

void emit_i8_i8(int a, int b) {
  int dummy;
  emit_i8(a);
  emit_i8(b);
}

void emit_i8_i8_i8_i8(int a, int b, int c, int d) {
  int dummy;
  emit_i8_i8(a, b);
  emit_i8_i8(c, d);
}

void emit_i32_le(int n) {
  int dummy;
  emit_i8_i8_i8_i8(n, n >> 8, n >> 16, n >> 24);
}

void write_i8(int n) {
  int dummy;
  putchar(n & 255);
}

void write_i8_i8(int a, int b) {
  int dummy;
  write_i8(a);
  write_i8(b);
}

void write_i8_i8_i8_i8(int a, int b, int c, int d) {
  int dummy;
  write_i8_i8(a, b);
  write_i8_i8(c, d);
}

void write_i32_le(int n) {
  int dummy;
  write_i8_i8_i8_i8(n, n >> 8, n >> 16, n >> 24);
}

void write_Elf32_Ehdr() {
  int dummy;
  write_i8_i8_i8_i8(#x7f, #x45, #x4c, #x46); /* e_ident */
  write_i8_i8_i8_i8(#x01, #x01, #x01, #x00);
  write_i8_i8_i8_i8(#x00, #x00, #x00, #x00);
  write_i8_i8_i8_i8(#x00, #x00, #x00, #x00);
  write_i8_i8(#x02, #x00);                   /* e_type */
  write_i8_i8(#x03, #x00);                   /* e_machine */
  write_i8_i8_i8_i8(#x01, #x00, #x00, #x00); /* e_version */
  write_i8_i8_i8_i8(#x54, #x80, #x04, #x08); /* e_entry */
  write_i8_i8_i8_i8(#x34, #x00, #x00, #x00); /* e_phoff */
  write_i8_i8_i8_i8(#x00, #x00, #x00, #x00); /* e_shoff */
  write_i8_i8_i8_i8(#x00, #x00, #x00, #x00); /* e_flags */
  write_i8_i8(#x34, #x00);                   /* e_ehsize */
  write_i8_i8(#x20, #x00);                   /* e_phentsize */
  write_i8_i8(#x01, #x00);                   /* e_phnum */
  write_i8_i8(#x00, #x00);                   /* e_shentsize */
  write_i8_i8(#x00, #x00);                   /* e_shnum */
  write_i8_i8(#x00, #x00);                   /* e_shstrndx */
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

void x86_mov_eax_i32(int n) { int dummy; emit_i8(#xb8); emit_i32_le(n); } /* mov eax, <i32> */
void x86_mov_ebx_i32(int n) { int dummy; emit_i8(#xbb); emit_i32_le(n); } /* mov ebx, <i32> */
void x86_mov_ecx_i32(int n) { int dummy; emit_i8(#xb9); emit_i32_le(n); } /* mov ecx, <i32> */
void x86_mov_edx_i32(int n) { int dummy; emit_i8(#xba); emit_i32_le(n); } /* mov edx, <i32> */
void x86_mov_ebx_eax() { int dummy; emit_i8_i8(#x89, #xc3); } /* mov eax, ebx */
void x86_mov_ecx_esp() { int dummy; emit_i8_i8(#x89, #xe1); } /* mov ecx, esp */
void x86_push_eax() { int dummy; emit_i8(#x50); } /* push eax */
void x86_push_ebx() { int dummy; emit_i8(#x53); } /* push ebx */
void x86_push_ecx() { int dummy; emit_i8(#x51); } /* push ecx */
void x86_push_edx() { int dummy; emit_i8(#x52); } /* push edx */
void x86_pop_eax() { int dummy; emit_i8(#x58); } /* pop eax */
void x86_pop_ebx() { int dummy; emit_i8(#x5b); } /* pop ebx */
void x86_pop_ecx() { int dummy; emit_i8(#x59); } /* pop ecx */
void x86_pop_edx() { int dummy; emit_i8(#x5a); } /* pop edx */
void x86_dec_eax() { int dummy; emit_i8(#x48); } /* dec eax */
void x86_test_eax_eax() { int dummy; emit_i8_i8(#x85, #xc0); } /* test eax, eax */
void x86_jne_short(int n) { int dummy; emit_i8_i8(#x75, n); } /* jne .+$1 */
void x86_int_i8(int n) { int dummy; emit_i8_i8(#xcd, n); } /* int <i8> */
void x86_ret() { int dummy; emit_i8(#xc3); } /* ret */

void x86_linux32_getchar() {
  int dummy;
  x86_mov_eax_i32(0); /* mov  eax, 0 */
  x86_push_eax();     /* push eax      # buffer to read byte */
  x86_mov_ebx_i32(0); /* mov  ebx, 0   # ebx = 0 = STDIN */
  x86_mov_edx_i32(1); /* mov  edx, 1   # edx = 1 = number of bytes to read */
  x86_mov_ecx_esp();  /* mov  ecx, esp # to the stack */
  x86_mov_eax_i32(3); /* mov  eax, 3   # SYS_READ */
  x86_int_i8(#x80);   /* int  0x80     # system call */
  x86_test_eax_eax(); /* test eax, eax */
  x86_pop_eax();      /* pop  eax */
  x86_jne_short(1);   /* jne  .+1      # skip dec */
  x86_dec_eax();      /* dec  eax      # -1 on EOF */
}

void x86_linux32_putchar() {
  int dummy;
  x86_push_eax();     /* push eax      # buffer to write byte */
  x86_mov_ebx_i32(1); /* mov  ebx, 1   # ebx = 1 = STDOUT */
  x86_mov_edx_i32(1); /* mov  edx, 1   # edx = 1 = number of bytes to write */
  x86_mov_ecx_esp();  /* mov  ecx, esp # from the stack */
  x86_mov_eax_i32(4); /* mov  eax, 4   # SYS_WRITE */
  x86_int_i8(#x80);   /* int  0x80     # system call */
  x86_pop_eax();      /* pop  eax */
}

void x86_linux32_exit() {
  int dummy;
  x86_mov_ebx_eax();  /* mov  ebx, eax */
  x86_mov_eax_i32(1); /* mov  eax, 1   # SYS_EXIT */
  x86_int_i8(#x80);   /* int  0x80     # system call */
}

void x86_linux32_print_msg(char_ptr msg) {
  char c;
  while ((c = *(msg++)) != 0) {
    x86_mov_eax_i32(c);    /* mov  eax, c */
    x86_linux32_putchar(); /* putchar */
  }
}

void codegen() {
  int dummy;
  x86_linux32_print_msg("hello world!\n");
  x86_mov_eax_i32(0); /* mov  eax, 0 */
  x86_linux32_exit(); /* exit process with code 0 */
}

int main() {
  int dummy;
  codegen();
  write_elf();
  return 0;
}
