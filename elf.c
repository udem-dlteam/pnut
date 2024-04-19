// ELF file output

// Minimal ELF 32 bit header.
// https://web.archive.org/web/20240409140025/http://www.muppetlabs.com/~breadbox/software/tiny/teensy.html
// https://web.archive.org/web/20240414151854/https://en.wikipedia.org/wiki/Executable_and_Linkable_Format

// For a minimal ELF 64 bit header
// see: https://web.archive.org/web/20231127152001/https://nathanotterness.com/2021/10/tiny_elf_modernized.html

void write_elf_e_header() {
  write_4_i8(0x7f, 0x45, 0x4c, 0x46); // e_ident
  write_4_i8(0x01, 0x01, 0x01, 0x00);
  write_4_i8(0x00, 0x00, 0x00, 0x00);
  write_4_i8(0x00, 0x00, 0x00, 0x00);
  write_2_i8(0x02, 0x00);             // e_type (2=ET_EXEC = an executable file)
  write_2_i8(0x03, 0x00);             // e_machine (3=x86)
  write_4_i8(0x01, 0x00, 0x00, 0x00); // e_version
  write_4_i8(0x54, 0x80, 0x04, 0x08); // e_entry
  write_4_i8(0x34, 0x00, 0x00, 0x00); // e_phoff
  write_4_i8(0x00, 0x00, 0x00, 0x00); // e_shoff
  write_4_i8(0x00, 0x00, 0x00, 0x00); // e_flags
  write_2_i8(0x34, 0x00);             // e_ehsize
  write_2_i8(0x20, 0x00);             // e_phentsize
  write_2_i8(0x01, 0x00);             // e_phnum
  write_2_i8(0x00, 0x00);             // e_shentsize
  write_2_i8(0x00, 0x00);             // e_shnum
  write_2_i8(0x00, 0x00);             // e_shstrndx
}

void write_elf_p_header() {
  write_i32_le(1);                 // p_type (1=PT_LOAD = a loadable segment)
  write_i32_le(0);                 // p_offset
  write_i32_le(0x08048000);        // p_vaddr
  write_i32_le(0x08048000);        // p_paddr
  write_i32_le(0x54 + code_alloc); // p_filesz
  write_i32_le(0x54 + code_alloc); // p_memsz
  write_i32_le(5);                 // p_flags
  write_i32_le(0x1000);            // p_align
}

void generate_exe() {

  int i = 0;

  write_elf_e_header();
  write_elf_p_header();

  while (i < code_alloc) {
     write_i8(code[i]);
     i += 1;
  }
}
