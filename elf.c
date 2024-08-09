// ELF file output

// Minimal i386 bit ELF header.
// https://web.archive.org/web/20240409140025/http://www.muppetlabs.com/~breadbox/software/tiny/teensy.html
// https://web.archive.org/web/20240414151854/https://en.wikipedia.org/wiki/Executable_and_Linkable_Format
#ifdef target_i386_linux

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

#endif


// Minimal x86-64 ELF header.
// see: https://web.archive.org/web/20231127152001/https://nathanotterness.com/2021/10/tiny_elf_modernized.html
// Notes:
//  64-bit virtual offsets always start at 0x400000
//  https://web.archive.org/web/20210918175202/https://stackoverflow.com/questions/38549972/why-elf-executables-have-a-fixed-load-address
//  Convention set in the x86_64 system-v abi:
//  https://web.archive.org/web/20230322152842/https://refspecs.linuxfoundation.org/elf/x86_64-SysV-psABI.pdf (Page 26)
#ifdef target_x86_64_linux

void write_elf_e_header() {
  write_4_i8(0x7f, 0x45, 0x4c, 0x46); // Header signature |
  write_4_i8(0x02, 0x01, 0x01, 0x00); // Header flags (class = 2 = 64 bit, little endian = 1, elf version = 1, OS ABI unused = 0) |
  write_4_i8(0x00, 0x00, 0x00, 0x00); // Extended ABI byte + 7 bytes padding. Leave as 0, it's ignored | dq
  write_4_i8(0x00, 0x00, 0x00, 0x00); // Cont.. | dq cont..
  write_2_i8(0x02, 0x00); // ELF file type : executable = 2 | dw
  write_2_i8(0x3e, 0x00); // Target architecture. 0x3e = x86_64 | dw
  write_4_i8(0x01, 0x00, 0x00, 0x00); // ELF version  is 1, corresponds with header flags | dd
  write_4_i8(0x78, 0x00, 0x00, 0x40); // Entry point (common) (0x40000078)
  write_4_i8(0x00, 0x00, 0x00, 0x00); // Cont.. | dq cont..
  write_4_i8(0x40, 0x00, 0x00, 0x00); // Program header offset | dq | 0x400000 64 bit convention
  write_4_i8(0x00, 0x00, 0x00, 0x00); // Cont.. | dq cont..
  write_4_i8(0x00, 0x00, 0x00, 0x00); // Section header offset | dq
  write_4_i8(0x00, 0x00, 0x00, 0x00); // Cont..
  write_4_i8(0x00, 0x00, 0x00, 0x00); // Additional flags | dd
  write_2_i8(0x40, 0x00); // Size of header (64 bytes)
  write_2_i8(0x38, 0x00); // Size of Program header  | dw
  write_2_i8(0x01, 0x00); // Num program header entries | dw
  write_2_i8(0x00, 0x00); // Size of section header entry
  write_2_i8(0x00, 0x00); // Sec_head num
  write_2_i8(0x00, 0x00); // Sec index
}

void write_elf_p_header() {
  write_i32_le(1);                 // p_type (1=PT_LOAD = a loadable segment) | dd
  write_i32_le(5);                 // Program header flags. 5 = Not writable. (bits 0, 1, and 2 = executable, writable, readable) | dd
  write_i32_le(0);                 // p_offset
  write_i32_le(0);                 // p_offset Cont..
  write_i32_le(0x40000000);        // p_vaddr The Virtual Address to place the segment at | dq
  write_i32_le(0x00000000);        // Cont.. | dq cont..
  write_i32_le(0x40000000);        // p_paddr The "phyiscal address" set to same as virtual address | dq
  write_i32_le(0x00000000);        // Cont.. | dq cont..
  write_i32_le(0x78 + code_alloc); // p_filesz File load virtual address : The size of the segment in the file. It ends at the string table | dq (problem code?)
  write_i32_le(0x0000000);        // Cont.. | dq cont..
  write_i32_le(0x78 + code_alloc); // p_memsz  String table : The size of the segment in memory | dq (problem code?)
  write_i32_le(0x00000000);        // Cont.. | dq cont..
  write_i32_le(0x200000);                  // p_align  | dq
  write_i32_le(0x00000000);        // Cont.. | dq cont..
}

#endif

void generate_exe() {
  int i = 0;

  write_elf_e_header();
  write_elf_p_header();

  while (i < code_alloc) {
    write_i8(code[i]);
    i += 1;
  }
}
