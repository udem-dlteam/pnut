#ifdef target_i386_linux
  #define WORD_SIZE 4
#endif

#ifdef target_x86_64_linux
  #define WORD_SIZE 8
#endif

#ifdef target_x86_64_mac
  #define WORD_SIZE 8
#endif

// x86 codegen
#include "exe.c"

#ifdef target_i386_linux
  #include "elf.c"
#endif

#ifdef target_x86_64_linux
  #include "elf.c"
#endif

#ifdef target_x86_64_mac
  #include "mach-o.c"
#endif

// Registers common to i386 and x86-64 (E and R prefixes are omitted).

const int AX = 0;
const int CX = 1;
const int DX = 2;
const int BX = 3;
const int SP = 4;
const int BP = 5;
const int SI = 6;
const int DI = 7;
const int R8 = 8;
const int R9 = 9;
const int R10 = 10;
const int R11 = 11;
const int R12 = 12;
const int R13 = 13;
const int R14 = 14;
const int R15 = 15;

// On old versions of gcc (such as on debian woody), setting a const variable
// to another const variable produces an error. This is a workaround.
const int reg_X = 0; // AX: temporary register
const int reg_Y = 1; // CX: temporary register
const int reg_Z = 2; // DX: temporary register
const int reg_SP = 4; // SP: stack pointer
const int reg_glo = 3; // BX: global variables table

#if WORD_SIZE == 8
void rex_prefix(int reg1, int reg2) {
  // REX prefix encodes:
  //  0x40: fixed value
  //  0x08: REX.W: a 64-bit operand size is used.
  //  0x04: REX.R: 1-bit extension for first register encoded for mod_rm
  //  0x02: REX.X: 1-bit extension for SIB index encoded for mod_rm (Not used)
  //  0x01: REX.B: 1-bit extension for second register encoded for mod_rm
  emit_i8(0x48 + 0x04 * (reg1 >= R8) + 0x01 * (reg2 >= R8));
}
#else
#define rex_prefix(reg1, reg2) ((void)0)
#endif

// ModR/M byte
//
// It is used to encode the operand(s) to an instruction.
// The format is the following:
// Bit    7   6   5   4   3   2   1   0
//        -----   ---------   ---------
// Usage   Mod       Reg         R/M
//
// Operations that use 1 operand generally use the R/M field to specify it.
// In that case, the Reg field may be repurposed as an "opcode extension" to
// allow multiple instructions to share the same opcode. This is generally
// indicated as /digit in the opcode table.
//
// The mod field encodes the addressing mode for the register/memory ("r/m") operand.
// When the mod field is 11, the r/m field is used to specify a register operand.
// Otherwise, 00, 01 and 10 specify different addressing modes:
//  00: no displacement     [reg]
//  01: 8-bit signed displacement  [reg + disp8]
//  10: 32-bit signed displacement [reg + disp32]
//
// When mod specifies an addressing mode, the ModR/M byte may be followed by
// a SIB byte (Scale Index Base) and/or a displacement.
//
// The SIB byte encodes addressing modes of the form [base + index*scale + disp].
// The format is the following:
// Bit    7   6   5   4   3   2   1   0
//        -----   ---------   ---------
// Usage  Scale     Index        Base
//
// Where scale is 1, 2, 4, or 8 (00, 01, 10, 11), index and base are register,
// potentially extended with REX prefix.
//
// Note that an index of ESP is forbidden, as it is used to encode no base
// register, i.e. an ESP-relative address ([ESP+disp0/8/32]).
// Also, if (MOD, BASE)=(00, 101), specifies no base register and a 32-bit
// displacement, analogous to the (MOD, R/M)=(00, 101) case. This is because in
// 64-bit mode, (MOD, R/M)=(00, 101) is used to encode RIP-relative addressing.
//
// See https://web.archive.org/web/20250207155122/https://en.wikipedia.org/wiki/ModR/M
void mod_rm(const int reg1, const int reg2) {
  // Both operands are registers, so mod = 11 (0xc0) and no displacement.
  emit_i8(0xc0 + ((reg1 & 7) << 3) + (reg2 & 7));
}

void mod_rm_reg_mem(int reg, int base, int offset) {
  bool is_8_bit_offset = -128 <= offset && offset <= 127;

  // One of the operand is an address, so mod is either 01 (0x40) or 10 (0x80).
  emit_i8((is_8_bit_offset ? 0x40 : 0x80) + (reg & 7) * 8 + (base & 7));

  // SIB byte. See 32/64-bit addressing mode
  if (base == SP || base == R12) emit_i8(0x24);

  if (is_8_bit_offset) {
    emit_i8(offset);
  } else {
    emit_i32_le(offset);
  }
}

// ModR/M byte with /digit opcode extension => The reg1 field is repurposed as an opcode extension.
#define mod_rm_slash_digit(digit, reg1) mod_rm(digit, reg1)

// For instructions with 2 register operands
void op_reg_reg(const int opcode, const int dst, const int src, const int reg_width) {
  // 16-bit operand size override prefix
  // See section on Legacy Prefixes: https://web.archive.org/web/20250210181519/https://wiki.osdev.org/X86-64_Instruction_Encoding#ModR/M
  if (reg_width == 2) emit_i8(0x66);
  if (reg_width == 8) rex_prefix(src, dst);
  emit_i8(opcode);
  mod_rm(src, dst);
}

// For instructions with 1 register operand and /digit opcode extension
void op_reg_slash_digit(const int opcode, const int digit, const int reg) {
  rex_prefix(0, reg);
  emit_i8(opcode);
  mod_rm_slash_digit(digit, reg);
}

#ifdef SKIP

// deprecated... kept here in case it might be useful in the future

void inc_reg(const int dst) { rex_prefix(dst, 0); emit_2_i8(0xff, 0xc0 + (dst & 7)); }
void dec_reg(const int dst) { rex_prefix(dst, 0); emit_2_i8(0xff, 0xc8 + (dst & 7)); }
void xchg_reg_reg(const int dst, const int src) { op_reg_reg(0x87, dst, src); }
void not_reg(const int dst) { rex_prefix(dst, 0); emit_2_i8(0xf7, 0xd0 + (dst & 7)); }
void neg_reg(const int dst) { rex_prefix(dst, 0); emit_2_i8(0xf7, 0xd8 + (dst & 7)); }
void shr_reg_cl(const int dst) { rex_prefix(dst, 0); emit_2_i8(0xd3, 0xe8 + (dst & 7)); }
void jump_cond_short(const int cond, const int n) { emit_2_i8(0x70 + cond, n); }

void test_reg_reg(const int dst, const int src) {

  // TEST dst_reg, src_reg ;; set Z condition flag based on result of dst_reg&src_reg
  // See: https://web.archive.org/web/20231004142335/https://www.felixcloutier.com/x86/test

  op_reg_reg(0x85, dst, src, WORD_SIZE);
}

#endif

void add_reg_reg(const int dst, const int src) {

  // ADD dst_reg, src_reg ;; dst_reg = dst_reg + src_reg
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/add

  op_reg_reg(0x01, dst, src, WORD_SIZE);
}

void or_reg_reg (const int dst, const int src) {

  // OR dst_reg, src_reg ;; dst_reg = dst_reg | src_reg
  // See: https://web.archive.org/web/20231002205127/https://www.felixcloutier.com/x86/or

  op_reg_reg(0x09, dst, src, WORD_SIZE);
}

void and_reg_reg(const int dst, const int src) {

  // AND dst_reg, src_reg ;; dst_reg = dst_reg & src_reg
  // See: https://web.archive.org/web/20240228122102/https://www.felixcloutier.com/x86/and

  op_reg_reg(0x21, dst, src, WORD_SIZE);
}

void sub_reg_reg(const int dst, const int src) {

  // SUB dst_reg, src_reg ;; dst_reg = dst_reg - src_reg
  // See: https://web.archive.org/web/20240118202232/https://www.felixcloutier.com/x86/sub

  op_reg_reg(0x29, dst, src, WORD_SIZE);
}

void xor_reg_reg(const int dst, const int src) {

  // XOR dst_reg, src_reg ;; dst_reg = dst_reg ^ src_reg
  // See: https://web.archive.org/web/20240323052259/https://www.felixcloutier.com/x86/xor

#if WORD_SIZE == 8
  if (src == dst) {
    op_reg_reg(0x31, dst, src, 4); // Use 32-bit opcode for XOR dst_reg, dst_reg because it's shorter
  } else {
    op_reg_reg(0x31, dst, src, WORD_SIZE);
  }
#else
  op_reg_reg(0x31, dst, src, WORD_SIZE);
#endif
}

void cmp_reg_reg(const int dst, const int src) {

  // CMP dst_reg, src_reg  ;; Set condition flags according to dst_reg-src_reg
  // See: https://web.archive.org/web/20240407051947/https://www.felixcloutier.com/x86/cmp
  // Note: For byte comparison, opcode is 0x38, for word/dword/qword comparison, opcode is 0x39

  op_reg_reg(0x39, dst, src, WORD_SIZE);
}

void mov_reg_reg(const int dst, const int src) {

  // MOV dst_reg, src_reg  ;; dst_reg = src_reg
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/mov

  op_reg_reg(0x89, dst, src, WORD_SIZE);
}

void mov_reg_imm(const int dst, const int imm) {

  // MOV dst_reg, imm  ;; Move 32 bit immediate value to register
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/mov

  rex_prefix(0, dst);
  emit_i8(0xb8 + (dst & 7));
#if WORD_SIZE == 4
  emit_i32_le(imm);
#elif WORD_SIZE == 8
  emit_i64_le(imm);
#else
  #error "mov_reg_imm: unknown word size"
#endif
}

#ifdef SUPPORT_64_BIT_LITERALS
void mov_reg_large_imm(const int dst, const int large_imm) {

  // MOV dst_reg, large_imm  ;; Move 32 bit or 64 bit immediate value to register
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/mov

  rex_prefix(0, dst);
  emit_i8(0xb8 + (dst & 7));

#if WORD_SIZE == 4
  emit_i32_le_large_imm(large_imm);
#elif WORD_SIZE == 8
  emit_i64_le_large_imm(large_imm);
#else
  #error "mov_reg_large_imm: unknown word size"
#endif
}
#endif

void add_reg_imm(const int dst, const int imm) {

  // ADD dst_reg, imm  ;; Add 32 bit immediate value to register
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/add

  rex_prefix(0, dst);
  emit_i8(0x81);
  mod_rm(0, dst);
  emit_i32_le(imm);
}

void add_reg_lbl(const int dst, const int lbl) {

  // ADD dst_reg, rel addr  ;; Add 32 bit displacement between next instruction and label to register
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/add

  rex_prefix(0, dst);
  emit_i8(0x81);
  mod_rm(0, dst);
  use_label(lbl); // 32 bit placeholder for distance
}

void mov_memory(const int op, const int reg, const int base, const int offset, const int reg_width) {

  // Move word between register and memory
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/mov

  // 16-bit operand size override prefix
  // See section on Legacy Prefixes: https://web.archive.org/web/20250210181519/https://wiki.osdev.org/X86-64_Instruction_Encoding#ModR/M
  if (reg_width == 2) emit_i8(0x66);
  if (reg_width == 8) rex_prefix(reg, base);
  emit_i8(op);
  mod_rm_reg_mem(reg, base, offset);
}

void mov_memory_extend(const int op, const int reg, const int base, const int offset, const bool include_0f) {

  // Move word between register and memory with sign extension
  // See: https://web.archive.org/web/20250121105942/https://www.felixcloutier.com/x86/movsx:movsxd
  // And  https://web.archive.org/web/20250109202608/https://www.felixcloutier.com/x86/movzx

  // From webpage:
  //  > The use of MOVSXD without REX.W in 64-bit mode is discouraged. Regular
  //  > MOV should be used instead of using MOVSXD without REX.W.
  rex_prefix(reg, base);
  if (include_0f) emit_i8(0x0f); // Most sign/zero extend instructions have a 0x0f prefix
  emit_i8(op);
  mod_rm_reg_mem(reg, base, offset);
}

void mov_mem8_reg(const int base, const int offset, const int src) {

  // MOVB [base_reg + offset], src_reg  ;; Move byte from register to memory
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/mov

#ifdef SAFE_MODE
  // The ModR/M byte cannot encode lower registers that are not AL, CL, DL, or BL
  if (src != AX && src != CX && src != DX && src != BX) {
    fatal_error("mov_mem8_reg: src must one of AX, CX, DX, BX");
  }
#endif

  mov_memory(0x88, src, base, offset, 1);
}

void mov_mem16_reg(const int base, const int offset, const int src) {

  // MOVB [base_reg + offset], src_reg  ;; Move word (2 bytes) from register to memory
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/mov

  mov_memory(0x89, src, base, offset, 2);
}

void mov_mem32_reg(const int base, const int offset, const int src) {

  // MOVB [base_reg + offset], src_reg  ;; Move dword (4 bytes) from register to memory
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/mov

  mov_memory(0x89, src, base, offset, 4);
}

void mov_mem64_reg(const int base, const int offset, const int src) {

  // MOVB [base_reg + offset], src_reg  ;; Move qword (8 bytes) from register to memory
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/mov

  mov_memory(0x89, src, base, offset, 8);
}

void mov_reg_mem8(const int dst, const int base, const int offset) {

  // MOVB dst_reg, [base_reg + offset]  ;; Move byte from memory to register, zero-extended
  // See: https://web.archive.org/web/20250109202608/https://www.felixcloutier.com/x86/movzx

  mov_memory_extend(0xb6, dst, base, offset, true);
}

void mov_reg_mem16(const int dst, const int base, const int offset) {

  // MOVB dst_reg, [base_reg + offset]  ;; Move word (2 bytes) from memory to register, zero-extended
  // See: https://web.archive.org/web/20250109202608/https://www.felixcloutier.com/x86/movzx

  mov_memory_extend(0xb7, dst, base, offset, true);
}

void mov_reg_mem32(const int dst, const int base, const int offset) {

  // MOV dst_reg, [base_reg + offset]  ;; Move dword (4 bytes) from memory to register, zero-extended
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/mov

  // Operations writing to the lower 32 bits of a register zero-extend the
  // result, so there's no movzx instruction for 32-bit operands.
  mov_memory(0x8b, dst, base, offset, 4);
}

void mov_reg_mem8_sign_ext(const int dst, const int base, const int offset) {

  // MOVB dst_reg, [base_reg + offset]  ;; Move byte from memory to register, sign-extended
  // See: https://web.archive.org/web/20250121105942/https://www.felixcloutier.com/x86/movsx:movsxd

  mov_memory_extend(0xbe, dst, base, offset, true);
}

void mov_reg_mem16_sign_ext(const int dst, const int base, const int offset) {

  // MOVB dst_reg, [base_reg + offset]  ;; Move word (2 bytes) from memory to register, sign-extended
  // See: https://web.archive.org/web/20250121105942/https://www.felixcloutier.com/x86/movsx:movsxd

  mov_memory_extend(0xbf, dst, base, offset, true);
}

void mov_reg_mem32_sign_ext(const int dst, const int base, const int offset) {

  // MOV dst_reg, [base_reg + offset]  ;; Move dword (4 bytes) from memory to register, sign-extended
  // See: https://web.archive.org/web/20250121105942/https://www.felixcloutier.com/x86/movsx:movsxd

  mov_memory_extend(0x63, dst, base, offset, false);
}

void mov_reg_mem64(const int dst, const int base, const int offset) {

  // MOV dst_reg, [base_reg + offset]  ;; Move qword (8 bytes) from memory to register
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/mov

  mov_memory(0x8b, dst, base, offset, 8);
}

void imul_reg_reg(const int dst, const int src) {

  // IMUL dst_reg, src_reg ;; dst_reg = dst_reg * src_reg
  // See: https://web.archive.org/web/20240228122220/https://www.felixcloutier.com/x86/imul

  rex_prefix(dst, src);
  emit_2_i8(0x0f, 0xaf);
  mod_rm(dst, src);
}

void mul_reg_reg(const int dst, const int src) {

  // For our purposes, this is the same as imul_reg_reg.
  // https://web.archive.org/web/20240914145321/https://stackoverflow.com/questions/42587607/why-is-imul-used-for-multiplying-unsigned-numbers

  imul_reg_reg(dst, src);
}

void idiv_reg(const int src) {

  // IDIV src_reg ;; AX = DX:AX / src_reg ; DX = DX:AX % src_reg
  // See: https://web.archive.org/web/20240407195950/https://www.felixcloutier.com/x86/idiv

  op_reg_slash_digit(0xf7, 7, src);
}

void div_reg(const int src) {

  // DIV src_reg ;; AX = DX:AX / src_reg ; DX = DX:AX % src_reg
  // See: https://web.archive.org/web/20250202075400/https://www.felixcloutier.com/x86/div

  op_reg_slash_digit(0xf7, 6, src);
}

void cdq_cqo() {

  // CDQ ;; Convert Doubleword to Quadword (EDX:EAX = sign-extend EAX)
  // x86-64: CQO ;; Convert Quadword to Octoword (RDX:RAX = sign-extend RAX)
  // See: https://web.archive.org/web/20240118201956/https://www.felixcloutier.com/x86/cwd:cdq:cqo

  rex_prefix(0, 0);
  emit_i8(0x99);
}

void idiv_reg_reg(const int dst, const int src) {

  // Computes dst_reg = dst_reg / src_reg
  // This is not an actual instruction on x86. The operation
  // is emulated with a sequence of instructions that will clobber the
  // registers AX and DX.

  mov_reg_reg(AX, dst);
  cdq_cqo(); // sign extend AX to DX:AX
  idiv_reg(src);
  mov_reg_reg(dst, AX);
}

void irem_reg_reg(const int dst, const int src) {

  // Computes dst_reg = dst_reg % src_reg
  // This is not an actual instruction on x86. The operation
  // is emulated with a sequence of instructions that will clobber the
  // registers AX and DX.

  mov_reg_reg(AX, dst);
  cdq_cqo(); // sign extend AX to DX:AX
  idiv_reg(src);
  mov_reg_reg(dst, DX);
}

void div_reg_reg(const int dst, const int src) {

  // Computes dst_reg = dst_reg / src_reg
  // This is not an actual instruction on x86. The operation
  // is emulated with a sequence of instructions that will clobber the
  // registers AX and DX.

#ifdef SAFE_MODE
  if (src == AX || src == DX) fatal_error("div_reg_reg: src cannot be AX");
#endif

  mov_reg_reg(AX, dst);
  mov_reg_imm(DX, 0); // Clear DX
  div_reg(src);
  mov_reg_reg(dst, AX);
}

void rem_reg_reg(const int dst, const int src) {

  // Computes dst_reg = dst_reg % src_reg
  // This is not an actual instruction on x86. The operation
  // is emulated with a sequence of instructions that will clobber the
  // registers AX and DX.

  mov_reg_reg(AX, dst);
  mov_reg_imm(DX, 0); // Clear DX
  div_reg(src);
  mov_reg_reg(dst, DX);
}

void s_l_reg_cl(const int dst) {

  // SHL dst_reg, cl ;; dst_reg = dst_reg << cl (arithmetic or logical shift, they are the same)
  // See: https://web.archive.org/web/20240405194323/https://www.felixcloutier.com/x86/sal:sar:shl:shr

  op_reg_slash_digit(0xd3, 4, dst);
}

void s_l_reg_reg(const int dst, const int src) {

  // Computes dst_reg = dst_reg << src_reg (arithmetic or logical shift, they are the same)
  // This is not an actual instruction on x86. The operation
  // is emulated with a sequence of instructions that clobbers the
  // register CX, and does not work if dst = CX.

#ifdef SAFE_MODE
  if (dst == CX) fatal_error("s_l_reg_reg: dst cannot be CX");
#endif

  mov_reg_reg(CX, src);
  s_l_reg_cl(dst);
}

void sar_reg_cl(const int dst) {

  // SAR dst_reg, cl ;; dst_reg = dst_reg >> cl (arithmetic shift)
  // See: https://web.archive.org/web/20240405194323/https://www.felixcloutier.com/x86/sal:sar:shl:shr

  op_reg_slash_digit(0xd3, 7, dst);
}

void sar_reg_reg(const int dst, const int src) {

  // Computes dst_reg = dst_reg >> src_reg (arithmetic shift)
  // This is not an actual instruction on x86. The operation
  // is emulated with a sequence of instructions that clobbers the
  // register CX, and does not work if dst = CX.

  mov_reg_reg(CX, src);
  sar_reg_cl(dst);
}

void shr_reg_cl(const int dst) {

  // SHR dst_reg, cl ;; dst_reg = dst_reg >> cl (logical shift)
  // See: https://web.archive.org/web/20240405194323/https://www.felixcloutier.com/x86/sal:sar:shl:shr

  op_reg_slash_digit(0xd3, 5, dst);
}

void shr_reg_reg(const int dst, const int src) {

  // Computes dst_reg = dst_reg >> src_reg (logical shift)
  // This is not an actual instruction on x86. The operation
  // is emulated with a sequence of instructions that clobbers the
  // register CX, and does not work if dst = CX.

  mov_reg_reg(CX, src);
  shr_reg_cl(dst);
}

void push_reg(const int src) {

  // PUSH src_reg  ;; Push word from register to stack
  // See: https://web.archive.org/web/20240407051929/https://www.felixcloutier.com/x86/push

  emit_i8(0x50 + src);
}

void pop_reg (const int dst) {

  // POP dst_reg  ;; Pop word from stack to register
  // See: https://web.archive.org/web/20240204122208/https://www.felixcloutier.com/x86/pop

  emit_i8(0x58 + dst);
}

void jump(const int lbl) {

  // JMP rel32  ;; Jump to 32 bit displacement relative to next instruction
  // See: https://web.archive.org/web/20240407051904/https://www.felixcloutier.com/x86/jmp

  emit_i8(0xe9);
  use_label(lbl);
}

void jump_rel(const int offset) {

  // JMP rel32  ;; Jump to 32 bit displacement relative to next instruction
  // See: https://web.archive.org/web/20240407051904/https://www.felixcloutier.com/x86/jmp

  emit_i8(0xe9);
  emit_i32_le(offset);
}

void call(const int lbl) {

  // CALL rel32  ;; Call to 32 bit displacement relative to next instruction
  // See: https://web.archive.org/web/20240323052931/https://www.felixcloutier.com/x86/call

  emit_i8(0xe8);
  use_label(lbl);
}

void call_reg(const int reg) {

  // CALL reg  ;; Indirect call to address in register
  // See: https://web.archive.org/web/20240323052931/https://www.felixcloutier.com/x86/call

  emit_i8(0xff);
  mod_rm(2, reg);
}

void ret() {

  // RET  ;; Return to calling procedure
  // See: https://web.archive.org/web/20240302232015/https://www.felixcloutier.com/x86/ret

  emit_i8(0xc3);
}

void debug_interrupt() {

  // INT 3  ;; Debug interrupt
  // See: https://web.archive.org/web/20250118000553/https://www.felixcloutier.com/x86/intn:into:int3:int1

  emit_i8(0xcc);
}

// Conditions for use by jump_cond:

const int EQ   = 0x4; // x == y
const int NE   = 0x5; // x != y
const int LT   = 0xc; // x < y
const int LT_U = 0x2; // x < y  (Jump near if not above or equal (CF=1))
const int GE   = 0xd; // x >= y
const int GE_U = 0x3; // x >= y (Jump near if above or equal (CF=0))
const int LE   = 0xe; // x <= y
const int LE_U = 0x6; // x <= y (Jump near if below or equal (CF=1 or ZF=1))
const int GT   = 0xf; // x > y
const int GT_U = 0x7; // x > y  (Jump near if not below or equal (CF=0 and ZF=0))

void jump_cond(const int cond, const int lbl) {

  // JE rel32, JNE rel32, JL rel32, JLE rel32, JG rel32, JGE rel32, ...
  // Jump conditionally to 32 bit displacement relative to next instruction
  // See: https://web.archive.org/web/20231007122614/https://www.felixcloutier.com/x86/jcc

  emit_2_i8(0x0f, 0x80 + cond);
  use_label(lbl);
}

void jump_cond_reg_reg(const int cond, const int lbl, const int reg1, const int reg2) {
  cmp_reg_reg(reg1, reg2);
  jump_cond(cond, lbl);
}

void int_i8(const int n) {

  // INT imm8 ;; Software interrupt with vector specified by immediate byte
  // See: https://web.archive.org/web/20240407051903/https://www.felixcloutier.com/x86/intn:into:int3:int1

  emit_2_i8(0xcd, n);
}

void setup_proc_args(const int global_vars_size) {
  // See page 54 of Intel386 System V ABI document:
  // https://web.archive.org/web/20240107061436/https://www.sco.com/developers/devspecs/abi386-4.pdf
  // See page 29 of AMD64 System V ABI document:
  // https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf
  //
  // On x86-32, argc is at [esp+0] and the content of argv directly follows at program start.
  // At this point, we've initialized the globals table so the stack looks like this:
  // [esp + 0]: global table start (global_vars_size bytes long)
  // ...
  // [esp + global_vars_size]     : argc
  // [esp + global_vars_size + 4] : argv[0]
  // [esp + global_vars_size + 8] : argv[1]
  // ...
  // The main function expects argv to be a char**, so it's missing an indirection, which is added here.
  // The stack will then look like this:
  // [esp + 0] : argc
  // [esp + 4] : argv
  // [esp + 8] : global table start (global_vars_size bytes long)
  // ...
  // For x86-64, it works similarly with [rsp + 0] for argc and [rsp + 8] for argv.
  //
  // Note(13/02/2025): Global variables are now allocated in a separate memory region so global_vars_size is 0.

  mov_reg_reg(reg_X, SP);
  add_reg_imm(reg_X, global_vars_size + WORD_SIZE); // compute address of argv
  push_reg(reg_X); // push argv address

  mov_reg_mem(reg_Y, reg_X, -WORD_SIZE); // load argc
  push_reg(reg_Y); // push argc
}

void mov_reg_lbl(int reg, int lbl) {
  // Since we can't do rip-relative addressing in 32 bit mode, we need to push
  // the address to the stack and then some arithmetic to get the address in a
  // register.

  int lbl_for_pc = alloc_label("lbl_for_pc");

  call(lbl_for_pc);        // call lbl
  def_label(lbl_for_pc);   // end label
                           // <--- The stack now has the address of the next instruction
  pop_reg(reg);            // pop reg_X (1 byte)
  add_reg_lbl(reg, lbl);   // load address of label to reg_X (6 or 7 bytes if 32 or 64 bit)
  add_reg_imm(reg, TERNARY(WORD_SIZE == 8, 8, 7)); // adjust for the pop and add instructions
}

// For 32 bit linux.
#ifdef target_i386_linux

// Regular system calls for 32 bit linux.
// The system call number is passed in the rax register.
// Other arguments are passed in ebx, ecx and edx.
// The return value is in rax.
// If the parameter registers are ebx, ecx or edx, the function assume they may
// be clobberred in the order of the mov instructions.
// i.e. syscall_3(SYS_READ, ..., ebx, ...) is not valid because ebx is clobberred by the first mov instructions.
// For syscalls that use less than 3 parameters, the extra register params are set to -1.
void syscall_3(const int syscall_code, const int bx_reg, const int cx_reg, const int dx_reg) {
  push_reg(BX);                  // save address of global variables table
  if (bx_reg >= 0) mov_reg_reg(BX, bx_reg);
  if (cx_reg >= 0) mov_reg_reg(CX, cx_reg);
  if (dx_reg >= 0) mov_reg_reg(DX, dx_reg);
  mov_reg_imm(AX, syscall_code); // AX = syscall_code
  int_i8(0x80);                  // syscall
  pop_reg(BX);                   // restore address of global variables table
}

#ifdef USE_BRK_SYSCALL
// Use brk/sbrk syscalls instead of mmap for memory allocation
// This is needed for environments like builder-hex0 that don't support mmap
void os_allocate_memory(const int size) {
  mov_reg_imm(reg_X, 0);
  syscall_3(45, reg_X, -1, -1);   // brk(0) to get current break address, SYS_BRK = 45

  push_reg(reg_X);                // save current break address
  add_reg_imm(reg_X, size);       // add current break + size

  syscall_3(45, reg_X, -1, -1);   // extend program break, SYS_BRK = 45
  pop_reg(reg_X);                 // restore original break address
}
#else
// Use mmap syscall for memory allocation (original implementation)
void os_allocate_memory(const int size) {
  push_reg(BX);           // save address of global variables table
  mov_reg_imm(AX, 192);   // mov  eax, 192 == SYS_MMAP2
  mov_reg_imm(BX, 0);     // mov  ebx, 0 | NULL
  mov_reg_imm(CX, size);  // mov  ecx, size | size
  mov_reg_imm(DX, 0x3);   // mov  edx, 0x3 | PROT_READ (0x1) | PROT_WRITE (0x2)
  mov_reg_imm(SI, 0x22);  // mov  esi, 0x21 | MAP_ANONYMOUS (0x20) | MAP_PRIVATE (0x2)
  mov_reg_imm(DI, -1);    // mov  edi, -1 (file descriptor)
  mov_reg_imm(BP, 0);     // mov  ebp, 0 (offset)
  int_i8(0x80);           // int  0x80     # system call
  pop_reg(BX);            // restore address of global variables table
}
#endif

void os_exit() {
  syscall_3(1, reg_X, -1, -1); // SYS_EXIT = 1
}

void os_read() {
  syscall_3(3, reg_X, reg_Y, reg_Z); // SYS_READ = 3
}

void os_write() {
  syscall_3(4, reg_X, reg_Y, reg_Z); // SYS_WRITE = 4
}

void os_open() {
  syscall_3(5, reg_X, reg_Y, reg_Z); // SYS_OPEN = 5
}

void os_close() {
  syscall_3(6, reg_X, -1, -1); // SYS_CLOSE = 6
}

void os_seek() {
  syscall_3(19, reg_X, reg_Y, reg_Z); // SYS_LSEEK = 19
}

void os_unlink() {
  syscall_3(10, reg_X, -1, -1); // SYS_UNLINK = 10
}

void os_mkdir() {
  syscall_3(39, reg_X, reg_Y, -1); // SYS_MKDIR = 39
}

void os_chmod() {
  syscall_3(15, reg_X, reg_Y, -1); // SYS_CHMOD = 15
}

void os_access() {
  syscall_3(21, reg_X, reg_Y, -1); // SYS_ACCESS = 21
}

#endif

// Both x86_64_linux and x86_64_mac use the System V ABI, the difference is in the system calls.
#ifdef target_x86_64_linux
  #define SYSTEM_V_ABI
  #define SYS_READ 0
  #define SYS_WRITE 1
  #define SYS_OPEN 2
  #define SYS_CLOSE 3
  #define SYS_LSEEK 8
  #define SYS_UNLINK 87
  #define SYS_MKDIR 83
  #define SYS_CHMOD 90
  #define SYS_ACCESS 21
  #define SYS_STAT 4
  #define SYS_MMAP_MAP_TYPE 0x22
  #define SYS_MMAP 9
  #define SYS_BRK 12
  #define SYS_EXIT 60
#endif

#ifdef target_x86_64_mac
  // Refer to following page for macOS system call numbers:
  // https://web.archive.org/web/20211102014723/http://www.opensource.apple.com/source/xnu/xnu-1504.3.12/bsd/kern/syscalls.master
  // Because it's a 64 bit system, 0x2000000 is added to the system call number.
  #define SYSTEM_V_ABI
  #define SYS_READ 0x2000003
  #define SYS_WRITE 0x2000004
  #define SYS_OPEN 0x2000005
  #define SYS_CLOSE 0x2000006
  #define SYS_LSEEK 0x20000c7
  #define SYS_UNLINK 0x200000a
  #define SYS_MKDIR 0x2000088
  #define SYS_CHMOD 0x200000f
  #define SYS_ACCESS 0x2000021
  #define SYS_MMAP_MAP_TYPE 0x1020
  #define SYS_MMAP 0x20000C5
  #define SYS_EXIT 0x2000001
#ifdef USE_BRK_SYSCALL
  #error "USE_BRK_SYSCALL is not supported on macOS"
#endif
#endif

// For 64 bit System V ABI.
#ifdef SYSTEM_V_ABI

void syscall_() {

  // SYSCALL ;; Fast System Call
  // See: https://web.archive.org/web/20240620153804/https://www.felixcloutier.com/x86/syscall

  emit_2_i8(0x0F, 0x05);
}

// Regular system calls for 64 bit linux and macOS.
// The system call number is passed in the rax register.
// Other arguments are passed in rdi, rsi and rdx.
// The return value is in rax.
// The di_reg, si_reg, dx_reg parameters
// If the parameter registers are rdi, rsi or rdx, the function assume they may
// be clobberred in the order of the mov instructions.
// i.e. syscall_3(SYS_READ, ..., rdi, ...) is not valid because rdi is clobberred by the first mov instructions.
// For syscalls that use less than 3 parameters, the extra register params are set to -1.
void syscall_3(const int syscall_code, const int di_reg, const int si_reg, const int dx_reg) {
  if (di_reg >= 0) mov_reg_reg(DI, di_reg);
  if (si_reg >= 0) mov_reg_reg(SI, si_reg);
  if (dx_reg >= 0) mov_reg_reg(DX, dx_reg);
  mov_reg_imm(AX, syscall_code); // AX = syscall_code
  syscall_();                    // syscall
}

#if defined(USE_BRK_SYSCALL) && defined(SYS_BRK)
// Use brk/sbrk syscalls instead of mmap for memory allocation
// This is needed for environments like builder-hex0 that don't support mmap
void os_allocate_memory(const int size) {
  mov_reg_imm(reg_X, 0);
  syscall_3(SYS_BRK, reg_X, -1, -1);  // brk(0) to get current break address

  push_reg(reg_X);                    // save current break address
  add_reg_imm(reg_X, size);           // add current break + size

  syscall_3(SYS_BRK, reg_X, -1, -1);  // Extend program break
  pop_reg(reg_X);                     // restore original break address
}
#else
// Use mmap syscall for memory allocation (original implementation)
void os_allocate_memory(const int size) {
  mov_reg_imm(DI, 0);                  // mov rdi, 0 | NULL
  mov_reg_imm(SI, size);               // mov rsi, size | size
  mov_reg_imm(DX, 0x3);                // mov rdx, 0x3 | PROT_READ (0x1) | PROT_WRITE (0x2)
  mov_reg_imm(R10, SYS_MMAP_MAP_TYPE); // mov r10, 0x21 | MAP_ANONYMOUS (0x20) | MAP_PRIVATE (0x2)
  mov_reg_imm(R8, -1);                 // mov r8, -1 (file descriptor)
  mov_reg_imm(R9, 0);                  // mov r9, 0 (offset)
  mov_reg_imm(AX, SYS_MMAP);           // mov rax, SYS_MMAP
  syscall_();                          // syscall
}
#endif

void os_exit() {
  syscall_3(SYS_EXIT, reg_X, -1, -1);
}

void os_read() {
  syscall_3(SYS_READ, reg_X, reg_Y, reg_Z);
}

void os_write() {
  syscall_3(SYS_WRITE, reg_X, reg_Y, reg_Z);
}

void os_open() {
  syscall_3(SYS_OPEN, reg_X, reg_Y, reg_Z);
  // MacOS (BSD) signals errors by setting the carry flag, while Linux uses a negative return value.
  // For now, we'll assume macOS and Linux have the same behavior and return a negative value in rax.
}

void os_close() {
  syscall_3(SYS_CLOSE, reg_X, -1, -1);
}

void os_seek() {
  syscall_3(SYS_LSEEK, reg_X, reg_Y, reg_Z);
}

void os_unlink() {
  syscall_3(SYS_UNLINK, reg_X, -1, -1);
}

void os_mkdir() {
  syscall_3(SYS_MKDIR, reg_X, reg_Y, -1);
}

void os_chmod() {
  syscall_3(SYS_CHMOD, reg_X, reg_Y, -1);
}

void os_access() {
  syscall_3(SYS_ACCESS, reg_X, reg_Y, -1);
}

#endif
