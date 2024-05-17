// arm codegen

// FULLY WORK IN PROGRESS JUST SKELETON CODE FOR NOW

#include "exe.c"
#include "elf.c"

#ifdef arm

const int word_size = 8; // 64 bit

//Registers common to aarch64 architecture (31 general-purpose registers)
const int r0 = 0;
const int r1 = 1;
const int r2 = 2;
const int r3 = 3;
const int r4 = 4;
const int r5 = 5;
const int BP = 29; // Base pointer
const int SP = 31; // Stack pointer (the value 31 represents
// 31 Represents either the current stack pointer or the zero register, depending on the instruction and the operand position)

const int reg_X = r0;
const int reg_Y = r1;
const int reg_SP = SP;
const int reg_glo = r5;

// A64 instruction set https://yurichev.com/mirrors/ARMv8-A_Architecture_Reference_Manual_(Issue_A.a).pdf
// https://iitd-plos.github.io/col718/ref/arm-instructionset.pdf

// The Condition Field
const int EQ = 0x00; // Equal
const int NE = 0x01; // Not equal
const int CS = 0x02; // Carry set
const int CC = 0x03; // Carry clear
const int MI = 0x04; // Minus/negative
const int PL = 0x05; // Plus/positive or zero
const int VS = 0x06; // Overflow
const int VC = 0x07; // No overflow
const int HI = 0x08; // Unsigned higher
const int LS = 0x09; // Unsigned lower or same
const int GE = 0x0A; // Signed greater than or equal
const int LT = 0x0B; // Signed less than
const int GT = 0x0C; // Signed greater than
const int LE = 0x0D; // Signed less than or equal
const int AL = 0x0E; // Always (unconditional)

// Operation Code
//const int AND = 0x00; // Logical AND (op1 AND op2)
//const int EOR = 0x01; // Exclusive OR (op1 XOR op2)
//const int SUB = 0x02; // Subtraction (op1 - op2)
//const int RSB = 0x03; // Reverse subtract (op2 - op1)
//const int ADD = 0x04; // Addition (op1 + op2)
//const int ADC = 0x05; // Add with carry (op1 + op2 + carry)
//const int SBC = 0x06; // Subtract with carry (op1 - op2 + carry)
//const int RSC = 0x07; // Reverse subtract with carry (op2 - op1 + carry)
//const int TST = 0x08; // Test bits (op1 AND op2)
//const int TEQ = 0x09; // Test equivalence (op1 XOR op2)
//const int CMP = 0x0A; // Compare (subtract)
//const int CMN = 0x0B; // Compare negative (op1 + op2)
//const int ORR = 0x0C; // Logical (inclusive) OR (op1 OR op2)
//const int MOV = 0x0D; // Move (op2)
//const int BIC = 0x0E; // Bit clear (op1 AND NOT op2)
//const int MVN = 0x0F; // Move NOT (op2)

// Immediate Operand
const int I = 0x01; // Immediate (operand is an immediate value)
const int R = 0x00; // Register (operand is a register)

// Data processing instructions (A64) format
// [ Cond | 00 | OpCode | S | Rn | Rd | Operand 2 ]

// Shift types (shift amount is 5bits unsigned) | (shift register amount is bottom byte Rs)
const int LSL = 0x00; // Logical shift left
const int LSR = 0x01; // Logical shift right
const int ASR = 0x02; // Arithmetic shift right
const int ROR = 0x03; // Rotate right

//arm arithmetic instructions
// ARM arithmetic instructions
void add(int rd, int rn, int rm) { // rd = rn + rm
    emit_i32_le(0x8B000000 | (rm << 16) | (rn << 5) | rd); // ADD
}

void sub(int rd, int rn, int rm) { // rd = rn - rm
    emit_i32_le(0xCB000000 | (rm << 16) | (rn << 5) | rd); // SUB
}

void mul(int rd, int rn, int rm) { // rd = rn * rm
    emit_i32_le(0x9B007C00 | (rm << 16) | (rn << 5) | rd); // MUL
}

void sdiv(int rd, int rn, int rm) { // rd = rn / rm
    emit_i32_le(0x1AC00C00 | (rm << 16) | (rn << 5) | rd); // SDIV
}

//arm memory access instructions
//load and store instructions
void ldr(int rd, int rn, int offset) { // Load Register rd <= [rn + offset]
  emit_i32_le(0xB9400000 | (offset << 10) | (rn << 5) | rd);
}

void str(int rd, int rn, int offset) { // Store Register [rn + offset] <= rd
  emit_i32_le(0xB9000000 | (offset << 10) | (rn << 5) | rd);
}

//arm logical instructions
void and(int rd, int rn, int rm) { // Logical AND (op1 AND op2)
    emit_i32_le(0x8A000000 | (rm << 16) | (rn << 5) | rd);
}

void orr(int rd, int rn, int rm) { // Logical (inclusive) OR (op1 OR op2)
    emit_i32_le(0xAA000000 | (rm << 16) | (rn << 5) | rd);
}

void xor(int rd, int rn, int rm) { // Exclusive OR (op1 XOR op2)
    emit_i32_le(0xCA000000 | (rm << 16) | (rn << 5) | rd);
}

void bic(int rd, int rn, int rm) { // Bit clear (op1 AND NOT op2)
    emit_i32_le(0x8A200000 | (rm << 16) | (rn << 5) | rd);
}

//arm branching instructions
void branch(int label) { // Branch
  emit_i32_le(0x14000000 | ((label >> 2) & 0x03FFFFFF));
}

//arm data movement
void mov_reg_reg(int reg, int arg){ // Move (op2) to (op1)
  emit_i32_le(0xAA0003E0 | (arg << 16) | reg);
}

void mov_reg_imm(int dst, int imm) { // Move immediate (MOVZ) to register
  // MOVZ dst, imm ;; dst = imm (zero-extended)
  emit_i32_le(0xD2800000 | (dst << 5) | (imm & 0xFFFF));
}

void and_reg_reg(int dst, int src) { // Bitwise AND register by register
  // AND dst, dst, src ;; dst = dst & src
  emit_i32_le(0x8A000000 | (dst << 5) | (src << 16) | dst);
}

void or_reg_reg(int dst, int src) { // Bitwise OR register by register
  // ORR dst, dst, src ;; dst = dst | src
  emit_i32_le(0xAA000000 | (dst << 5) | (src << 16) | dst);
}

void xor_reg_reg(int dst, int src) { // Bitwise XOR register by register
  // EOR dst, dst, src ;; dst = dst ^ src
  emit_i32_le(0xCA000000 | (dst << 5) | (src << 16) | dst);
}

void sub_reg_reg(int dst, int src) { // Subtract register by register
  // SUB dst, dst, src ;; dst = dst - src
  emit_i32_le(0xCB000000 | (dst << 5) | (src << 16) | dst);
}

void add_reg_reg(int dst, int src) { // Add register by register
  // ADD dst, dst, src ;; dst = dst + src
  emit_i32_le(0x8B000000 | (dst << 5) | (src << 16) | dst);
}

void add_reg_imm(int dst, int imm) { // Add immediate
  // ADD dst, dst, imm ;; dst = dst + imm
  emit_i32_le(0x91000000 | (dst << 5) | (imm << 10) | dst);
}

void mul_reg_reg(int dst, int src) { // Multiply register by register
  mul(dst, dst, src);
}

void div_reg_reg(int dst, int src) { // Divide register by register
  // SDIV dst, dst, src ;; dst = dst / src
  sdiv(dst, dst, src);
}

void rem_reg_reg(int dst, int src) {  // Compute remainder using division
  // Compute remainder using division
  // TEMP = dst / src
  sdiv(r0, dst, src);
  // dst = dst - TEMP * src
  mul(r1, r0, src); // TEMP = dst / src; r1 = TEMP * src
  sub(dst, dst, r1); // dst = dst - (TEMP * src)
}

//arm memory access instructions
void mov_mem_reg(int base, int offset, int src) { // Move from memory to register
  // STR src, [base, #offset]
  str(src, base, offset);
}

void mov_reg_mem(int dst, int base, int offset) { // Move from register to memory
  // LDR dst, [base, #offset]
  ldr(dst, base, offset);
  //or

}

//arm shift instructions
void shl_reg_cl(int dst) { // Shift left register by register
  // LSL dst, dst, cl ;; dst = dst << cl
  emit_i32_le(0x1AC02000 | (dst << 5) | dst);
}

void shl_reg_reg(int dst, int src) { // Shift left register by register
  // LSL dst, dst, src ;; dst = dst << src
  emit_i32_le(0x1AC02000 | (src << 16) | (dst << 5) | dst);
}

void sar_reg_cl(int dst) { // Shift right arithmetic register by register
  // ASR dst, dst, cl ;; dst = dst >> cl
  emit_i32_le(0x1AC02800 | (dst << 5) | dst);
}

void sar_reg_reg(int dst, int src) { // Shift right arithmetic register by register
  // ASR dst, dst, src ;; dst = dst >> src
  emit_i32_le(0x1AC02800 | (src << 16) | (dst << 5) | dst);
}

//arm stack instructions
void push_reg(int src) { // Push register to stack
  // STP src, src, [SP, #-16]! ;; Push src to stack
  emit_i32_le(0xA9BF0000 | (src << 10) | src);
}

void push_imm32_le(int imm) { // Push immediate to stack
  // MOV temp, imm
  mov_reg_imm(0, imm);
  // STP temp, temp, [SP, #-16]!
  emit_i32_le(0xA9BF0000 | (0 << 10) | 0);
}

void pop_reg(int dst) { // Pop stack to register
  // LDP dst, dst, [SP], #16 ;; Pop stack to dst
  emit_i32_le(0xA8BF0000 | (dst << 10) | dst);
}

//arm control flow instructions
void cmp_reg_reg(int reg1, int reg2) { // Compare register to register
  // CMP reg1, reg2 ;; Compare reg1 to reg2
  emit_i32_le(0xEB00001F | (reg2 << 16) | (reg1 << 5));
}


void jump(int lbl) { // Unconditional jump to label
  // B lbl ;; Unconditional branch to label
  emit_i32_le(0x14000000 | ((lbl >> 2) & 0x03FFFFFF));
}

void call(int lbl) { // Call label
  // BL lbl ;; Branch with link to label
  emit_i32_le(0x94000000 | ((lbl >> 2) & 0x03FFFFFF));
}

void ret() {
  // RET ;; Return from subroutine
  emit_i32_le(0xD65F0000);
}

// Conditions for use by jump_cond:
const int COND_EQ = 0x0; // Equal
const int COND_NE = 0x1; // Not equal
const int COND_LT = 0xB; // Less than
const int COND_GE = 0xA; // Greater than or equal
const int COND_LE = 0xD; // Less than or equal
const int COND_GT = 0xC; // Greater than


// Conditional jump to label
void jump_cond(int cond, int lbl) {
  // B.cond label ;; Conditional branch to label
  emit_i32_le(0x54000000 | (cond << 24) | ((lbl >> 2) & 0x00FFFFFF));
}

void jump_cond_reg_reg(int cond, int lbl, int reg1, int reg2) {
  cmp_reg_reg(reg1, reg2);
  jump_cond(cond, lbl);
}

//arm system instructions
void int_i8(int n) {
  // SVC imm16 ;; Supervisor call with immediate value
  emit_i32_le(0xD4000001 | ((n & 0xFFFF) << 5));
}

void os_getchar(){
  //TODO
}

void os_putchar(){
  //TODO
}

void os_exit() {
  // MOV X0, #0 ;; Exit code
  mov_reg_imm(reg_X, 0);
  // MOV X8, #93 ;; syscall number for exit
  mov_reg_imm(8, 93);
  // SVC #0 ;; Exit
  int_i8(0);
}

void os_fopen() {
  //TODO
}

void os_fclose() {
  //TODO
}

void os_fgetc() {
  //TODO
}

#endif

// TODO!
