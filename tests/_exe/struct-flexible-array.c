#include <stdio.h>
#include <stdlib.h>
#include <stdint.h> // for intptr_t

#ifdef PNUT_CC
// On pnut, intptr_t is not defined
#define intptr_t int
#endif

void putint(int n) {
  if (n < 0) {
    putchar('-');
    n = -n;
  }
  if (n >= 10) {
    putint(n / 10);
  }
  putchar('0' + n % 10);
}

enum OP {
  IMM,
  ADD,
  TERNARY,
};

typedef struct Instruction {
  enum OP code;
  intptr_t opnds[];
} Instruction;

#define to_instr(ptr) ((Instruction*) ptr)
#define from_instr(instr) ((intptr_t) instr)

Instruction *imm(int val) {
  Instruction *instr = (Instruction*) malloc(sizeof(Instruction) + 1 * sizeof(Instruction*));
  instr->code = IMM;
  instr->opnds[0] = (intptr_t) val;
  return instr;
}

Instruction *add(Instruction *r1, Instruction *r2) {
  Instruction *instr = (Instruction*) malloc(sizeof(Instruction) + 2 * sizeof(Instruction*));
  instr->code = ADD;
  instr->opnds[0] = from_instr(r1);
  instr->opnds[1] = from_instr(r2);
  return instr;
}

Instruction *ternary(Instruction *r1, Instruction *r2, Instruction *r3) {
  Instruction *instr = (Instruction*) malloc(sizeof(Instruction) + 3 * sizeof(Instruction*));
  instr->code = TERNARY;
  instr->opnds[0] = from_instr(r1);
  instr->opnds[1] = from_instr(r2);
  instr->opnds[2] = from_instr(r3);
  return instr;
}

int eval(Instruction *instr) {
  switch (instr->code) {
    case IMM:     return (int) instr->opnds[0];
    case ADD:     return eval(to_instr(instr->opnds[0])) + eval(to_instr(instr->opnds[1]));
    case TERNARY: return eval(to_instr(instr->opnds[0])) ? eval(to_instr(instr->opnds[1])) : eval(to_instr(instr->opnds[2]));
    default:      return 0;
  }
}

int main() {
  Instruction *instr = ternary(imm(1), add(imm(2), imm(3)), imm(4));
  putint(eval(instr)); putchar('\n');
  return 0;
}
