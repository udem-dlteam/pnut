#include <stdio.h>
#include <stdlib.h>

void putstr(char* s) {
  while (*s) {
    putchar(*s);
    s += 1;
  }
}

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
};

struct Instruction {
  enum OP code;
  union {
    int imm;
    struct {
      struct Instruction* r1;
      struct Instruction* r2;
    };
  };
};

int eval(struct Instruction *instr) {
  switch (instr->code) {
    case IMM: return instr->imm;
    case ADD: return eval(instr->r1) + eval(instr->r2);
    default:  return 0;
  }
}

struct Instruction* imm(int val) {
  struct Instruction* instr = (struct Instruction*) malloc(sizeof(struct Instruction));
  instr->code = IMM;
  instr->imm = val;
  return instr;
}

struct Instruction* add(struct Instruction* r1, struct Instruction* r2) {
  struct Instruction* instr = (struct Instruction*) malloc(sizeof(struct Instruction));
  instr->code = ADD;
  instr->r1 = r1;
  instr->r2 = r2;
  return instr;
}

int main() {
  struct Instruction instr;
  struct Instruction imm1;
  imm1.code = IMM;
  imm1.imm = 5;

  instr.code = ADD;
  instr.r1 = &imm1;
  instr.r2 = &imm1;
  putstr("(stack allocated) 5 + 5 = ");
  putint(eval(&instr));
  putchar('\n');

  putstr("(heap allocated)  5 + 10 = ");
  putint(eval(add(imm(5), imm(10))));
  putchar('\n');

  return 0;
}
