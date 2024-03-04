/*
Changes to make it compatible with six-cc.scm
1. Replace //.*\n with \n
2. split global declarations and replace (char/int)* with char_ptr/int_ptr
3. Define ops_string once
4. Change PRTF instruction because printf can't return (hardcode `a = 0`)
5. Replace sizeof(char/int) with 1
-- After that, we're breaking compatibility with the C standard
6. Enums
7. Remove casts
8. Remove #include and typedefs
9. Fix 1-character string bug
10. Add parens around x in `*x * y` expressions to work-around six parsing bug:
    - See *datastart / 16, and MUL/DIV/MOD instructions in VM
11. Fix ops_string access
12. Replace %.4s with %0.4s
13. Rename argv to args
*/

/*
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long

typedef typeof(char*) char_ptr;
typedef typeof(char**) char_ptr_ptr;
typedef typeof(int*) int_ptr;
*/

char_ptr p;
char_ptr lp;
char_ptr data;

int_ptr e;
int_ptr le;
int_ptr id;
int_ptr sym;
int tk;
int ival;
int ty;
int loc;
int line;
int src;
int debug;
int ops;
int portable;

int sizeof_int = 1;
int sizeof_char = 1;

char_ptr ops_string;
ops_string="LEA ,IMM ,REF ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,";

enum A() {
  Num = 128; Fun; Sys; Glo; Loc; Id;
  Char; Else; Enum; If; Int; Return; Sizeof; While;
  Assign; Cond; Lor; Lan; Or; Xor; And; Eq; Ne; Lt; Gt; Le; Ge; Shl; Shr; Add; Sub; Mul; Div; Mod; Inc; Dec; Brak;
}

enum A() { LEA ;IMM ;REF ;JMP ;JSR ;BZ  ;BNZ ;ENT ;ADJ ;LEV ;LI  ;LC  ;SI  ;SC  ;PSH ;
       OR  ;XOR ;AND ;EQ  ;NE  ;LT  ;GT  ;LE  ;GE  ;SHL ;SHR ;ADD ;SUB ;MUL ;DIV ;MOD ;
       OPEN;READ;CLOS;PRTF;MALC;FREE;MSET;MCMP;EXIT; }

enum A() { CHAR; INT; PTR; }

enum A() { Tk; Hash; Name; Class; Type; Val; HClass; HType; HVal; Idsz; }

/*
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

enum { LEA ,IMM ,REF ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT };

enum { CHAR, INT, PTR };

enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };
*/

void next()
{
  char_ptr pp;

  while (tk = *p) {
    ++p;
    if (tk == '\n') {
      if (src) {
        printf("%d: %.*s", line, p - lp, lp);
        lp = p;
        while (le < e) {
          printf("%8.4s", ops_string + (*++le * 5));
          if (*le <= ADJ) printf(" %d\n", *++le); else printf(" \n");
        }
      }
      ++line;
    }
    else if (tk == '#') {
      while (*p != 0 && *p != '\n') ++p;
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
      pp = p - 1;
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 47 + *p++;
      tk = (tk << 6) + (p - pp);
      id = sym;
      while (id[Tk]) {
        if (tk == id[Hash] && !memcmp(id[Name], pp, p - pp)) { tk = id[Tk]; return; }
        id = id + Idsz;
      }
      id[Name] = pp;
      id[Hash] = tk;
      tk = id[Tk] = Id;
      return;
    }
    else if (tk >= '0' && tk <= '9') {
      if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }
      else if (*p == 'x' || *p == 'X') {
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
      }
      else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }
      tk = Num;
      return;
    }
    else if (tk == '/') {
      if (*p == '/') {
        ++p;
        while (*p != 0 && *p != '\n') ++p;
      }
      else {
        tk = Div;
        return;
      }
    }
    else if (tk == '\'' || tk == '"') {
      pp = data;
      while (*p != 0 && *p != tk) {
        if ((ival = *p++) == '\\') {
          if ((ival = *p++) == 'n') ival = '\n';
        }
        if (tk == '"') *data++ = ival;
      }
      ++p;
      if (tk == '"') ival = pp; else tk = Num;
      return;
    }
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}

void expr(int lev)
{
  int t;
  int_ptr d;

  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }
  else if (tk == '"') {
    *++e = REF; *++e = ival; next();
    while (tk == '"') next();
    data = (data + sizeof_int & -sizeof_int); ty = PTR;
  }
  else if (tk == Sizeof) {
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
    ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }
    while (tk == Mul) { next(); ty = ty + PTR; }
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
    *++e = IMM; *++e = portable ? 1 : (ty == CHAR) ? sizeof_char : sizeof_int;
    ty = INT;
  }
  else if (tk == Id) {
    d = id; next();
    if (tk == '(') {
      next();
      t = 0;
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }
      next();
      if (d[Class] == Sys) *++e = d[Val];
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }
      else { printf("%d: bad function call\n", line); exit(-1); }
      if (t) { *++e = ADJ; *++e = t; }
      ty = d[Type];
    }
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
    else {
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }
      else if (d[Class] == Glo) { *++e = REF; *++e = d[Val]; }
      else { printf("%d: undefined variable\n", line); exit(-1); }
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
    }
  }
  else if (tk == '(') {
    next();
    if (tk == Int || tk == Char) {
      t = (tk == Int) ? INT : CHAR; next();
      while (tk == Mul) { next(); t = t + PTR; }
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc);
      ty = t;
    }
    else {
      expr(Assign);
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  else if (tk == Mul) {
    next(); expr(Inc);
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
    *++e = (ty == CHAR) ? LC : LI;
  }
  else if (tk == And) {
    next(); expr(Inc);
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }
    ty = ty + PTR;
  }
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }
  else if (tk == Add) { next(); expr(Inc); ty = INT; }
  else if (tk == Sub) {
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    ty = INT;
  }
  else if (tk == Inc || tk == Dec) {
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; }
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = portable ? 1 : (ty > PTR) ? sizeof_int : sizeof_char;
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;
  }
  else { printf("%d: bad expression\n", line); exit(-1); }

  while (tk >= lev) {
    t = ty;
    if (tk == Assign) {
      next();
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    else if (tk == Cond) {
      next();
      *++e = BZ; d = ++e;
      expr(Assign);
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (e + 3); *++e = JMP; d = ++e;
      expr(Cond);
      *d = (e + 1);
    }
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (e + 1); ty = INT; }
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (e + 1); ty = INT; }
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    else if (tk == Add) {
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = portable ? 1 : sizeof_int; *++e = MUL;  }
      *++e = ADD;
    }
    else if (tk == Sub) {
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = portable ? 1 : sizeof_int; *++e = DIV; ty = INT; }
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = portable ? 1 : sizeof_int; *++e = MUL; *++e = SUB; }
      else *++e = SUB;
    }
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
    else if (tk == Inc || tk == Dec) {
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      *++e = PSH; *++e = IMM; *++e = portable ? 1 : (ty > PTR) ? sizeof_int : sizeof_char;
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == CHAR) ? SC : SI;
      *++e = PSH; *++e = IMM; *++e = portable ? 1 : (ty > PTR) ? sizeof_int : sizeof_char;
      *++e = (tk == Inc) ? SUB : ADD;
      next();
    }
    else if (tk == Brak) {
      next(); *++e = PSH; expr(Assign);
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = portable ? 1 : sizeof_int; *++e = MUL;  }
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      *++e = ADD;
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}

void stmt()
{
  int_ptr a;
  int_ptr b;

  if (tk == If) {
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;
    stmt();
    if (tk == Else) {
      *b = (e + 3); *++e = JMP; b = ++e;
      next();
      stmt();
    }
    *b = (e + 1);
  }
  else if (tk == While) {
    next();
    a = e + 1;
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;
    stmt();
    *++e = JMP; *++e = a;
    *b = (e + 1);
  }
  else if (tk == Return) {
    next();
    if (tk != ';') expr(Assign);
    *++e = LEV;
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
  else if (tk == '{') {
    next();
    while (tk != '}') stmt();
    next();
  }
  else if (tk == ';') {
    next();
  }
  else {
    expr(Assign);
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

int main(int argc, char_ptr_ptr args)
{
  int fd;
  int bt;
  int ty;
  int poolsz;
  int_ptr idmain;
  int_ptr pc;
  int_ptr sp;
  int_ptr bp;
  int a;
  int cycle;
  int i;
  int_ptr t;
  int_ptr spstart;
  int_ptr stack_ix;

  int_ptr estart;
  char_ptr datastart;
  char_ptr datastart2;

  --argc; ++args;
  if (argc > 0 && **args == '-' && (*args)[1] == 's') { src = 1; --argc; ++args; }
  if (argc > 0 && **args == '-' && (*args)[1] == 'd') { debug = 1; --argc; ++args; }
  if (argc > 0 && **args == '-' && (*args)[1] == 'b') { ops = 1; --argc; ++args; }
  if (argc > 0 && **args == '-' && (*args)[1] == 'p') { portable = 1; --argc; ++args; }
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }

  if ((fd = open(*args, 0)) < 0) { printf("could not open(%s)\n", *args); return -1; }

  poolsz = 32*1024;
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(estart = le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  if (!(datastart = data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }

  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  p = "char else enum if int return sizeof while open read close printf malloc free memset memcmp exit void main";
  i = Char; while (i <= While) { next(); id[Tk] = i++; }
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; }
  next(); id[Tk] = Char;
  next(); idmain = id;

  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
  p[i] = 0;
  close(fd);

  line = 1;
  next();
  while (tk) {
    bt = INT;
    if (tk == Int) next();
    else if (tk == Char) { next(); bt = CHAR; }
    else if (tk == Enum) {
      next();
      if (tk != '{') next();
      if (tk == '{') {
        next();
        i = 0;
        while (tk != '}') {
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
          next();
          if (tk == Assign) {
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival;
            next();
          }
          id[Class] = Num; id[Type] = INT; id[Val] = i++;
          if (tk == ',') next();
        }
        next();
      }
    }
    while (tk != ';' && tk != '}') {
      ty = bt;
      while (tk == Mul) { next(); ty = ty + PTR; }
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
      next();
      id[Type] = ty;
      if (tk == '(') {
        id[Class] = Fun;
        id[Val] = (e + 1);
        next(); i = 0;
        while (tk != ')') {
          ty = INT;
          if (tk == Int) next();
          else if (tk == Char) { next(); ty = CHAR; }
          while (tk == Mul) { next(); ty = ty + PTR; }
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;
          next();
          if (tk == ',') next();
        }
        next();
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        loc = ++i;
        next();
        while (tk == Int || tk == Char) {
          bt = (tk == Int) ? INT : CHAR;
          next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;
            next();
            if (tk == ',') next();
          }
          next();
        }
        *++e = ENT; *++e = i - loc;
        while (tk != '}') stmt();
        *++e = LEV;
        id = sym;
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      else {
        id[Class] = Glo;
        id[Val] = data;
        data = data + sizeof_int;
      }
      if (tk == ',') next();
    }
    next();
  }

  if (!(pc = idmain[Val])) { printf("main() not defined\n"); return -1; }
  if (src) return 0;

  if (ops) {
    estart++;
    printf("%d\n", data - datastart);
    datastart2 = datastart;
    while (datastart < data) {
      if (*datastart <= 31 || *datastart >= 127 || *datastart == '\\') {
        printf("\\%x%x", (*datastart) / 16, (*datastart) % 16);
      } else {
        printf("%c", *datastart);
      }
      datastart++;
    }
    datastart = datastart2;
    printf(" \n");

    printf("%d\n", (pc - estart));
    le = estart;
    while (le <= e) {
      i = *le;
      if (i == JMP || i == JSR || i == BZ || i == BNZ) {
        printf("%4.4s", ops_string + i * 5);
        if (*le <= ADJ) { printf(" %d\n", ((*++le) -  estart) / sizeof_int); }
        else { printf(" \n"); }
      } else if (i == REF) {
        printf("REF  %d\n", (*++le -  datastart));
      } else {
        printf("%4.4s", ops_string + i * 5);
        if (*le <= ADJ) printf(" %d\n", *++le); else printf(" \n");
      }
      le++;
    }
    return 0;
  }

  if (!(sp = malloc(256 * 1024))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }

  bp = sp = (sp + poolsz);
  spstart = sp;
  *--sp = EXIT;
  *--sp = PSH; t = sp;
  *--sp = argc;
  *--sp = args;
  *--sp = t;

  cycle = 0;
  while (1) {
    i = *pc++; ++cycle;
    if (debug) {
      printf("%d> %0.4s", cycle, ops_string + i * 5);
      if (i <= ADJ) printf(" %d\n", *pc); else printf(" \n");

      printf("    pc = %d, sp = %d, bp = %d, a = %d\n", pc, sp, bp, a);

      stack_ix = spstart;
      while (stack_ix > sp) {
        stack_ix--;
        printf("    _stack_%d = %d\n", stack_ix, *stack_ix);
      }
    }
    if      (i == LEA) a = (bp + *pc++);
    else if (i == IMM) a = *pc++;
    else if (i == REF) a = *pc++;
    else if (i == JMP) pc = *pc;
    else if (i == JSR) { *--sp = (pc + 1); pc = *pc; }
    else if (i == BZ)  pc = a ? pc + 1 : *pc;
    else if (i == BNZ) pc = a ? *pc : pc + 1;
    else if (i == ENT) { *--sp = bp; bp = sp; sp = sp - *pc++; }
    else if (i == ADJ) sp = sp + *pc++;
    else if (i == LEV) { sp = bp; bp = *sp++; pc = *sp++; }
    else if (i == LI)  a = *a;
    else if (i == LC)  a = *a;
    else if (i == SI)  **sp++ = a;
    else if (i == SC)  a = **sp++ = a;
    else if (i == PSH) *--sp = a;

    else if (i == OR)  a = (*sp++) |  a;
    else if (i == XOR) a = (*sp++) ^  a;
    else if (i == AND) a = (*sp++) &  a;
    else if (i == EQ)  a = (*sp++) == a;
    else if (i == NE)  a = (*sp++) != a;
    else if (i == LT)  a = (*sp++) <  a;
    else if (i == GT)  a = (*sp++) >  a;
    else if (i == LE)  a = (*sp++) <= a;
    else if (i == GE)  a = (*sp++) >= a;
    else if (i == SHL) a = (*sp++) << a;
    else if (i == SHR) a = (*sp++) >> a;
    else if (i == ADD) a = (*sp++) +  a;
    else if (i == SUB) a = (*sp++) -  a;
    else if (i == MUL) a = (*sp++) *  a;
    else if (i == DIV) a = (*sp++) /  a;
    else if (i == MOD) a = (*sp++) %  a;

    else if (i == OPEN) a = open(sp[1], *sp);
    else if (i == READ) a = read(sp[2], sp[1], *sp);
    else if (i == CLOS) a = close(*sp);
    else if (i == PRTF) { t = sp + pc[1]; a = 0; printf(t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
    else if (i == MALC) a = malloc(*sp);
    else if (i == FREE) free(*sp);
    else if (i == MSET) a = memset(sp[2], sp[1], *sp);
    else if (i == MCMP) a = memcmp(sp[2], sp[1], *sp);
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
  }
}
