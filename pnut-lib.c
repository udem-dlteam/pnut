// Used for the pnut.sh website.

#define SUPPORT_INCLUDE

#include "pnut.c"

void compile(char* file) {

  int i;
  ast decl;

  init_ident_table();

  init_pnut_macros();

  include_file(file, 0);

  codegen_begin();

  ch = '\n';
  get_tok();

  while (tok != EOF) {
    decl = parse_declaration(false);
    codegen_glo_decl(decl);
  }

  codegen_end();
}
