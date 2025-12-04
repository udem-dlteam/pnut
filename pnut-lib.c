// Used for the pnut.sh website.

#include "pnut.c"

void compile(bool annotate, char* file) {

  int i;
  ast decl;

#ifdef SH_INCLUDE_C_CODE
  code_annotations_quiet_mode = !annotate;
#endif

  init_ident_table();

  init_pnut_macros();

  include_file(file, 0);

  codegen_begin();

  ch = '\n';
  get_tok();

  while (tok != EOF) {
    decl = parse_declaration(false);
#ifdef SH_INCLUDE_C_CODE
    if (!code_annotations_quiet_mode) {
      output_declaration_c_code();
    }
#endif
    codegen_glo_decl(decl);
  }

  codegen_end();
}
