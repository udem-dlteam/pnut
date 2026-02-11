#define GLO_DECL_SIZE 100000
#define GLO_DECL_ENTRY_SIZE 3
text glo_decls[GLO_DECL_SIZE];  // Generated code
int glo_decl_ix = 0;            // Index of last generated line of code
int nest_level = 0;             // Current level of indentation

void append_glo_decl(text decl) {
  if (glo_decl_ix + GLO_DECL_ENTRY_SIZE >= GLO_DECL_SIZE) fatal_error("glo_decls overflow");
  glo_decls[glo_decl_ix] = nest_level;
  glo_decls[glo_decl_ix + 1] = 1; // If it's active or not. Used by undo_glo_decls and replay_glo_decls
  glo_decls[glo_decl_ix + 2] = decl;
  glo_decl_ix += GLO_DECL_ENTRY_SIZE;
}

// Fixups are represented as negative nest levels (-1, -2, ...). The actual
// nest level is obtained by negating the value and subtracting 1.
int append_glo_decl_fixup() {
  if (glo_decl_ix + GLO_DECL_ENTRY_SIZE >= GLO_DECL_SIZE) fatal_error("glo_decls overflow");
  glo_decls[glo_decl_ix] = - (nest_level + 1);
  glo_decls[glo_decl_ix + 1] = 1; // If it's active or not. Used by undo_glo_decls and replay_glo_decls
  glo_decls[glo_decl_ix + 2] = 0;
  glo_decl_ix += GLO_DECL_ENTRY_SIZE;
  return glo_decl_ix - GLO_DECL_ENTRY_SIZE;
}

void fixup_glo_decl(int fixup_ix, text decl) {
  if (glo_decls[fixup_ix] >= 0) fatal_error("fixup_glo_decl: invalid fixup");

  glo_decls[fixup_ix] = -glo_decls[fixup_ix] - 1; // Make nest level positive
  glo_decls[fixup_ix + 2] = decl;
}

// Remove the n last declarations by decrementing the active field.
// A non-positive active value means that the declaration is active,
// A 0 value means that the declaration was unset once.
// A negative value means that the declaration was unset multiple times.
// Because undone declarations are generally replayed, declarations with negative
// values are ignored when replayed since they have already been replayed before.
// This is useful to compile some code at a different time than it is used.
void undo_glo_decls(int start) {
  while (start < glo_decl_ix) {
    glo_decls[start + 1] -= 1; // To support nested undone declarations
    start += GLO_DECL_ENTRY_SIZE;
  }
}

// Check if there are any active and non-empty declarations since the start index.
// This is used to determine if a ':' statement must be added to the current block.
bool any_active_glo_decls(int start) {
  while (start < glo_decl_ix) {
    if (glo_decls[start + 1] && glo_decls[start + 2] != 0) return true;
    start += GLO_DECL_ENTRY_SIZE;
  }
  return false;
}

// Replay the declarations betwee start and end. Replayed declarations must first
// be undone with undo_glo_decls.
void replay_glo_decls(int start, int end) {
  while (start < end) {
    if (glo_decls[start + 1] == 0) { // Skip inactive declarations that are at the current level
      append_glo_decl(glo_decls[start + 2]);
    }
    start += GLO_DECL_ENTRY_SIZE;
  }
}

text replay_glo_decls_inline(int start, int end) {
  text res = 0;
  while (start < end) {
    if (glo_decls[start + 1] == 0) { // Skip inactive declarations
      res = concatenate_strings_with(res, glo_decls[start + 2], wrap_str_lit("; "));
    }
    start += GLO_DECL_ENTRY_SIZE;
  }
  if (res != 0) { res = string_concat(res, wrap_str_lit("; ")); }

  return res;
}

void print_glo_decls() {
  int i = 0;
  int level;
  while (i < glo_decl_ix) {
    if (glo_decls[i + 1] == 1) { // Skip inactive declarations
      if (glo_decls[i + 2] != 0) {
        level = glo_decls[i];
        while (level > 0) {
          putchar(' '); putchar(' ');
          level -= 1;
        }
        print_text(glo_decls[i + 2]);
        putchar('\n');
      }
    }
    i += GLO_DECL_ENTRY_SIZE;
  }
}
