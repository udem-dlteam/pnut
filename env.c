int cgc_fs = 0;
// Function bindings that follows lexical scoping rules
int cgc_locals = 0;
// Like cgc_locals, but with 1 scope for the entire function
int cgc_locals_fun = 0;
// Global bindings
int cgc_globals = 0;
// Bump allocator used to allocate static objects (in bytes)
int cgc_global_alloc = 0;

enum BINDING {
  // Because function params, local and global variables all share the same
  // namespace and we want to find the first one of them, we need to keep
  // BINDING_PARAM_LOCAL, BINDING_VAR_LOCAL and BINDING_VAR_GLOBAL
  // in consecutive order.
  BINDING_PARAM_LOCAL,
  BINDING_VAR_LOCAL,
  BINDING_VAR_GLOBAL,
  BINDING_ENUM_CST,
  BINDING_LOOP,
  BINDING_SWITCH,
  BINDING_FUN,
#ifdef SUPPORT_GOTO
  BINDING_GOTO_LABEL,
#endif
#ifdef SUPPORT_STRUCT_UNION
  BINDING_TYPE_STRUCT,
  BINDING_TYPE_UNION,
#endif
  BINDING_TYPE_ENUM,
};

// Some small accessor for the bindings
// All bindings have a next pointer and a kind.
// Most have an identifier, but not all
#define binding_next(binding)                     heap[binding]
#define binding_kind(binding)                     heap[binding+1]
#define binding_ident(binding)                    heap[binding+2]

// Params, locals and globals share a layout: an offset (stack slot for locals,
// cgc_global_alloc offset for globals) and the declared type.
#define var_binding_offset(binding)               heap[binding+3]
#define var_binding_type(binding)                 heap[binding+4]

#ifdef SUPPORT_EMULATED_INT64
#define switch_binding_expr_type(binding)         heap[binding+6]
#endif

int cgc_lookup_last_binding(const int binding_type, int binding) {
  while (binding != 0) {
    if (binding_kind(binding) == binding_type) {
      break;
    }
    binding = binding_next(binding);
  }
  return binding;
}

int cgc_lookup_var(const int ident, int binding) {
  while (binding != 0) {
    if (binding_kind(binding) <= BINDING_VAR_GLOBAL && binding_ident(binding) == ident) {
      break;
    }
    binding = binding_next(binding);
  }
  return binding;
}

int cgc_lookup_enclosing_loop(const int env) {
  return cgc_lookup_last_binding(BINDING_LOOP, env);
}

int cgc_lookup_enclosing_loop_or_switch(int binding) {
  while (binding != 0) {
    if (binding_kind(binding) == BINDING_LOOP || binding_kind(binding) == BINDING_SWITCH) {
      break;
    }
    binding = binding_next(binding);
  }
  return binding;
}

int cgc_add_local(const enum BINDING binding_type, const int ident, const ast type, int env) {
  int binding = alloc_obj(5);
  binding_next(binding) = env;
  binding_kind(binding) = binding_type;
  binding_ident(binding) = ident;
  var_binding_offset(binding) = cgc_fs;
  var_binding_type(binding) = type;
  return binding;
}

#if defined(target_sh) || defined(target_awk)

// A loop binding records the glo_decls range holding the loop's end actions
// (increment, etc.) so they can be replayed on continue and at the bottom of
// the loop body. A switch binding records whether it is in tail position.
#define loop_binding_action_start(binding)        heap[binding+2]
#define loop_binding_action_end(binding)          heap[binding+3]
#define switch_binding_in_tail_position(binding)  heap[binding+2]

void cgc_add_local_var(const enum BINDING binding_type, const int ident, const ast type) {
  cgc_fs += 1;
  cgc_locals = cgc_add_local(binding_type, ident, type, cgc_locals);
  // Add to cgc_locals_fun as well, if not already there
  if (cgc_lookup_var(ident, cgc_locals_fun) == 0) {
    cgc_locals_fun = cgc_add_local(binding_type, ident, type, cgc_locals_fun);
  }
}

void cgc_add_enclosing_loop() {
  int binding = alloc_obj(4);
  binding_next(binding) = cgc_locals;
  binding_kind(binding) = BINDING_LOOP;
  loop_binding_action_start(binding) = 0;
  loop_binding_action_end(binding) = 0;
  cgc_locals = binding;
}

void cgc_add_enclosing_switch(const bool in_tail_position) {
  int binding = alloc_obj(3);
  binding_next(binding) = cgc_locals;
  binding_kind(binding) = BINDING_SWITCH;
  switch_binding_in_tail_position(binding) = in_tail_position;
  cgc_locals = binding;
}

int cgc_loop_depth(int binding) {
  int loop_depth = 0;
  binding = cgc_lookup_enclosing_loop(binding); // Find the first loop
  while (binding != 0) {
    binding = cgc_lookup_enclosing_loop(binding_next(binding));
    loop_depth += 1;
  }
  return loop_depth;
}
#else

// Loop and switch bindings share their first two fields so that `break`, which
// finds either kind with cgc_lookup_enclosing_loop_or_switch, can adjust the
// stack and jump without knowing which one it got.
#define loop_or_switch_binding_fs(binding)        heap[binding+2]
#define loop_or_switch_binding_break_lbl(binding) heap[binding+3]
#define loop_binding_continue_lbl(binding)        heap[binding+4]

#define switch_binding_next_case_lbl(binding)     heap[binding+4]
#define switch_binding_default_lbl(binding)       heap[binding+5]

#define enum_binding_value(binding)               heap[binding+3]
#define goto_binding_lbl(binding)                 heap[binding+3]

// Struct, union and enum typedefs all record the type they name.
#define typedef_binding_type(binding)             heap[binding+3]

#define fun_binding_lbl(binding)                  heap[binding+3]
#define fun_binding_type(binding)                 heap[binding+4]
#ifdef ONE_PASS_GENERATOR
// Offset of the function's slot in the forward jump table.
#define fun_binding_glo_entry(binding)            heap[binding+5]
#endif

int cgc_lookup_binding_ident(const int binding_type, const int ident, int binding) {
  while (binding != 0) {
    if (binding_kind(binding) == binding_type && binding_ident(binding) == ident) {
      break;
    }
    binding = binding_next(binding);
  }
  return binding;
}

int cgc_lookup_fun(const int ident, const int env) {
  return cgc_lookup_binding_ident(BINDING_FUN, ident, env);
}

int cgc_lookup_enclosing_switch(const int env) {
  return cgc_lookup_last_binding(BINDING_SWITCH, env);
}

#ifdef SUPPORT_GOTO

int cgc_lookup_goto_label(const int ident, const int env) {
  return cgc_lookup_binding_ident(BINDING_GOTO_LABEL, ident, env);
}

#endif

#ifdef SUPPORT_STRUCT_UNION

int cgc_lookup_struct(const int ident, const int env) {
  return cgc_lookup_binding_ident(BINDING_TYPE_STRUCT, ident, env);
}

int cgc_lookup_union(const int ident, const int env) {
  return cgc_lookup_binding_ident(BINDING_TYPE_UNION, ident, env);
}

#endif

int cgc_lookup_enum(const int ident, const int env) {
  return cgc_lookup_binding_ident(BINDING_TYPE_ENUM, ident, env);
}

int cgc_lookup_enum_value(const int ident, const int env) {
  return cgc_lookup_binding_ident(BINDING_ENUM_CST, ident, env);
}

void cgc_add_local_param(const int ident, const int width, const ast type) {
  cgc_locals = cgc_add_local(BINDING_PARAM_LOCAL, ident, type, cgc_locals);
  cgc_fs -= width;
}

void cgc_add_local_var(const int ident, const int width, const ast type) {
  cgc_fs += width;
  cgc_locals = cgc_add_local(BINDING_VAR_LOCAL, ident, type, cgc_locals);
}

void cgc_add_enclosing_loop(const int loop_fs, const int break_lbl, const ast continue_lbl) {
  int binding = alloc_obj(5);
  binding_next(binding) = cgc_locals;
  binding_kind(binding) = BINDING_LOOP;
  loop_or_switch_binding_fs(binding) = loop_fs;
  loop_or_switch_binding_break_lbl(binding) = break_lbl;
  loop_binding_continue_lbl(binding) = continue_lbl;
  cgc_locals = binding;
}

#ifdef SUPPORT_EMULATED_INT64
void cgc_add_enclosing_switch(const int loop_fs, const int break_lbl, const int next_case_lbl, const ast type) {
  int binding = alloc_obj(7);
  binding_next(binding) = cgc_locals;
  binding_kind(binding) = BINDING_SWITCH;
  loop_or_switch_binding_fs(binding) = loop_fs;
  loop_or_switch_binding_break_lbl(binding) = break_lbl;
  switch_binding_next_case_lbl(binding) = next_case_lbl;
  switch_binding_default_lbl(binding) = 0;
  switch_binding_expr_type(binding) = type;
  cgc_locals = binding;
}
#else
void cgc_add_enclosing_switch(const int loop_fs, const int break_lbl, const int next_case_lbl) {
  int binding = alloc_obj(6);
  binding_next(binding) = cgc_locals;
  binding_kind(binding) = BINDING_SWITCH;
  loop_or_switch_binding_fs(binding) = loop_fs;
  loop_or_switch_binding_break_lbl(binding) = break_lbl;
  switch_binding_next_case_lbl(binding) = next_case_lbl;
  switch_binding_default_lbl(binding) = 0;
  cgc_locals = binding;
}
#endif

void cgc_add_global(const int ident, const int width, const ast type, const bool is_static_local) {
  int binding = alloc_obj(5);
  binding_next(binding) = TERNARY(is_static_local, cgc_locals, cgc_globals);
  binding_kind(binding) = BINDING_VAR_GLOBAL;
  binding_ident(binding) = ident;
  var_binding_offset(binding) = cgc_global_alloc;
  var_binding_type(binding) = type;
  cgc_global_alloc += width;
  if (is_static_local) {
    cgc_locals = binding;
  } else {
    cgc_globals = binding;
  }
}

void cgc_add_global_fun(const int ident, const int label, const ast type) {
#ifdef ONE_PASS_GENERATOR
  int binding = alloc_obj(6);
#else
  int binding = alloc_obj(5);
#endif
  binding_next(binding) = cgc_globals;
  binding_kind(binding) = BINDING_FUN;
  binding_ident(binding) = ident;
  fun_binding_lbl(binding) = label;
  fun_binding_type(binding) = type;
#ifdef ONE_PASS_GENERATOR
  fun_binding_glo_entry(binding) = cgc_global_alloc; // For forward jump table
  cgc_global_alloc += WORD_SIZE;
#endif
  cgc_globals = binding;
}

void cgc_add_enum(const int ident, const int value) {
  int binding = alloc_obj(4);
  binding_next(binding) = cgc_globals;
  binding_kind(binding) = BINDING_ENUM_CST;
  binding_ident(binding) = ident;
  enum_binding_value(binding) = value;
  cgc_globals = binding;
}

#ifdef SUPPORT_GOTO

void cgc_add_goto_label(const int ident, const int lbl) {
  int binding = alloc_obj(5);
  binding_next(binding) = cgc_locals_fun;
  binding_kind(binding) = BINDING_GOTO_LABEL;
  binding_ident(binding) = ident;
  goto_binding_lbl(binding) = lbl;
  cgc_locals_fun = binding;
}

#endif

void cgc_add_typedef(const int ident, const enum BINDING struct_or_union_or_enum, const ast type) {
  int binding = alloc_obj(4);
  binding_next(binding) = cgc_globals;
  binding_kind(binding) = struct_or_union_or_enum;
  binding_ident(binding) = ident;
  typedef_binding_type(binding) = type;
  cgc_globals = binding;
}
#endif
