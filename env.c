int cgc_fs = 0;
// Function bindings that follows lexical scoping rules
int cgc_locals = 0;
// Like cgc_locals, but with 1 scope for the entire function
int cgc_locals_fun = 0;
// Global bindings
int cgc_globals = 0;
// Bump allocator used to allocate static objects
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
  BINDING_GOTO_LABEL,
  BINDING_TYPE_STRUCT,
  BINDING_TYPE_UNION,
  BINDING_TYPE_ENUM,
};

// Some small accessor for the bindings
// All bindings have a next pointer and a kind.
// Most have an identifier, but not all
#define binding_next(binding)  heap[binding]
#define binding_kind(binding)  heap[binding+1]
#define binding_ident(binding) heap[binding+2]

int cgc_lookup_binding_ident(int binding_type, int ident, int binding) {
  while (binding != 0) {
    if (binding_kind(binding) == binding_type && binding_ident(binding) == ident) {
      break;
    }
    binding = binding_next(binding);
  }
  return binding;
}

int cgc_lookup_last_binding(int binding_type, int binding) {
  while (binding != 0) {
    if (binding_kind(binding) == binding_type) {
      break;
    }
    binding = binding_next(binding);
  }
  return binding;
}

int cgc_lookup_var(int ident, int binding) {
  while (binding != 0) {
    if (binding_kind(binding) <= BINDING_VAR_GLOBAL && binding_ident(binding) == ident) {
      break;
    }
    binding = binding_next(binding);
  }
  return binding;
}

int cgc_lookup_fun(int ident, int env) {
  return cgc_lookup_binding_ident(BINDING_FUN, ident, env);
}

int cgc_lookup_enclosing_loop(int env) {
  return cgc_lookup_last_binding(BINDING_LOOP, env);
}

int cgc_lookup_enclosing_switch(int env) {
  return cgc_lookup_last_binding(BINDING_SWITCH, env);
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

int cgc_lookup_goto_label(int ident, int env) {
  return cgc_lookup_binding_ident(BINDING_GOTO_LABEL, ident, env);
}

int cgc_lookup_struct(int ident, int env) {
  return cgc_lookup_binding_ident(BINDING_TYPE_STRUCT, ident, env);
}

int cgc_lookup_union(int ident, int env) {
  return cgc_lookup_binding_ident(BINDING_TYPE_UNION, ident, env);
}

int cgc_lookup_enum(int ident, int env) {
  return cgc_lookup_binding_ident(BINDING_TYPE_ENUM, ident, env);
}

int cgc_lookup_enum_value(int ident, int env) {
  return cgc_lookup_binding_ident(BINDING_ENUM_CST, ident, env);
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

int cgc_add_local(enum BINDING binding_type, int ident, ast type, int env) {
  int binding = alloc_obj(5);
  heap[binding+0] = env;
  heap[binding+1] = binding_type;
  heap[binding+2] = ident;
  heap[binding+3] = cgc_fs;
  heap[binding+4] = type;
  return binding;
}

#ifdef sh
void cgc_add_local_var(enum BINDING binding_type, int ident, ast type) {
  cgc_fs += 1;
  cgc_locals = cgc_add_local(binding_type, ident, type, cgc_locals);
  // Add to cgc_locals_fun as well, if not already there
  if (cgc_lookup_var(ident, cgc_locals_fun) == 0) {
    cgc_locals_fun = cgc_add_local(binding_type, ident, type, cgc_locals_fun);
  }
}

void cgc_add_enclosing_loop() {
  int binding = alloc_obj(4);
  heap[binding+0] = cgc_locals;
  heap[binding+1] = BINDING_LOOP;
  heap[binding+2] = 0; // loop end action start
  heap[binding+3] = 0; // loop end action end
  cgc_locals = binding;
}

void cgc_add_enclosing_switch(bool in_tail_position) {
  int binding = alloc_obj(3);
  heap[binding+0] = cgc_locals;
  heap[binding+1] = BINDING_SWITCH;
  heap[binding+2] = in_tail_position;
  cgc_locals = binding;
}
#else
void cgc_add_local_param(int ident, int width, ast type) {
  cgc_locals = cgc_add_local(BINDING_PARAM_LOCAL, ident, type, cgc_locals);
  cgc_fs -= width;
}

void cgc_add_local_var(int ident, int width, ast type) {
  cgc_fs += width;
  cgc_locals = cgc_add_local(BINDING_VAR_LOCAL, ident, type, cgc_locals);
}

void cgc_add_enclosing_loop(int loop_fs, int break_lbl, ast continue_lbl) {
  int binding = alloc_obj(5);
  heap[binding+0] = cgc_locals;
  heap[binding+1] = BINDING_LOOP;
  heap[binding+2] = loop_fs;
  heap[binding+3] = break_lbl;
  heap[binding+4] = continue_lbl;
  cgc_locals = binding;
}

void cgc_add_enclosing_switch(int loop_fs, int break_lbl, int next_case_lbl) {
  int binding = alloc_obj(6);
  heap[binding+0] = cgc_locals;
  heap[binding+1] = BINDING_SWITCH;
  heap[binding+2] = loop_fs;
  heap[binding+3] = break_lbl;
  heap[binding+4] = next_case_lbl;
  heap[binding+5] = 0; // Default label
  cgc_locals = binding;
}

void cgc_add_global(int ident, int width, ast type, bool is_static_local) {
  int binding = alloc_obj(5);
  heap[binding+0] = is_static_local ? cgc_locals : cgc_globals;
  heap[binding+1] = BINDING_VAR_GLOBAL;
  heap[binding+2] = ident;
  heap[binding+3] = cgc_global_alloc;
  heap[binding+4] = type;
  cgc_global_alloc += width;
  if (is_static_local) {
    cgc_locals = binding;
  } else {
    cgc_globals = binding;
  }
}

void cgc_add_global_fun(int ident, int label, ast type) {
  int binding = alloc_obj(6);
  heap[binding+0] = cgc_globals;
  heap[binding+1] = BINDING_FUN;
  heap[binding+2] = ident;
  heap[binding+3] = 0;
  heap[binding+4] = label;
  heap[binding+5] = type;
  cgc_globals = binding;
}

void cgc_add_enum(int ident, int value) {
  int binding = alloc_obj(4);
  heap[binding+0] = cgc_globals;
  heap[binding+1] = BINDING_ENUM_CST;
  heap[binding+2] = ident;
  heap[binding+3] = value;
  cgc_globals = binding;
}

void cgc_add_goto_label(int ident, int lbl) {
  int binding = alloc_obj(5);
  heap[binding+0] = cgc_locals_fun;
  heap[binding+1] = BINDING_GOTO_LABEL;
  heap[binding+2] = ident;
  heap[binding+3] = lbl;
  cgc_locals_fun = binding;
}

void cgc_add_typedef(int ident, enum BINDING struct_or_union_or_enum, ast type) {
  int binding = alloc_obj(4);
  heap[binding+0] = cgc_globals;
  heap[binding+1] = struct_or_union_or_enum;
  heap[binding+2] = ident;
  heap[binding+3] = type;
  cgc_globals = binding;
}
#endif
