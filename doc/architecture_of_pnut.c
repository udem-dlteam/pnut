//############################ Architecture of pnut ############################
//
// This comment block describes the architecture of pnut, covering its design
// and implementation. For each parts of the compiler, the relevant functions
// and variables are mentioned to help the reader navigate the codebase.
//
// Pnut features two compiler backends: one targeting human-readable POSIX shell
// code and another generating machine code (x86 only). Since both backends are
// required for bootstrapping, we distinguish between the compiler that
// generates shell code, which we call pnut-sh, and the compiler that generates
// machine code, which we call pnut-exe. Both compilers share the same frontend,
// which includes the reader, lexer and parser, and the same intermediate
// representation, which is an abstract syntax tree (AST).
//
// Because not every C construct can be compiled to human-readable POSIX shell,
// pnut-sh only compiles programs written in a specific subset of C. This gap
// between POSIX shell and C99 is bridged using pnut-exe, which supports most
// C99 features and can compile real C programs.
//
// Execution of pnut-sh generated scripts is significantly slower than native
// execution. This, combined with the fact that human-readability limits the
// possible optimizations, means that the primary way to reduce the time it
// takes to get to the executable pnut-exe is to generate code more efficiently,
// rather than generating faster code.
//
// Because the execution time of shell scripts is correlated with the numbers of
// variables they define, pnut is designed using a one-pass architecture that
// processes one top-level declaration at a time, maintaining a small state to
// keep track of the global scope, which includes functions, global variables,
// type declarations and macro declarations.
//
// Additionally, pnut is designed to be a single executable that performs all
// the steps of compilation, from preprocessing to machine code generation and
// linking.
//
//############################### Step 1: Reader ###############################
//
// For the reader, the entire file is read character by character as the memory
// usage from reading the whole file at once would overwhelm certain shells. To
// support #include directives, the reader maintains a stack of open files so it
// can start reading new files as they are included, and resume reading the
// parent file when it is done. The include stack also keeps track of the file
// path to compute the path of relative includes. For debugging, with the right
// compilation options, the reader can keep track of the line and column number
// so errors can be tagged with a location.
//
// Relevant variables and functions:
//  - ch: current character, represented as an integer (ASCII code or EOF)
//  - fp: current file pointer
//  - fp_filepath: path of the current file being read, for error messages
//  - fp_dirname: directory of the current file, used to resolve relative paths
//  - line_number/column_number: current position in file, for error messages
//  - include_search_path: search path for system include files (-I option)
//  - include_stack: stack of open files, to keep track of the reader state
//
//  - get_ch(): returns the next character and moves the cursor forward by one
//  - include_file(): starts reading a new file
//  - save_include_context(): saves the reader state before including a file
//  - restore_include_context(): restores the reader state after reading file
//
//############################## Step 2: Tokenizer #############################
//
// Next, the tokenizer consumes each character and produces a stream of tokens
// representing C language keywords, identifiers, operators and literals.
// Keyword, identifier and string literal tokens are interned so the underlying
// string can be shared between identical tokens, allowing constant-time
// comparison using pointer equality. The intern table also serves as the symbol
// table, storing metadata associated with each identifier, such as its kind
// (keyword, macro, typedef or other identifier), as well as metadata used for
// macros and typedefs. Doing so avoids the need for separate tables for
// typedefs and macros, making the implementation smaller and avoiding redundant
// table lookups.
//
// Relevant variables and functions (symbol table):
//  - heap: array of objects allocated by tokenizer (symbols) and parser (AST)
//  - string_pool: array of interned strings, where value of symbols are stored
//
//  - alloc_obj(): allocates an object in the heap and returns its index
//  - begin_symbol(): init interning context, start accumulating chars of symbol
//  - end_symbol(): intern the accumulated chars and return its symbol id
//  - intern_symbol(): intern a string and return its symbol id
//  - symbol_{buf,len,type,tag}(): get symbol string and metadata
//
// The tokenizer also performs macro expansion, recognizing the following
// preprocessor directives:
//   - Object macros such as #define FOO 123
//   - Function-like macros such as #define BAR(X) X + FOO
//   - Conditional groups: #if, #ifdef, #else, #elif and #endif
//   - Diagnostic macros: #warning and #error
//   - Predefined macros: __FILE__ and __LINE__
//   - System and user includes: #include <stdio.h> and #include "foo.h"
//
// In standard C, macros can expand to other macros, or take macros as
// parameters, both of which result in further expansion. Pnut's tokenizer
// recursively expands macros without any special handling for self-referential
// macros, meaning self-referential macros expand indefinitely. This departure
// from the C language simplifies the expansion logic, but means
// self-referencing macros cannot be used in pnut as they hit the maximum
// expansion depth, which raises an error.
//
// Because preprocessor macros are global, they are kept in the symbol table,
// alongside their arity and their list of tokens, for the entire execution
// (unless #undef is used). An unfortunate consequence of this design is that it
// prevents the symbol table from ever being cleared between declarations. The
// symbol table is one of the few data structures of pnut that never shrinks. In
// practice, however, most symbols in pnut are the same, and thus the table size
// remains relatively stable.
//
// Relevant types, variables and functions (tokenizer):
//  - enum TOKEN: of token types, including keywords, operators and literals.
//    Token values in the ASCII range are reserved for single-character tokens
//  - tok: current token type, represented as an integer
//  - val: value of the current token, integer if number literal, symbol id for
//    identifiers and string literals
//
//  - get_tok(): advances the token stream by one and updates tok and val
//  - init_ident_table(): initializes the symbol table with keywords
//  - init_pnut_macros(): initializes the symbol table with predefined macros
//
// Relevant types, variables and functions (macro expansion):
//  - expand_macro: Enable/disable macro expansion
//  - expand_macro_arg: Enable/disable macro expansion for macro arguments
//  - skip_newlines: Enable/disable skipping of newlines in tokenizer
//  - if_macro_stack: Stack to keep track of nested conditional groups
//  - if_macro_mask: If the current conditional group is active or not
//  - if_macro_executed: If any branch of the current conditional group has run
//  - macro_stack: Stack to keep track of nested macro expansions
//  - macro_tok_lst: List of tokens of the macro being expanded
//
//  - get_tok_macro(): like get_tok, but with macro expansion disabled
//  - handle_preprocessor_directive(): entry point for handling cpp directives
//  - handle_define(): handles #define directives
//  - handle_include(): handles #include directives
//  - evaluate_if_condition(): evaluates the condition of #if and #elif
//  - begin_macro_expansion/return_to_parent_macro(): manage expansion context
//  - attempt_macro_expansion(): starts expansion when a macro token is found
//
//################################### Parser ###################################
//
// The parser is a simple recursive descent parser supporting a large subset of
// C99. Like the reader and tokenizer, the parser is hand-written, so its
// implementation can be tailored to the constraints of pnut, such as the need
// to keep memory usage low and to keep the code simple and easy to understand.
//
// The parser advances one top-level declaration at a time, producing an
// abstract syntax tree (AST) object that is then passed to the code generator.
// The code generator then processes the AST and outputs the code for that
// declaration before returning to the parser to parse the next top-level
// declaration. Unfortunately, AST objects end up taking a significant portion
// of the memory used by pnut since, as things are currently implemented, some
// AST nodes are kept alive until the end of the compilation, preventing the
// reuse of the memory allocated for them.
//
// The typedef problem, where an identifier can be either a type or a variable
// depending on the context, is solved using the lexer hack, where the parser
// informs the tokenizer of new typedefs as they are encountered by changing the
// symbol kind of identifiers so they are recognized as type identifiers instead
// of regular identifiers.
// See https://en.wikipedia.org/wiki/Lexer_hack
//
// Relevant functions:
//  - parse_declaration(): parses a top-level declaration, function or variable
//  - parse_expression(): entry point for parsing an expression
//  - parse_statement(): entry point for parsing a statement
//
#ifdef target_sh
//############################ Shell Code Generator ############################
//
// POSIX shell allows forward references to functions and variables that are not
// yet defined or declared are allowed. This simplifies the implementation of a
// one-pass compiler as it eliminates the need for many fixups and indirections.
// As a result, the challenges in implementing a POSIX shell code generator come
// from other constraints, such as generating human-readable code. To be
// human-readable, the shell code must be appropriately indented and shims that
// are introduced by the shell code generation must be seamlessly integrated
// with the rest of the code.
//
// Pnut-sh compiles the statements of functions one after the other, each
// mapping to a few lines of shell code. Constructing each line of shell code is
// heavily dependent on string concatenation, which can result in quadratic time
// and memory complexity if implemented naively. To solve this problem, pnut-sh
// uses a tree-like data structure to represent string conversions and
// concatenation, similar to the rope data structure. Generally, conversions of
// strings, such as escaping or formatting numbers (decimal, hexadecimal and
// octal), are delayed as the conversion would expand their size. To further
// reduce memory usage, the data structure also differentiates between immutable
// strings, which come from the string intern pool and string literals, and
// mutable strings, which must be copied.
//
// Relevant functions and variables (rope data structure):
//  - text_pool: flat int array that stores the text nodes
//  - enum TEXT_NODES: defines the different types of text nodes
//  - string_concat(): concatenates two text nodes
//  - wrap_str_imm(): wraps an immutable string in a text node
//  - wrap_int(): wraps an integer in a text node
//  - escape_text(): escapes the characters of a text node
//  - print_text(): prints a text node to stdout
//
// A second data structure keeps track of the lines that have been generated and
// ensures they are properly indented. At specific points in the generated code,
// placeholders can be inserted to allow the insertion of the required shims
// once the function declaration has been fully processed. These shims are used
// to implement the function prologue and epilogue, which appear at the
// beginning and end of the function, but also wherever the function returns
// early. The data structure also allows lines to be removed, copied, and
// merged, and can be queried to count the number of lines between two points,
// useful for generating syntactically valid shell code as empty blocks are
// disallowed.
//
// Relevant functions and variables (declaration lines):
//  - glo_decls: stores the text nodes of the declaration being compiled
//  - nest_level: current nesting level, used for indentation
//  - append_glo_decl(): appends text node to the current declaration
//  - append_glo_decl_fixup(): appends placeholder, returns its id for later use
//  - print_glo_decls(): prints lines of the current declaration to stdout
//
// Both data structures are stored in preallocated buffers (text_pool and
// glo_decls) that are reset and reused between each top-level declaration.
// Reusing these buffers reduces total memory usage by more than half, which has
// a significant impact on the performance on the slowest shell implementations.
//
// Relevant functions and variables (code generator):
//  - codegen_begin(): initialization of code generator (called once)
//  - codegen_end(): finalization of code generator (called once)
//  - codegen_glo_decl(): entry point for compiling top-level declaration
//
// statements:
//  - enum STMT_CTX: current statement compilation context (else if, switch)
//  - comp_glo_fun_decl(): compiles a function declaration
//  - comp_glo_var_decl(): compiles a global variable declaration
//  - comp_statement(): entry point for compiling a statement
//  - comp_body(): compiles a scoped block (function/loop/conditional {...})
//
// expressions:
//  - enum VALUE_CTX: current expression compilation context ($((..)), cond)
//  - comp_rvalue(): entry point for compiling a C expression
//  - comp_lvalue(): entry point for compiling a C lvalue
//  - handle_side_effects(): lifts and handles side effects of a C expression
//  - character_ident(): map characters to their corresponding variable names
//  - fresh_string_ident(): map string literals to a unique variable names
//  - fresh_ident(): generates a fresh variable name for temporary variables
//
// environment:
//  - main_defined: tracks if main() is defined, to generate the entry point
//  - in_tail_position: tracks if the current statement is in tail position
//  - rest_loc_var_fixups: list of `endlet {local variables}` fixups
//  - cgc_fs: Number of local variables defined in the current function
//  - cgc_locals_fun: function-scoped environment
//  - cgc_globals: globally-scoped environment
//
// To avoid generating dead code, special care was taken to include only the
// parts of the runtime library referenced by the shell code. To do so, calls to
// runtime library functions are tracked. This reduces the amount of code
// generated, which in turn reduces compilation time and bloat of the generated
// scripts.
//
// Relevant functions and variables (runtime library):
//  - runtime_use_{rt_function}: if the runtime function is used by program
//  - runtime_{rt_function}_defined: if implementation has already been printed
//  - runtime_{rt_function}(): prints implementation of the runtime function
//  - produce_runtime(): prints implementations of required runtime functions
//
// The majority of the runtime library code are printf calls outputting a
// constant string. Since POSIX shell already includes a printf built-in
// function, it is often possible to reuse the shell's built-in printf if the
// format string is known at compile time. By doing so, the conversion of the
// format string to a C string can be avoided, and the string formatting is done
// directly by the shell. The result is a shorter and faster shell script, as
// the printf implementation is not needed.
//
// Relevant functions (printf optimization):
//  - printf_call(): converts C printf() to shell printf()
//  - comp_putchar_inline(): converts C putchar() to shell printf()
//
#endif
#if defined(target_i386_linux) || defined (target_x86_64_linux) || defined (target_x86_64_mac)
//########################### Machine Code Generator ###########################
//
// TODO
//
#endif
//-------------------- Here begins the actual implementation -------------------
