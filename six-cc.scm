#!/usr/bin/env gsi

(define support-addr-of? #f)
; Determines how to return a value from a function call.
; Possible values:
; - '(variable var_name): Each function returns in a variable called {var_name} and the caller must save it if needed.
; - '(addr always-pass): Functions take an extra parameter (always or when assigning) that is the name of the variable where to store the return value.
; (define function-return-method '(variable 0result))
(define function-return-method '(addr #t))
; Useful for debugging only
(define disable-save-restore-vars? #f)
; Disable for faster execution of programs that allocate a lot of memory
(define initialize-memory-when-alloc? #t)
; free function is noop or unsets the variables?
; Some shell implementations get significantly slower when the environment is large
; so it can be useful to have free be a NOOP or have it unset variables depending
; of the shell that's used.
; Fast shells: ksh, bash
; Slow shells: dash, zsh
(define free-unsets-variables? #t)
; Inline += -= operations?
(define inline-inplace-arithmetic-ops #t)
; Local variables start with _?
(define prefix-local-vars #f)
; Unpack strings where they are used instead of at program initialization
; This can slow down programs because the initialization may be done multiple times
(define inline-string-init #t)
; If it's the caller or callee that should save local variables
(define callee-save? #t)
; Always use arithmetic expansion to do if/while conditions?
(define arithmetic-conditions? #t)
; Always use arithmetic expansion to do assignments?
; Warning: Assignment using arithmetic expansion doesn't overflow while regular
; assignment to an overflowing expression does in ksh. The overflow breaks c4's
; hashing algorithm, and prevents it from compiling itself.
; ** Do not use this option if you want to compile c4 with c4 **
(define arithmetic-assignment? #f)

(define (function-name ident)
  (string-append "_" (symbol->string (cadr ident))))

(define-type ctx
  glo-decls
  loc-env
  all-variables ; Set represented as a table
  literals      ; Literals that have been assigned to a variable
  data          ; The data section
  level
  tail?)

(define (ctx-add-glo-decl! ctx decl)
  (ctx-glo-decls-set! ctx (cons (cons decl (ctx-level ctx)) (ctx-glo-decls ctx))))

(define (nest-level ctx f)
  (ctx-level-set! ctx (+ (ctx-level ctx) 1))
  (f)
  (ctx-level-set! ctx (- (ctx-level ctx) 1)))

(define-macro (nest ctx . body)
  `(nest-level ,ctx (lambda () ,@body)))

(define-type local-var
  position
  initialized)

(define (shift-ctx-loc-env-position ctx)
  (for-each (lambda (p)
              (if (local-var-position (cdr p))
                (local-var-position-set! (cdr p) (+ 1 (local-var-position (cdr p))))))
            (table->list (ctx-loc-env ctx))))

(define (mark-ctx-loc-env-initialized ctx ident)
  (let* ((env (ctx-loc-env ctx))
         (loc (table-ref (ctx-loc-env ctx) ident #f)))
    (if loc
      (local-var-initialized-set! loc #t))))

(define (add-new-local-var ctx ident initialized #!optional (position #f))
  (table-set! (ctx-loc-env ctx)
              ident
              (make-local-var
                (or position (+ 1 (table-length (ctx-loc-env ctx))))
                initialized))
  ; Gather the list of all local variables.
  ; Used to initialize them all at the beginning of the execution so
  ; save_loc_var doesn't crash when saving an unitialized variable.
  ; result_loc is always initialized, so we don't need to add it to the list.
  (if (not (equal? (cadr ident) 'result_loc))
    (table-set! (ctx-all-variables ctx) ident '())))

(define (is-local-var ctx ident)
  (table-ref (ctx-loc-env ctx) ident #f))

(define (env-var ctx ident #!optional (prefixed-by-dollar #f))
  (cond
    ((number? (cadr ident)) ; Number identifiers are always local and don't appear in the local environment
      (string-append (if (not prefixed-by-dollar) "$" "") (number->string (cadr ident))))
    ((equal? '_ (cadr ident))
      "_")
    ((table-ref (ctx-loc-env ctx) ident #f)
      (format-var ident))
    (else
      (global-ref ident))))

(define (global-var ident)
  (string-append "_" (symbol->string (cadr ident))))

(define (global-ref ident)
  (if support-addr-of?
      (string-append "_$" (global-var ident))
      (global-var ident)))

(define (format-var ident)
  (if prefix-local-vars
    (string-append "_" (symbol->string (cadr ident)))
    (symbol->string (cadr ident))))

(define (obj-ref ident)
  (string-append "__" (number->string ident)))

(define (def_str_code var_str str)
  (let ((escaped-str (escape-string str)))
    (string-append
      "defstr "
      var_str
      " \"" escaped-str "\"")))

(define (comp-glo-decl ctx ast)
  (case (car ast)

    ((six.define-variable)
     (comp-glo-define-variable ctx ast))

    ((six.define-procedure)
      (case (cadr (caddr ast)) ; Match on the return type to see if struct/enum def
        ((enum)
          (comp-glo-define-enum ctx ast))
        ((struct)
          (error "Structures not yet supported"))
        (else
          (comp-glo-define-procedure ctx ast))))
    (else
     (error "unknown global declaration" ast))))

(define (comp-constant ctx ast)
  (case (car ast)
    ((six.literal)
     (let ((val (cadr ast)))
       (cond ((exact-integer? val)
              (number->string val))
             (else
              "unknown literal" ast))))
    (else
     (error "unknown constant" ast))))

(define (comp-glo-define-variable ctx ast)
  (let* ((name (cadr ast))
         (type (caddr ast))
         (dims (cadddr ast))
         (init (car (cddddr ast))))
    (if (pair? dims)
        (let ((size
               (apply * (map (lambda (x) (cadr x)) dims))))
          (ctx-add-glo-decl!
           ctx
           (list "defarr " (global-var name) " " size)))
        (let ((val (if init (comp-constant ctx init) "0")))
          (ctx-add-glo-decl!
           ctx
           (if support-addr-of?
               (list "defglo_pointable " (global-var name) " " val)
               (list "defglo " (global-var name) " " val)))))))

; An enum is defined with a function without parameters returning a value of type `enum`
; and that has a body containing only assignments or identifiers.
; Here's an example: enum MyEnum() { A = 128; B; C; }
(define (comp-glo-define-enum ctx ast)
  (let* ((name (cadr ast))
         (proc (caddr ast))
         (parameters (caddr proc))
         (body (cdar (cdddr proc)))
         (enum-value-counter 0))
    (if (not (null? parameters))
      (error "enums must have no parameters" proc))

    ; Simple comment
    (ctx-add-glo-decl!
      ctx
      (list "# Defining enum " (cdr name)))

    (ctx-add-glo-decl!
      ctx
      (map
        (lambda (decl)
          (case (car decl)
            ((six.x=y)
              (let ((new-value (cadr (caddr decl))))
                (set! enum-value-counter (+ new-value 1))
                (list "_" (cdadr decl) "=" (number->string new-value) " ")))

            ((six.identifier)
              (set! enum-value-counter (+ enum-value-counter 1))
              (list "_" (cdr decl) "=" (number->string (- enum-value-counter 1)) " "))

            (else
              (error "Enum definition can only contain identifiers or assigments. Got:" decl))))
        body))

    (ctx-add-glo-decl! ctx '())))

(define reserved-variable-prefix
  (map symbol->string
  '(result_loc   ; Variable storing the variable name where to return the value from a function call
    str_         ; Prefix of variables used to store the address of a string literals
    ; Special variable for zsh. If we initialize the local variables before
    ; handling $# $@, writing to argv will overwrite the arguments those values.
    argv
    ; Variables storing the argc/argv during program initialization (before main function call)
    argc_for_main
    argv_for_main
    ; Determine if malloc initializes the memory it allocates to 0
    STRICT_MODE
    ; Determine if malloc initializes the memory it allocates to 0
    FREE_UNSETS_VARS
    ; Runtime library variables
    strict_alloc
    make_argv
    push_data
    unpack_array
    unpack_string
    pack_string
    print_string
    char_to_int
    int_to_char
    defstr
    ; Primitives
    malloc
    printf
    open
    read
    close
    read_n_char
    fopen
    fclose
    fread
    fgetc
    read_all_char
    get_char
    memset
    memcmp
    show_heap
    show_arg_stack
    show_fd
    )))

(define (is-name-reserved name)
  (any (lambda (prefix) (string-prefix? prefix name)) reserved-variable-prefix))

(define (assert-variable-name-is-safe name)
  (if (and (not prefix-local-vars) (string-prefix? "_" (symbol->string (cadr name))))
    (error "Variable name can't start with _:" (cadr name))
    (if (is-name-reserved (symbol->string (cadr name)))
      (error "Variable name is reserved:" (cadr name)))))

(define (assert-variable-names-are-safe names)
  (for-each assert-variable-name-is-safe names))

(define (get-body-declarations lst)
  (let loop ((lst lst) (new-local-vars '()))
      (if (and (pair? lst) (eq? (caar lst) 'six.define-variable))
        (let* ((def-var (car lst))
               (var-name (cadr def-var))
               (var-init (car (cddddr def-var))))
          (loop (cdr lst) (cons (cons var-name var-init) new-local-vars)))
        (cons lst (reverse new-local-vars)))))

(define (comp-glo-define-procedure ctx ast)
  (let* ((name (cadr ast))
         (proc (caddr ast))
         (result-type (cadr proc))
         (parameters (caddr proc))
         (body (cdar (cdddr proc))))
    (ctx-add-glo-decl!
     ctx
     (list (function-name name) "() {"))
    (ctx-tail?-set! ctx #t)
    (nest ctx
      (let* ((start-loc-env (table-copy (ctx-loc-env ctx)))
             (body-decls (get-body-declarations body))
             (body-rest (car body-decls))
             (new-local-vars (cdr body-decls)))

        (assert-variable-names-are-safe (map car parameters)) ; Parameters are always initialized
        (assert-variable-names-are-safe (map car new-local-vars))

        (for-each
          (lambda (param pos) (add-new-local-var ctx (car param) #t pos))
          parameters
          (iota (length parameters) 1))

        ; Add local variables to the environment
        (for-each
          (lambda (param) (add-new-local-var ctx (car param) (cdr param)))
          new-local-vars)

        (if (equal? 'addr (car function-return-method))
          (begin
            (shift-ctx-loc-env-position ctx) ; Make room for the result_loc var
            (add-new-local-var ctx '(six.identifier result_loc) #t 1)
            (if callee-save? (save-local-variables ctx))
            (if (cadr function-return-method) ; return address is always passed?
              (begin
                (ctx-add-glo-decl!
                  ctx
                  (list (env-var ctx '(six.identifier result_loc))
                        "=\"$1\"; shift "
                        "# Remove result_loc param")))
              (begin
                (ctx-add-glo-decl!
                  ctx
                  (list "if [ $# -eq " (+ 1 (length parameters)) " ]; "
                        "then " (env-var ctx '(six.identifier result_loc)) "=$1; shift ; "
                        "else " (env-var ctx '(six.identifier result_loc)) "= ; fi ;")))))
          (begin
            (if callee-save? (save-local-variables ctx))))

        (let ((local-vars-to-map
                (map cons parameters (iota (length parameters) 1))))

          (for-each
            (lambda (p) (comp-assignment ctx `(six.x=y (six.identifier ,(cadaar p)) (six.identifier ,(cdr p)))))
            local-vars-to-map))

        (for-each
          (lambda (new-local-var)
            (if (cdr new-local-var)
              (comp-assignment ctx `(six.x=y ,(car new-local-var) ,(cdr new-local-var)))
              (comp-assignment ctx `(six.x=y ,(car new-local-var) (six.literal 0)))))
          new-local-vars)

      (comp-body ctx body-rest)

      (if (and callee-save?
               (not (equal? (car (last body-rest)) 'six.return)))
        (restore-local-variables ctx))

      (ctx-loc-env-set! ctx start-loc-env)))

    (ctx-add-glo-decl!
     ctx
     (list "}"))))

(define (comp-body ctx lst)
  (let* ((start-loc-env (table-copy (ctx-loc-env ctx)))
         (body-decls (get-body-declarations lst))
         (body-rest (car body-decls))
         (new-local-vars (cdr body-decls)))

    ; This limitation is a arbitrary and simply to make it easier to
    ; save/restore local variables.
    ; One way to solve this would be to traverse the body AST and find all local
    ; variable declared and save them at the beginning of the function, but
    ; traversing the AST multiple times may complicate the C implementation.
    (if (not (null? new-local-vars))
      (error "Variables must be defined at the top of the function"))

    (for-each
      (lambda (new-local-var)
        (add-new-local-var ctx (car new-local-var) (cdr new-local-var))

        (if (cdr new-local-var)
          (comp-assignment ctx `(six.x=y ,(car new-local-var) ,(cdr new-local-var)))
          (comp-assignment ctx `(six.x=y ,(car new-local-var) (six.literal 0)))))
      new-local-vars)

    (comp-statement-list ctx body-rest)

    ; Bug: We need to propagate the is_initialized flag of local variables to start-loc-env?
    (ctx-loc-env-set! ctx start-loc-env)))

(define (comp-statement-list ctx lst)
  (let ((start-tail? (ctx-tail? ctx)))
    (ctx-tail?-set! ctx #f)
    (let loop ((lst lst))
      (if (pair? lst)
          (begin
            (if (not (pair? (cdr lst)))
                (ctx-tail?-set! ctx start-tail?))
            (comp-statement ctx (car lst))
            (loop (cdr lst)))
          (ctx-tail?-set! ctx start-tail?)))))

(define (comp-statement ctx ast #!optional (else-if? #f))
  (case (car ast)
    ((six.while)
     (if #t ;; simple-expression
         (let ((code-test (comp-loop-test ctx (cadr ast))))
           (ctx-add-glo-decl!
            ctx
            (list "while " code-test " ; do"))
           (nest ctx (comp-statement ctx (caddr ast)))
           (ctx-add-glo-decl!
            ctx
            (list "done")))
         (error "complex expression not yet supported")))
    ((six.for)
     ;;(six.for (six.x=y (six.identifier i) (six.literal 0))     (six.x<y (six.identifier i) (six.literal 2800))    (six.x++ (six.i...
     (let ((expr1 (cadr ast))
           (expr2 (caddr ast))
           (expr3 (cadddr ast))
           (stat (car (cddddr ast))))
       (comp-statement ctx expr1)
       ;; TODO: support "continue"
       (let ((code-test (if expr2 (comp-loop-test ctx expr2) ":")))
         (ctx-add-glo-decl!
          ctx
          (list "while " code-test " ; do"))
         (nest ctx
          (comp-statement ctx stat)
          (if expr3 (comp-statement ctx expr3))
         )
         (ctx-add-glo-decl!
          ctx
          (list "done")))))
    ((six.if)
     (let ((test (cadr ast))
           (stat (caddr ast)))
       (let ((code-test (comp-if-test ctx test)))
         (ctx-add-glo-decl!
            ctx
            (list (if else-if? "elif " "if ") code-test " ; then"))
         (nest ctx (comp-statement ctx stat))
         (if (pair? (cdddr ast))
             (let ((else-block (cadddr ast)))
               (if (equal? (car else-block) 'six.if)
                   (begin
                    ; Next statement is if, so compiling to elif
                    (comp-statement ctx else-block #t))
                   (begin
                    ; Normal case
                    (ctx-add-glo-decl!
                      ctx
                      (list "else"))
                    (nest ctx (comp-statement ctx else-block))))))
         (if (not else-if?)
          (ctx-add-glo-decl!
            ctx
            (list "fi"))))))
    ((six.return)
     (if (pair? (cdr ast))
         (let ((code-expr (comp-rvalue ctx (cadr ast) '(return))))
              (case (car function-return-method)
                ((variable)
                (ctx-add-glo-decl!
                  ctx
                  (list "_" (symbol->string (cadr function-return-method)) "=$((" code-expr "))")))
                ((addr)
                (ctx-add-glo-decl!
                  ctx
                  (list "prim_return_value $((" code-expr ")) $" (env-var ctx '(six.identifier result_loc)))))
                (else
                  (error "Unknown value return method" function-return-method)))))
    ; Seems broken in while loops
    ;  (if (not (ctx-tail? ctx))
     (if callee-save? (restore-local-variables ctx))
     (ctx-add-glo-decl! ctx (list "return")))
    ((six.break)
     (ctx-add-glo-decl!
      ctx
      (list "break")))
    (else
     (comp-statement-expr ctx ast))))

(define (comp-if-test ctx ast)
  (let* ((rvalue (comp-rvalue ctx ast '(test-if))))
  (if arithmetic-conditions?
    (string-append
      "[ 0 != "
      rvalue
      " ]")
    rvalue)))

(define (comp-loop-test ctx ast)
  (let* ((rvalue-res (comp-rvalue ctx ast '(test)))
         (rvalue (car rvalue-res))
         (pre-side-effects (comp-side-effects ctx (cdr rvalue-res))))

    (string-append
      (apply string-append (map (lambda (e) (string-append e "; ")) pre-side-effects))

      (if arithmetic-conditions?
        (string-append
          "[ 0 != "
          rvalue
          " ]")
        rvalue))))

; Primitives from the runtime library, and if they return a value or not.
(define runtime-primitives
  '((putchar #f)
    (exit #f)
    (malloc #t)
    (free #f)
    (printf #f)
    (open #t)
    (read #t)
    (close #t)
    (fopen #t)
    (fclose #t)
    (fread #t)
    (fgetc #t)
    (memset #t)
    (memcmp #t)
    (show_heap #f)))

; Remove the variable we are assigning to from the list of local variables to save.
; Also, saving and restoring parameters on every function call will likely make the code harder to read and slower.
; Some ideas on how to reduce the number of variables saved:
;  [x] Only save the variables that have been written to/initialized.
;  [ ] Only save the variables that are used by the function, or used by functions called transitively.
;     - [x] Primitives
;     - [ ] For user-defined functions
;  [ ] Only restore variables lazily, so consecutive function calls don't save and restore needlessly.
;      This doesn't seem to be worth its complexity, and consecutive function calls may not be that common.
;      Note: on branch laurent/six-cc-lazy-var-restore.
;  [ ] Use a smarter data structure, so we can save and restore variables in different orders.
;  [ ] Use dynamic variables only ( $((_$i_{var_name})) ), with a counter that we increment on each function call so we don't need to save and restore them. The counter could be $1, since it's scoped locally and doesn't need to be restored
(define (save-local-variables ctx #!optional (assign_to #f))
  (let* ((sorted-local-vars
          (list-sort (lambda (v1 v2) (< (local-var-position (cdr v1)) (local-var-position (cdr v2)))) (table->list (ctx-loc-env ctx))))
         (initialized-local-vars
          (if callee-save? ; If it's callee-save, we have to save all variables, even uninitialized ones
            (map car sorted-local-vars)
            (map car (filter (lambda (l) (local-var-initialized (cdr l))) sorted-local-vars))))
         (local-vars-to-save
          (filter (lambda (x) (not (member x (list assign_to)))) initialized-local-vars))
         (local-vars-to-save-no-result-loc
          (filter (lambda (x) (not (member x (list assign_to '(six.identifier result_loc))))) initialized-local-vars)))

      ; All primitives uses variables not starting with _, meaning that there can't be a conflicts
      (if (and (not disable-save-restore-vars?) (not (null? local-vars-to-save)))
        (ctx-add-glo-decl!
          ctx
          (list "save_loc_var " (string-concatenate (reverse (map (lambda (l) (env-var ctx l)) local-vars-to-save-no-result-loc)) " "))))))

(define (restore-local-variables ctx #!optional (assign_to #f))
  (let* ((sorted-local-vars
          (list-sort (lambda (v1 v2) (< (local-var-position (cdr v1)) (local-var-position (cdr v2)))) (table->list (ctx-loc-env ctx))))
         (initialized-local-vars
          (if callee-save? ; If it's callee-save, we have to save all variables, even uninitialized ones
            (map car sorted-local-vars)
            (map car (filter (lambda (l) (local-var-initialized (cdr l))) sorted-local-vars))))
         (local-vars-to-save
          (filter (lambda (x) (not (member x (list assign_to)))) initialized-local-vars))
         (local-vars-to-save-no-result-loc
          (filter (lambda (x) (not (member x (list assign_to '(six.identifier result_loc))))) initialized-local-vars)))

      (if (and (not disable-save-restore-vars?) (not (null? local-vars-to-save)))
        (ctx-add-glo-decl!
          ctx
          (list "rest_loc_var " (string-concatenate (map (lambda (l) (env-var ctx l)) local-vars-to-save-no-result-loc) " "))))))

(define (comp-fun-call ctx ast #!optional (assign_to #f))
  ; There are many ways we can implement function calls. There are 2 axis for the calling protocol:
  ; - how to preserve value of variables that may be used by the caller (because no local variables).
  ;   Options:
  ;    - Caller saves all of its variable and restore them after the call (what we do for now).
  ;    - Caller saves all variables that may be used by the callee. (requires an analysis of the functions and transitive closure).
  ;    - Have every variable on a stack.
  ;    - Let the C code handle the problem
  ;    Optimization ideas:
  ;    - Exploit how $1, $2, ... are scoped locally to the function, and are restored after a function call.
  ; - how to pass the return value.
  ;   Options:
  ;    - Each function returns in a variable called {function_name}_result and the caller must save it if needed.
  ;    - Functions take an extra parameter that is the address/name of the variable where to store the return value.
  (let* ((name (car ast))
         (params (cdr ast))
         (func-call-params (filter (lambda (p) (equal? (car p) 'six.call)) params))
         (code-params
          (map (lambda (p)
                  (let* ((params-after (or (member p func-call-params) '()))
                         (params-after-count (max 0 (- (length params-after) 1))))
                    (comp-rvalue ctx p `(argument ,params-after-count))))
               params))
         (call-code
          (string-concatenate (cons (function-name name) code-params) " "))
         (is-prim
          (member (cadr name) (map car runtime-primitives)))
         (can-return
          (if is-prim
              (cadr (assoc (cadr name) runtime-primitives))
              #t)))

      (if (and (not can-return) assign_to)
        (error "Function call can't return a value, but we are assigning to a variable"))

      ; All primitives uses variables not starting with _, meaning that there can't be a conflicts
      (if (and (not is-prim) (not callee-save?)) (save-local-variables ctx assign_to))

      (case (car function-return-method)
        ((variable)
          (ctx-add-glo-decl! ctx (list call-code))
          (if assign_to
            (comp-assignment ctx `(six.x=y ,assign_to (six.identifier ,(cadr function-return-method))))))
        ((addr)
          (if (and (or assign_to (cadr function-return-method)) can-return) ; Always pass the return address, even if it's not used, as long as the function can return a value
            (ctx-add-glo-decl!
              ctx
              (list (string-concatenate (cons (function-name name)
                                          (cons (env-var ctx (or assign_to '(six.identifier _)))
                                          code-params))
                                        " ")))
          (ctx-add-glo-decl! ctx (list call-code))))
        (else
          (error "Unknown value return method" function-return-method)))

      ; All primitives uses variables not starting with _, meaning that there can't be a conflicts
      (if (and (not is-prim) (not callee-save?)) (restore-local-variables ctx assign_to))))

(define (comp-statement-expr ctx ast)
  (case (car ast)
    ((six.call)
     (comp-fun-call ctx (cdr ast)))
    ((six.compound)
     (comp-body ctx (cdr ast)))
    ((six.x=y)
     (comp-assignment ctx ast))
    (else
     ; Since we don't assign the result of the expression and comp-rvalue does the side-effect, we can drop the result of comp-rvalue
     (comp-rvalue ctx ast '(statement-expr)))))

(define (comp-glo-decl-list ctx ast)
  (if (pair? ast)
      (let ((decl (car ast)))
        (if (eq? (car decl) 'six.x=y) ;; allow assignments at top level
            (comp-assignment ctx decl)
            (comp-glo-decl ctx decl))
        (comp-glo-decl-list ctx (cdr ast)))))

(define (comp-assignment ctx ast)
  (let ((lhs (cadr ast))
        (rhs (caddr ast)))
    (case (car lhs)
      ((six.identifier six.index six.*x six.**x)
       (case (car rhs)
        ((six.call)
          (comp-fun-call ctx (cdr rhs) lhs))
        (else
          (let* ((simple-assignment (and (equal? (car lhs) 'six.identifier) arithmetic-assignment?))
                 (rvalue-res (comp-rvalue ctx rhs `(assignment ,(not simple-assignment))))
                 (rvalue (car rvalue-res))
                 (pre-side-effects (comp-side-effects ctx (cadr rvalue-res)))
                 (literal-inits (caddr rvalue-res)))
          (for-each
            (lambda (p) (ctx-add-glo-decl! ctx p))
            pre-side-effects)
          (comp-literal-inits ctx literal-inits)
          (if simple-assignment
            (ctx-add-glo-decl!
              ctx
              (list (comp-lvalue ctx lhs) "=" rvalue))
            (ctx-add-glo-decl!
              ctx
              (list ": $(( " (comp-lvalue ctx lhs) " = " rvalue " ))")))
              )))

       ; Mark the variable as used written to, used to determine if we need to save and restore it
       ; We could do a much smarter lifetime analysis, where we also determine when the variable becomes dead,
       ; but this is simple and works for now.
       (if (is-local-var ctx lhs)
        (mark-ctx-loc-env-initialized ctx lhs))) ; Mark local var as initialized
      (else
       (error "unknown lhs" lhs)))))

(define (comp-lvalue ctx ast)
  (case (car ast)
    ((six.identifier)
     (env-var ctx ast))
    ((six.index)
     (string-append "_$((" (comp-array-lvalue ctx (cadr ast)) "+" (comp-rvalue ctx (caddr ast) '(index-lvalue)) "))"))
    ((six.*x)
     ; Not sure if comp-rvalue is right here
     (string-append "_$((" (comp-rvalue ctx (cadr ast) '(index-lvalue)) "))"))
    ((six.**x)
     ; Not sure if comp-rvalue is right here
     (string-append "_$(( _$((" (comp-rvalue ctx (cadr ast) '(index-lvalue)) ")) ))"))
    (else
     (error "unknown lvalue" ast))))

(define (comp-array-lvalue ctx ast)
  (case (car ast)
    ((six.identifier)
     (env-var ctx ast))
    ((six.*x)
     (string-append "_$((" (comp-rvalue ctx (cadr ast) '(index-lvalue)) "))"))
    (else
     (error "unknown array lvalue" ast))))

; We can't have function calls and other side effects in $(( ... )), so we need to handle them separately.
; For function calls, this function replaces them with unique identifiers and returns a list of the replaced function calls and their new identifiers.
; For pre/post-increments/decrements, we map them to a pre-side-effect and replace with the corresponding operation.
; Note that pre/post-increments/decrements of function calls are not supported.
(define (handle-side-effects ctx ast)
  (define replaced-fun-calls '())
  (define pre-side-effect '())
  (define literal-inits (make-table))
  (define contains-side-effect #f)
  (define (alloc-identifier)
    (let ((id (gensym)))
      (list 'six.identifier id)))

  (define (go-inplace-arithmetic-op assigned-op inplace-arith #!optional (side-effect #f))
    (if inline-inplace-arithmetic-ops
      (begin
        (set! contains-side-effect #t)
        inplace-arith)
      (begin
        (set! pre-side-effect (cons (or side-effect inplace-arith) pre-side-effect))
        assigned-op)))

  (define (go ast)
    (case (car ast)
      ((six.call)
        (let* ((id (alloc-identifier))
               (fun-name (car ast))
               (fun-params (cdr ast))
               (replaced-params (map go fun-params))
               (new-fun-call (cons fun-name replaced-params)))
          (set! replaced-fun-calls (cons (cons id new-fun-call) replaced-fun-calls))
          id))
      ((six.literal)
        (let ((val (cadr ast)))
          (if (and inline-string-init
                  (string? val)
                  (not (equal? 1 (string-length val)))) ; We don't inline single character strings because those are interpreted as characters
            (let* ((string-variable (string->variable-name ctx val))
                   (table-val (table-ref literal-inits string-variable #f)))
              ; FIXME: If a string is passed to a function multiple times, this caching mechanism won't work because
              ; function arguments are processed separately.
              (if table-val
                (if (equal? val table-val)
                  string-variable
                  (error "Two different string literals map to same variable" string-variable val))
                (begin
                  (table-set! literal-inits string-variable val)
                  string-variable))
              )
            ast)))
      ((six.list six.identifier six.-x)
        ast)
      ((six.x+y six.x-y six.x*y six.x/y six.x%y six.x==y six.x!=y six.x<y six.x>y six.x<=y six.x>=y six.x>>y six.x<<y six.x&y |six.x\|y| six.x^y six.x&&y |six.x\|\|y| six.index six.x=y)
        (list (car ast)
              (go (cadr ast))
              (go (caddr ast))))
      ((six.x?y:z)
        (list (car ast)
              (go (cadr ast))
              (go (caddr ast))
              (go (cadddr ast))))
      ((six.!x six.*x six.**x)
        (list (car ast)
              (go (cadr ast))))
      ((six.++x six.--x six.x++ six.x-- six.x+=y six.x-=y six.x*=y six.x/=y six.x%=y)
        (case (car ast)
          ((six.++x)
            (let ((new-op (go (cadr ast))))
              (go-inplace-arithmetic-op new-op
                                        `(six.++x ,new-op))))
          ((six.--x)
            (let ((new-op (go (cadr ast))))
              (go-inplace-arithmetic-op new-op
                                        `(six.--x ,new-op))))
          ((six.x++)
            ; We map post increments to a pre-increment and replace x++ with x - 1 to compensate
            (let ((new-op (go (cadr ast))))
              (go-inplace-arithmetic-op `(six.x-y ,new-op (six.literal 1))
                                        `(six.x-y (six.++x ,new-op) (six.literal 1))
                                        `(six.++x ,new-op))))
          ((six.x--)
            ; We map post decrements to a pre-decrement and replace x-- with x + 1 to compensate
            (let ((new-op (go (cadr ast))))
              (go-inplace-arithmetic-op `(six.x+y ,new-op (six.literal 1))
                                        `(six.x+y (six.--x ,new-op) (six.literal 1))
                                        `(six.--x ,new-op))))
          ((six.x+=y)
            (let ((new-op1 (go (cadr ast))) (new-op2 (go (caddr ast))))
              (go-inplace-arithmetic-op new-op1
                                        `(six.x+=y ,new-op1 ,new-op2))))
          ((six.x-=y)
            (let ((new-op1 (go (cadr ast))) (new-op2 (go (caddr ast))))
              (go-inplace-arithmetic-op new-op1
                                        `(six.x-=y ,new-op1 ,new-op2))))
          ((six.x*=y)
            (let ((new-op1 (go (cadr ast))) (new-op2 (go (caddr ast))))
              (go-inplace-arithmetic-op new-op1
                                        `(six.x*=y ,new-op1 ,new-op2))))
          ((six.x/=y)
            (let ((new-op1 (go (cadr ast))) (new-op2 (go (caddr ast))))
              (go-inplace-arithmetic-op new-op1
                                        `(six.x/=y ,new-op1 ,new-op2))))
          ((six.x%=y)
            (let ((new-op1 (go (cadr ast))) (new-op2 (go (caddr ast))))
              (go-inplace-arithmetic-op new-op1
                                        `(six.x%=y ,new-op1 ,new-op2))))))
      (else
        (error "unknown ast" ast))))

  (list (go ast)
        (reverse replaced-fun-calls)
        (reverse pre-side-effect)
        (table->list literal-inits)
        contains-side-effect))

(define (replace-identifier ast old new)
  (case (car ast)
    ((six.identifier)
      (if (equal? ast old)
          new
          ast))
    ((six.literal six.list)
      ast)
    ((six.call six.x+y six.x-y six.x*y six.x/y six.x%y six.x==y six.x!=y six.x<y six.x>y six.x<=y six.x>=y six.x>>y six.x<<y six.x&y |six.x\|y| six.x^y six.x&&y |six.x\|\|y| six.index six.x+=y six.x=y)
      (list (car ast)
            (replace-identifier (cadr ast) old new)
            (replace-identifier (caddr ast) old new)))
    ((six.x?y:z)
      (list (car ast)
            (replace-identifier (cadr ast) old new)
            (replace-identifier (caddr ast) old new)
            (replace-identifier (cadddr ast) old new)))
    ((six.x++ six.x-- six.++x six.--x six.!x six.*x six.**x)
      (list (car ast)
            (replace-identifier (cadr ast) old new)))
    (else
      (error "unknown ast identifier" ast))))

(define (comp-side-effects ctx side-effects)
  (define (comp-side-effect ast)
      (case (car ast)
        ((six.++x)
          (string-append ": $((" (comp-lvalue ctx (cadr ast)) " += 1 ))"))
        ((six.--x)
          (string-append ": $((" (comp-lvalue ctx (cadr ast)) " -= 1 ))"))
        ((six.x+=y)
          (string-append ": $((" (comp-lvalue ctx (cadr ast)) " += " (comp-rvalue ctx (caddr ast) '(pure)) "))"))
        ((six.x-=y)
          (string-append ": $((" (comp-lvalue ctx (cadr ast)) " -= " (comp-rvalue ctx (caddr ast) '(pure)) "))"))
        ((six.x*=y)
          (string-append ": $((" (comp-lvalue ctx (cadr ast)) " *= " (comp-rvalue ctx (caddr ast) '(pure)) "))"))
        ((six.x/=y)
          (string-append ": $((" (comp-lvalue ctx (cadr ast)) " /= " (comp-rvalue ctx (caddr ast) '(pure)) "))"))
        ((six.x%=y)
          (string-append ": $((" (comp-lvalue ctx (cadr ast)) " %= " (comp-rvalue ctx (caddr ast) '(pure)) "))"))
        (else
          (error "unknown side effect" ast))))

  (map comp-side-effect side-effects))

(define (comp-literal-inits ctx literal-inits)
  (define (comp-literal-init literal-init)
    (let ((id (car literal-init))
          (val (cdr literal-init)))
      (ctx-add-glo-decl! ctx (def_str_code (env-var ctx id) val))))
  (for-each comp-literal-init literal-inits))

(define (comp-rvalue ctx ast context-tag)
  (let* ((side-effects-res (handle-side-effects ctx ast))
         (new-ast (car side-effects-res))
         (fun-calls-to-replace (cadr side-effects-res))
         (pre-side-effects (caddr side-effects-res))
         (literal-inits (cadddr side-effects-res))
         (contains-side-effect (car (cddddr side-effects-res))))
    (define (comp-side-effects-then-rvalue wrapped)
      ; Optimization: When returning with the variable return method, we can
      ; use the return location directly when it's used by the next function
      ; call.
      ; Also, the return location can be returned directly by comp-rvalue when
      ; we know that it will be used directly after. This is normally true,
      ; except when the rvalue is a function call argument and the parent
      ; function call has multiple function call arguments. In that case, only
      ; the last function call argument can skip assigning to the return
      ; location.
      ; Note: This could maybe be done in a simpler way by moving this logic
      ; to comp-fun-call but it works for now.
      (if (and (equal? (car function-return-method) 'variable) (not (null? fun-calls-to-replace)))
        (let ((must-assign-to-return-loc (and (equal? (car context-tag) 'argument) (< 0 (cadr context-tag))))
              (ast-with-result-identifier
                (replace-identifier new-ast
                                    (car (last fun-calls-to-replace)) ; Location of last function call
                                    `(six.identifier ,(cadr function-return-method)))))
          (for-each
            (lambda (e) (ctx-add-glo-decl! ctx e))
            (comp-side-effects ctx pre-side-effects))
          (comp-literal-inits ctx literal-inits)
          (for-each
            (lambda (call nextCall)
              ; Check if next function call uses the result, if it's the case use the variable directly
              (let ((nextCallUsesReturnLoc (and nextCall (member (car call) (cddr nextCall)))))
                (if nextCallUsesReturnLoc
                  (begin
                    ; Overwrite the argument of next function call with the return location
                    (set-cdr! (car nextCallUsesReturnLoc) (cdr function-return-method))
                    (comp-fun-call ctx (cddr call)))
                  ; We only assign to the return location if it's not the
                  ; last function call. In that case, it's up to the caller
                  ; of comp-rvalue to read the value.
                  ; TODO: Add the return location to the list of local vars to save to support recursive function calls
                  (comp-fun-call ctx (cddr call) (and (or nextCall must-assign-to-return-loc) (car call))))))
            fun-calls-to-replace
            (cdr (reverse (cons #f (reverse fun-calls-to-replace)))))
          (if must-assign-to-return-loc
            (comp-rvalue-go ctx new-ast wrapped #f)
            (comp-rvalue-go ctx ast-with-result-identifier wrapped #f)))
        (begin
          (for-each
            (lambda (e) (ctx-add-glo-decl! ctx e))
            (comp-side-effects ctx pre-side-effects))
          (comp-literal-inits ctx literal-inits)
          (for-each
            (lambda (call) (comp-fun-call ctx (cddr call) (car call)))
            fun-calls-to-replace)
          (comp-rvalue-go ctx new-ast wrapped #f))))

    (case (car context-tag)
      ((return index-lvalue)
        (comp-side-effects-then-rvalue #t))
      ((argument)
        (comp-side-effects-then-rvalue #f))
      ((statement-expr)
        (if contains-side-effect
          (ctx-add-glo-decl!
            ctx
            (list ": $(( " (comp-side-effects-then-rvalue #t) " ))"))
          (comp-side-effects-then-rvalue #f))) ; Wrapped flag doesn't matter
      ((test)
        (if (null? fun-calls-to-replace)
          (cons (comp-rvalue-go ctx new-ast #f (not arithmetic-conditions?)) pre-side-effects)
          (error "comp-rvalue: Function calls not supported in condition" ast)))
      ((test-if)
        (for-each
          (lambda (e) (ctx-add-glo-decl! ctx e))
          (comp-side-effects ctx pre-side-effects))
        (comp-literal-inits ctx literal-inits)
        (for-each
          (lambda (call) (comp-fun-call ctx (cddr call) (car call)))
          fun-calls-to-replace)
        (comp-rvalue-go ctx new-ast #f (not arithmetic-conditions?)))
      ((assignment)
        (if (null? fun-calls-to-replace)
          (list (comp-rvalue-go ctx new-ast (cadr context-tag) #f) pre-side-effects literal-inits)
          (error "function calls in assignment not supported" ast)))
      ((pure)
        (if (and (null? fun-calls-to-replace) (null? pre-side-effects) (null? literal-inits))
          (comp-rvalue-go ctx new-ast #t #f)
          (error "function calls, side effects and string literals in pure context not supported" ast)))
      (else
        (error "unknown context tag" context-tag)))))

(define (six-op-string op #!optional (test-operand #f))
  (case op
    ((six.x+y) " + ")
    ((six.x-y) " - ")
    ((six.x*y) " * ")
    ((six.x/y) " / ")
    ((six.x%y) " % ")
    ((six.x>>y) " >> ")
    ((six.x<<y) " << ")
    ((six.x&y)  " & ")
    ((|six.x\|y|) " | ")
    ((six.x^y) " ^ ")
    ((six.x+=y) " += ")
    ((six.x-=y) " -= ")
    ((six.x*=y) " *= ")
    ((six.x/=y) " /= ")
    ((six.x%=y) " %= ")
    ((six.x=y)  " = ")
    ((six.x==y) (if test-operand " -eq " " == "))
    ((six.x!=y) (if test-operand " -ne " " != "))
    ((six.x<y)  (if test-operand " -lt " " < "))
    ((six.x>y)  (if test-operand " -gt " " > "))
    ((six.x<=y) (if test-operand " -le " " <= "))
    ((six.x>=y) (if test-operand " -ge " " >= "))
    ((six.x&&y)  " && ")
    ((|six.x\|\|y|)  " || ")
    (else
      (error "unknown six op" op))))

; TODO: Distinguish between wrapped in $(( ... )) and wrapped in ( ... )?
(define (comp-rvalue-go ctx ast wrapped compiling-test)
  (define (wrap-if-needed parens-otherwise? . lines)
    (if (not wrapped)
      (if compiling-test
        (string-append "[ $((" (apply string-append lines) ")) -ne 0 ]")
        (string-append "$((" (apply string-append lines) "))"))
      (if parens-otherwise?
        (string-append "(" (apply string-append lines) ")")
        (apply string-append lines))))

  ; Used to supports the case `if/while (c) { ... }`, where c is a variable.
  ; This is otherwise handled by wrap-if-needed, but we don't want to wrap
  ; in $(( ... )) here.
  (define (wrap-in-condition-if-needed . lines)
    (if compiling-test
      (string-append "[ " (apply string-append lines) " -ne 0 ]")
      (apply string-append lines)))

  (if (and wrapped compiling-test)
    (error "Can't be compiling in a test context and wrapped in $(( ... ))" ast))

  (case (car ast)
    ((six.literal)
     (let ((val (cadr ast)))
       (cond ((exact-integer? val)
              (wrap-in-condition-if-needed (number->string val)))
             ((string? val)
              ; Hacky way to detect character literals
              ; since six doesn't distinguish between " and '
              (if (equal? 1 (string-length val))
                (wrap-in-condition-if-needed (number->string (char->integer (string-ref val 0))))
                (begin
                  (ctx-data-set! ctx (cons val (ctx-data ctx)))
                  (wrap-in-condition-if-needed (if wrapped
                    (obj-ref (- (length (ctx-data ctx)) 1))
                    (string-append "$" (obj-ref (- (length (ctx-data ctx)) 1))))))))
             (else
              "unknown literal" ast))))
    ((six.list)
      (let loop ((lst ast) (acc '()))
        (if (and (pair? lst) (not (equal? 'six.null (car lst))))
          (if (and (equal? 'six.list (car lst)) (equal? 'six.literal (caadr lst)))
              (loop (caddr lst) (cons (cadadr lst) acc))
              (error "List elements must be literals" lst))
            (begin
              (ctx-data-set! ctx (cons (reverse acc) (ctx-data ctx)))
              (string-append "$" (obj-ref (- (length (ctx-data ctx)) 1)))))))
    ((six.identifier)
      (if wrapped
        (env-var ctx ast)
        (wrap-in-condition-if-needed "\"$" (env-var ctx ast arithmetic-assignment?) "\"")))
    ((six.x&&y |six.x\|\|y|)
      ; Note, this could also be compiled in a single [ ] block using -a and -o,
      ; which I think are POSIX compliant but are deprecated.
      (if compiling-test
        (let ((wrap-left (and (member (caadr ast) '(six.x&&y |six.x\|\|y|))
                              (not (equal? (car ast) (caadr ast)))))
              (wrap-right (and (member (caaddr ast) '(six.x&&y |six.x\|\|y|))
                                  (not (equal? (car ast) (caaddr ast))))))
          (string-append
                        ; Something very important to know is that the
                        ; precendence of && and || in Shell is equal, instead of
                        ; && grouping more tightly than || in C. This means that
                        ; some parenthesis that wouldn't be needed in C are needed.
                        ; As a simple heuristic, whenever the left or right hand
                        ; side of the expression is a different conditional, we
                        ; wrap it in parenthesis. It adds parenthesis that
                        ; sometimes aren't needed, but it hopefully helps readability.
                        (if wrap-left "(" "")
                        (comp-rvalue-go ctx (cadr ast) wrapped #t)
                        (if wrap-left ")" "")
                        (six-op-string (car ast) #t)
                        (if wrap-right "(" "")
                        (comp-rvalue-go ctx (caddr ast) wrapped #t)
                        (if wrap-right ")" "")))
        (wrap-if-needed #f
                        (comp-rvalue-go ctx (cadr ast) #t #f)
                        (six-op-string (car ast) #t)
                        (comp-rvalue-go ctx (caddr ast) #t #f))))
    ((six.x==y six.x!=y six.x<y six.x>y six.x<=y six.x>=y)
     (if compiling-test
         (string-append "[ "
                        (comp-rvalue-go ctx (cadr ast) wrapped #f)
                        (six-op-string (car ast) #t)
                        (comp-rvalue-go ctx (caddr ast) wrapped #f)
                        " ]")
         (wrap-if-needed #t
                        (comp-rvalue-go ctx (cadr ast) #t #f)
                        (six-op-string (car ast))
                        (comp-rvalue-go ctx (caddr ast) #t #f))))
    ((six.x+y six.x-y six.x*y six.x/y six.x%y six.x>>y six.x<<y six.x&y |six.x\|y| six.x^y)
     (wrap-if-needed #t
                    (comp-rvalue-go ctx (cadr ast) #t #f)
                    (six-op-string (car ast))
                    (comp-rvalue-go ctx (caddr ast) #t #f)))
    ((six.-x)
     ; Check if the rest of ast is a literal, if so directly return the negated value
     (if (and (equal? 'six.literal (caadr ast)) (exact-integer? (cadadr ast)))
       (wrap-if-needed #f (string-append (number->string (- (cadadr ast)))))
       (wrap-if-needed #f (string-append "-(" (comp-rvalue-go ctx (cadr ast) wrapped #f) ")"))))
    ((six.x?y:z)
      (wrap-if-needed #t
                      (comp-rvalue-go ctx (cadr ast) #t #f)
                      " ? "
                      (comp-rvalue-go ctx (caddr ast) #t #f)
                      " : "
                      (comp-rvalue-go ctx (cadddr ast) #t #f)))
    ((six.index)
     (wrap-if-needed #f "_$((" (comp-array-lvalue ctx (cadr ast)) "+" (comp-rvalue-go ctx (caddr ast) #t #f) "))"))
    ((six.++x)
     (wrap-if-needed #t (comp-lvalue ctx (cadr ast)) " += 1"))
    ((six.--x)
     (wrap-if-needed #t (comp-lvalue ctx (cadr ast)) " -= 1"))
    ((six.!x)
     ; This part could be improved by propagating the negation and applying
     ; De Morgan's law. For now, we can do the transformation by hand if we want
     ; to generate clearer code.
     ; Case where it would be useful: !(a == b && b == c) is compiled as
     ; [ $(( a == b && b == c)) -ne 0 ] instead of [ $a -ne $b ] [ $b -ne $c ].
     (wrap-if-needed #f "!" (comp-rvalue-go ctx (cadr ast) #t #f)))
    ((six.x+=y six.x-=y six.x*=y six.x/=y six.x%=y six.x=y)
     (wrap-if-needed #t (comp-lvalue ctx (cadr ast))
                        (six-op-string (car ast))
                        (comp-rvalue-go ctx (caddr ast) #t #f)))
    ((six.*x)
     (wrap-if-needed #f "_$((" (comp-rvalue-go ctx (cadr ast) #t #f) "))"))
    ((six.**x)
     (wrap-if-needed #f "_$(( _$((" (comp-rvalue-go ctx (cadr ast) #t #f) ")) ))"))
    ((six.x++)
     (error "x++ should have been replaced with x - 1 (and ++x side effect) by comp-rvalue"))
    ((six.x--)
     (error "x-- should have been replaced with (x + 1) (and --x side effect) by comp-rvalue"))
    ((six.call)
      (error "Function calls not supported in rvalue"))
    (else
     (error "unknown rvalue" ast))))

(define (comp-program ast)
  (let* ((ctx (make-ctx '() (make-table) (make-table) (make-table) '() 0 #f)))
    (comp-glo-decl-list ctx ast)
    (codegen ctx)))

(define (codegen ctx)
  (println (runtime-prelude ctx))
  (println (heap-prelude ctx))

  (for-each (lambda (decl)
              (let ((level (cdr decl)))
                (if (not (zero? level))
                    (print (make-string (* 2 level) #\space)))
                (println (car decl))))
            (reverse (ctx-glo-decls ctx)))
  (println (runtime-postlude)))

(define (unlines . lst)
  (string-concatenate (filter (lambda (p) (string? p)) lst) "\n"))

(define (runtime-prelude ctx)
  (define result_loc_var (format-var '(six.identifier result_loc)))
  (unlines
    "set -e -u"
    ""
    (if initialize-memory-when-alloc? "STRICT_MODE=1" "STRICT_MODE=0")
    (if free-unsets-variables? "FREE_UNSETS_VARS=1" "FREE_UNSETS_VARS=0")
    ""
    "# Load runtime library and primitives"
    ". ./runtime.sh"
    ""
    "# Local variables"
    ""
    "SP=0 # Note: Stack grows up, not down"
    ""
    "save_loc_var() {"
    (if (equal? 'addr (car function-return-method))
      (unlines
      "  : $((SP += 1))"
      ; "  unset \"save_loc_var_$SP\" # For some reason, ksh doesn't like to overwrite the value"
      "  # We must use eval to set a string to a dynamic variable"
      (string-append
      "  eval \"save_loc_var_$SP=\\$" result_loc_var "\"")))
    "  while [ $# -gt 0 ]; do"
    "    : $((SP += 1))"
    "    : $((save_loc_var_$SP=$1))"
    "    shift"
    "  done"
    "}"
    ""
    "rest_loc_var() {"
    (if (equal? 'addr (car function-return-method))
      (unlines
      "  while [ $# -gt 0 ]; do"
      "    # Make we we don't overwrite the result_loc"
      (string-append
      "    if [ $1 != \"$" result_loc_var "\" ]; then : $(($1=save_loc_var_$SP)); fi")
      "    : $((SP -= 1))"
      "    shift"
      )
      (unlines
      "  while [ $# -gt 0 ]; do"
      "    : $(($1=save_loc_var_$SP))"
      "    : $((SP -= 1))"
      "    shift"
      ))
    "  done"
    (if (equal? 'addr (car function-return-method))
      (unlines
      (string-append
      "  eval \"" result_loc_var "=\\$save_loc_var_$SP\"")
      "  : $((SP -= 1))"
      ))
    "}"
    ""
    (case (car function-return-method)
      ((variable)
        (string-append "prim_return_value() { : $((_" (symbol->string (cadr function-return-method)) " = $1)) ; }"))
      ((addr)
        "prim_return_value() { if [ $# -eq 2 ]; then : $(($2 = $1)) ; fi ; }")
      (else
        (error "Unknown value return method" function-return-method)))
    ""
    "defarr() { : $(($1 = ALLOC)) $((ALLOC = ALLOC+$2)) ; }"
    (if support-addr-of?
        "defglo_pointable() { : $(($1 = ALLOC)) $((_$ALLOC = $2)) $((ALLOC = ALLOC+1)) ; }"
        "defglo() { : $(($1 = $2)) ; }")
    ""
    "# Setup argc, argv"
    "argc_for_main=$(($# + 1));"
    "make_argv $argc_for_main \"$0\" $@; argv_for_main=make_argv_ptr;"
    ; This must be after make_argv because if one of the local variable is argv,
    ; writing to it will overwrite the $@ array in zsh.
    (let ((local-vars (map car (table->list (ctx-all-variables ctx)))))
      (if (not (null? local-vars))
        (unlines
          ""
          "# Initialize local vars so set -u can be used"
          (string-append ": $(( "
                        (string-concatenate (map (lambda (l) (format-var l)) local-vars) " = ")
                        " = 0 ))"))))
    ""
    (string-append result_loc_var "=_ # Dummy result location")
    ""
    ))

(define (runtime-postlude)
  (string-append
   (function-name '(six.identifier main))
   (if (equal? 'addr (car function-return-method)) " _" "")
   " $argc_for_main"
   " $argv_for_main"))

(define escape-string-table
  (list->table '((#\alarm    . "\\a")
                 (#\backspace . "\\b")
                 (#\page      . "\\f")
                 (#\newline   . "\\n")
                 (#\return    . "\\r")
                 (#\tab       . "\\t")
                 (#\vtab      . "\\v")
                 (#\\         . "\\\\\\\\")
                 (#\"         . "\\\"")
                 (#\'         . "\\\'")
                 (#\?         . "\\\?")
                 (#\$         . "\\$")
                 )))

(define (escape-string str)
  (define (escape-char c) (table-ref escape-string-table c (list->string (list c))))
  (string-concatenate (map escape-char (string->list str))))

(define (string->variable-name ctx str)
  (let ((var-name (table-ref (ctx-literals ctx) str #f)))
    (if var-name
      var-name
      (let ((new-var-name `(six.identifier ,(string->symbol (string-append "str_" (number->string (table-length (ctx-literals ctx))))))))
        (table-set! (ctx-literals ctx) str new-var-name)
        new-var-name))))

; Initialize the heap, allocating memory for hardcoded strings and arrays.
(define (heap-prelude ctx)
  (if (not (null? (ctx-data ctx)))
    (unlines
      "# Heap initialization"
      (apply unlines
        (map (lambda (datum ix)
              (cond ((list? datum)
                      (string-append
                        "unpack_array " (string-concatenate (map number->string datum) " ")
                        "; __" (number->string ix) "=$unpack_array_addr"))
                    ((string? datum)
                      (def_str_code (string-append "__" (number->string ix)) datum))))
          (reverse (ctx-data ctx))
          (iota (length (ctx-data ctx)))))
      "")
    ""))

(define (comp-file path)
  (let ((ast (call-with-input-file path read-six)))
    (comp-program ast)))

(define (read-six port)
  (##six-types-set! '((void . #f) (void_ptr . #f) (char . #f) (char_ptr . #f) (char_ptr_ptr . #f) (int . #f) (int_ptr . #f) (struct . #f) (enum . #f)))
  (let ((rt (input-port-readtable port)))
    (input-port-readtable-set! port (readtable-start-syntax-set rt 'six))
    (read-all port)))

(define (main . args)
  (for-each comp-file (parse-cmd-line args)))

; Code generation options:
; - function-return-method: addr | variable
; - initialize-memory-when-alloc: boolean
; - free-unsets-variables: boolean
; - inline-inplace-arithmetic-ops: boolean
; - prefix-local-vars: boolean
; - inline-string-init: boolean
; - init-string-precompute-hash: boolean
; - callee-save: boolean
; - arithmetic-conditions: boolean
; - arithmetic-assignment: boolean
(define (parse-cmd-line args)
  (let loop ((args args) (files '()))
    (if (null? args)
        files
        (let ((arg (car args))
              (rest (cdr args)))
          (cond ((and (member arg '("--function-return-method-variable")))
                  (set! function-return-method '(variable 0result))
                  (loop rest files))
                ((and (pair? rest) (member arg '("--function-return-method-arg-loc")))
                  (set! function-return-method `(addr ,(not (equal? "false" (car rest)))))
                  (loop (cdr rest) files))
                ((and (pair? rest) (member arg '("--inline-inplace-arithmetic-ops")))
                  (set! inline-inplace-arithmetic-ops (not (equal? "false" (car rest))))
                  (loop (cdr rest) files))
                ((and (pair? rest) (member arg '("--initialize-memory-when-alloc")))
                  (set! initialize-memory-when-alloc? (not (equal? "false" (car rest))))
                  (loop (cdr rest) files))
                ((and (pair? rest) (member arg '("--free-unsets-variables")))
                  (set! free-unsets-variables? (not (equal? "false" (car rest))))
                  (loop (cdr rest) files))
                ((and (pair? rest) (member arg '("--prefix-local-vars")))
                  (set! prefix-local-vars (not (equal? "false" (car rest))))
                  (loop (cdr rest) files))
                ((and (pair? rest) (member arg '("--inline-string-init")))
                  (set! inline-string-init (not (equal? "false" (car rest))))
                  (loop (cdr rest) files))
                ((and (pair? rest) (member arg '("--init-string-precompute-hash")))
                  (set! init-string-precompute-hash? (not (equal? "false" (car rest))))
                  (loop (cdr rest) files))
                ((and (pair? rest) (member arg '("--callee-save")))
                  (set! callee-save? (not (equal? "false" (car rest))))
                  (loop (cdr rest) files))
                ((and (pair? rest) (member arg '("--arithmetic-conditions")))
                  (set! arithmetic-conditions? (not (equal? "false" (car rest))))
                  (loop (cdr rest) files))
                ((and (pair? rest) (member arg '("--arithmetic-assignment")))
                  (set! arithmetic-assignment? (not (equal? "false" (car rest))))
                  (loop (cdr rest) files))
                (else
                  (if (and (>= (string-length arg) 2)
                           (string=? (substring arg 0 1) "-"))
                      (begin
                        (display "*** unknown option ")
                        (display arg)
                        (newline)
                        (exit 1))
                    (begin
                      (loop rest (cons arg files))))))))))
