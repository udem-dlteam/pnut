#!/usr/bin/env gsi

(define support-addr-of? #f)
; Determines how to return a value from a function call.
; Possible values:
; - '(variable var_name): Each function returns in a variable called {var_name} and the caller must save it if needed.
; - '(addr always-pass): Functions take an extra parameter (always or when assigning) that is the name of the variable where to store the return value.
(define value-return-method '(variable 0result))
; (define value-return-method '(addr #t))
; Assign $1, $2, ... to local parameters.
; If #f, we use the parameters directly when referring to local variables when possible.
; This generates shorter code, but it may be harder to read and $1, $2, ... can't be assigned.
(define map-all-param-to-local-var? #t)
; Useful for debugging only
(define disable-save-restore-vars? #f)
; Disable for faster execution of programs that allocate a lot of memory
(define initialize-memory-when-alloc #t)
; Inline += -= operations?
(define inline-inplace-arithmetic-ops #t)

(define (function-name ident)
  (string-append "_" (symbol->string (cadr ident))))

(define-type ctx
  glo-decls
  loc-env
  data ; The data section
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

(define (is-local-var ctx ident)
  (table-ref (ctx-loc-env ctx) ident #f))

(define (env-var ctx ident)
  (if (number? (cadr ident)) ; Number identifiers are always local and don't appear in the local environment
    (string-append "$" (number->string (cadr ident)))
    (if (table-ref (ctx-loc-env ctx) ident #f)
      (format-var ident)
      (global-ref ident))))

(define (global-var ident)
  (string-append "_" (symbol->string (cadr ident))))

(define (global-ref ident)
  (if support-addr-of?
      (string-append "_$" (global-var ident))
      (global-var ident)))

(define (format-var ident)
  (string-append "_" (symbol->string (cadr ident))))

(define (obj-ref ident)
  (string-append "__" (number->string ident)))

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
                (list "_" (cdadr decl) "=" (number->string new-value) "; ")))

            ((six.identifier)
              (set! enum-value-counter (+ enum-value-counter 1))
              (list "_" (cdr decl) "=" (number->string (- enum-value-counter 1)) "; "))

            (else
              (error "Enum definition can only contain identifiers or assigments. Got:" decl))))
        body))

    (ctx-add-glo-decl! ctx '())))

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
             (mk-param (lambda (p i) (cons (car p) (make-local-var i #t)))) ; Parameters are always initialized
             (parameters-list (map mk-param parameters (iota (length parameters) 1)))
             (parameter-table (list->table parameters-list)))
        (ctx-loc-env-set! ctx (table-merge parameter-table (ctx-loc-env ctx)))
        (if (equal? 'addr (car value-return-method))
          (begin
            (shift-ctx-loc-env-position ctx) ; Make room for the result_loc var
            (table-set! (ctx-loc-env ctx) '(six.identifier result_loc) (make-local-var 1 #t))
            (if (cadr value-return-method) ; return address is always passed?
              (begin
                (ctx-add-glo-decl!
                  ctx
                  (list (env-var ctx '(six.identifier result_loc))
                        "=$1; shift "
                        "# Remove result_loc param")))
              (begin
                ; TODO: Replace _result_loc with a call to local-var when it is implemented
                (ctx-add-glo-decl!
                  ctx
                  (list "if [ $# -eq " (+ 1 (length parameters)) " ]; "
                        "then " (env-var ctx '(six.identifier result_loc)) "=$1; shift ; "
                        "else " (env-var ctx '(six.identifier result_loc)) "= ; fi ;"))))))

        ; IDEA: We could also use $1, $2, ... when referring to parameters.
        ; If map-all-param-to-local-var? is #f, we use the parameters directly,
        ; generating shorter code, but that's harder to read and where we can't
        ; assign
        (let ((local-vars-to-map
                (if map-all-param-to-local-var?
                  (map cons parameters (iota (length parameters) 1))
                  (error "not yet supported"))))
          (for-each
            (lambda (p) (comp-assignment ctx `(six.x=y (six.identifier ,(cadaar p)) (six.identifier ,(cdr p)))))
            local-vars-to-map))
      (comp-body ctx body)
      (ctx-loc-env-set! ctx start-loc-env)))

    (ctx-add-glo-decl!
     ctx
     (list "}"))))

(define (comp-body ctx lst)
  (let ((start-loc-env (table-copy (ctx-loc-env ctx))))
    (let loop ((lst lst) (new-local-vars '()))
      (if (and (pair? lst) (eq? (caar lst) 'six.define-variable))
          (let* ((def-var (car lst))
                 (var-name (cadr def-var))
                 (var-init (car (cddddr def-var))))
            (table-set! (ctx-loc-env ctx)
                        var-name
                        (make-local-var (+ 1 (table-length (ctx-loc-env ctx))) var-init))
            (loop (cdr lst) (cons (cons var-name var-init) new-local-vars)))
          (begin
            (for-each
              (lambda (new-local-var)
                (if (cdr new-local-var)
                  (comp-assignment ctx `(six.x=y ,(car new-local-var) ,(cdr new-local-var)))
                  (comp-assignment ctx `(six.x=y ,(car new-local-var) (six.literal 0)))))
              new-local-vars)
            (comp-statement-list ctx lst)
            ; Bug: We need to propagate the is_initialized flag of local variables to start-loc-env?
            (ctx-loc-env-set! ctx start-loc-env))))))

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
          (case (car value-return-method)
            ((variable)
             (ctx-add-glo-decl!
              ctx
              (list "_" (symbol->string (cadr value-return-method)) "=$((" code-expr "))")))
            ((addr)
             (ctx-add-glo-decl!
              ctx
              (list "prim_return_value $((" code-expr ")) $" (env-var ctx '(six.identifier result_loc)))))
            (else
              (error "Unknown value return method" value-return-method)))))
    ; Seems broken in while loops
    ;  (if (not (ctx-tail? ctx))
         (ctx-add-glo-decl!
          ctx
          (list "return")))
    ((six.break)
     (ctx-add-glo-decl!
      ctx
      (list "break")))
    (else
     (comp-statement-expr ctx ast))))

(define (comp-if-test ctx ast)
  (let* ((rvalue (comp-rvalue ctx ast '(test-if))))
    (string-append
      "[ 0 != $(( "
      rvalue
      " )) ]")))

(define (comp-loop-test ctx ast)
  (let* ((rvalue-res (comp-rvalue ctx ast '(test)))
         (rvalue (car rvalue-res))
         (pre-side-effects (comp-side-effects ctx (cdr rvalue-res))))

    (string-append
      (apply string-append (map (lambda (e) (string-append e "; ")) pre-side-effects))
      "[ 0 != $(( "
      rvalue
      " )) ]")))

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
                    (string-append "$((" (comp-rvalue ctx p `(argument ,params-after-count)) "))")))
               params))
         (call-code
          (string-concatenate (cons (function-name name) code-params) " "))
         (is-prim
          (member (cadr name) (map car runtime-primitives)))
         (can-return
          (if is-prim
              (cadr (assoc (cadr name) runtime-primitives))
              #t)) ; We assume that all user-defined functions can return a value
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
          (initialized-local-vars
           (map car (filter (lambda (l) (local-var-initialized (cdr l))) (table->list (ctx-loc-env ctx)))))
          (local-vars-to-save
           (filter (lambda (x) (not (member x (list assign_to)))) initialized-local-vars))
          (local-vars-to-save-no-result-loc
           (filter (lambda (x) (not (member x (list assign_to '(six.identifier result_loc))))) initialized-local-vars)))

      ; Save local variables
      ; All primitives uses variables not starting with _, meaning that there can't
      ; be a conflicts
      ; TODO: Disable save_loc_var/rest_loc_var for tail calls
      (if (and (not disable-save-restore-vars?) (not is-prim) (not (null? local-vars-to-save)))
        (ctx-add-glo-decl!
          ctx
          (list "save_loc_var " (string-concatenate (reverse (map (lambda (l) (env-var ctx l)) local-vars-to-save-no-result-loc)) " "))))

      (if (and (not can-return) assign_to)
        (error "Function call can't return a value, but we are assigning to a variable"))

      (case (car value-return-method)
        ((variable)
          (ctx-add-glo-decl! ctx (list call-code))
          (if assign_to
            (comp-assignment ctx `(six.x=y ,assign_to (six.identifier ,(cadr value-return-method))))))
        ((addr)
          (if (and (or assign_to (cadr value-return-method)) can-return) ; Always pass the return address, even if it's not used, as long as the function can return a value
            (ctx-add-glo-decl!
              ctx
              (list (string-concatenate (cons (function-name name)
                                          (cons (env-var ctx (or assign_to '(identifier dummy_loc)))
                                          code-params))
                                        " ")))
          (ctx-add-glo-decl! ctx (list call-code))))
        (else
          (error "Unknown value return method" value-return-method)))

      (if (and (not disable-save-restore-vars?) (not is-prim) (not (null? local-vars-to-save)))
        (ctx-add-glo-decl!
          ctx
          (list "rest_loc_var " (string-concatenate (map (lambda (l) (env-var ctx l)) local-vars-to-save-no-result-loc) " "))))))

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
          (let* ((rvalue-res (comp-rvalue ctx rhs '(assignment)))
                 (rvalue (car rvalue-res))
                 (pre-side-effects (comp-side-effects ctx (cdr rvalue-res))))
          (for-each
            (lambda (p) (ctx-add-glo-decl! ctx p))
            pre-side-effects)
          (ctx-add-glo-decl!
            ctx
            (list ": $(( " (comp-lvalue ctx lhs) " = " rvalue " ))"))
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
(define (handle-side-effects ast)
  (define replaced-fun-calls '())
  (define pre-side-effect '())
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
      ((six.literal six.list six.identifier six.-x)
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

(define (comp-rvalue ctx ast context-tag)
  (let* ((side-effects-res (handle-side-effects ast))
         (new-ast (car side-effects-res))
         (fun-calls-to-replace (cadr side-effects-res))
         (pre-side-effects (caddr side-effects-res))
         (contains-side-effect (cadddr side-effects-res)))
    (define (comp-side-effects-then-rvalue)
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
      (if (and (equal? (car value-return-method) 'variable) (not (null? fun-calls-to-replace)))
        (let ((must-assign-to-return-loc (and (equal? (car context-tag) 'argument) (< 0 (cadr context-tag))))
              (ast-with-result-identifier
                (replace-identifier new-ast
                                    (car (last fun-calls-to-replace)) ; Location of last function call
                                    `(six.identifier ,(cadr value-return-method)))))
          (for-each
            (lambda (e) (ctx-add-glo-decl! ctx e))
            (comp-side-effects ctx pre-side-effects))
          (for-each
            (lambda (call nextCall)
              ; Check if next function call uses the result, if it's the case use the variable directly
              (let ((nextCallUsesReturnLoc (and nextCall (member (car call) (cddr nextCall)))))
                (if nextCallUsesReturnLoc
                  (begin
                    ; Overwrite the argument of next function call with the return location
                    (set-cdr! (car nextCallUsesReturnLoc) (cdr value-return-method))
                    (comp-fun-call ctx (cddr call)))
                  ; We only assign to the return location if it's not the
                  ; last function call. In that case, it's up to the caller
                  ; of comp-rvalue to read the value.
                  ; TODO: Add the return location to the list of local vars to save to support recursive function calls
                  (comp-fun-call ctx (cddr call) (and (or nextCall must-assign-to-return-loc) (car call))))))
            fun-calls-to-replace
            (cdr (reverse (cons #f (reverse fun-calls-to-replace)))))
          (if must-assign-to-return-loc
            (comp-rvalue-go ctx new-ast)
            (comp-rvalue-go ctx ast-with-result-identifier)))
        (begin
          (for-each
            (lambda (e) (ctx-add-glo-decl! ctx e))
            (comp-side-effects ctx pre-side-effects))
          (for-each
            (lambda (call) (comp-fun-call ctx (cddr call) (car call)))
            fun-calls-to-replace)
          (comp-rvalue-go ctx new-ast))))
    (case (car context-tag)
      ((return index-lvalue argument) (comp-side-effects-then-rvalue))
      ((statement-expr)
        (if contains-side-effect
          (ctx-add-glo-decl!
            ctx
            (list ": $(( " (comp-side-effects-then-rvalue) " ))"))
          (comp-side-effects-then-rvalue)))
      ((test)
        (if (null? fun-calls-to-replace)
          (cons (comp-rvalue-go ctx new-ast) pre-side-effects)
          (error "comp-rvalue: Function calls not supported in condition" ast)))
      ((test-if)
        (for-each
          (lambda (e) (ctx-add-glo-decl! ctx e))
          (comp-side-effects ctx pre-side-effects))
        (for-each
          (lambda (call) (comp-fun-call ctx (cddr call) (car call)))
          fun-calls-to-replace)
        (comp-rvalue-go ctx new-ast))
      ((assignment)
        (if (null? fun-calls-to-replace)
            (cons (comp-rvalue-go ctx new-ast) pre-side-effects)
            (error "function calls in assignment not supported" ast)))
      ((pure)
        (if (and (null? fun-calls-to-replace) (null? pre-side-effects))
            (comp-rvalue-go ctx new-ast)
            (error "function calls and side effects in pure context not supported" ast)))
      (else
        (error "unknown context tag" context-tag)))))

(define (comp-rvalue-go ctx ast)
  (case (car ast)
    ((six.literal)
     (let ((val (cadr ast)))
       (cond ((exact-integer? val)
              (number->string val))
             ((string? val)
              ; Hacky way to detect character literals
              ; since six doesn't distinguish between " and '
              (if (equal? 1 (string-length val))
                (number->string (char->integer (string-ref val 0)))
                (begin
                  (ctx-data-set! ctx (cons val (ctx-data ctx)))
                  (obj-ref (- (length (ctx-data ctx)) 1)))))
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
              (obj-ref (- (length (ctx-data ctx)) 1))))))
    ((six.identifier)
     (env-var ctx ast))
    ((six.x+y six.x-y six.x*y six.x/y six.x%y six.x==y six.x!=y six.x<y six.x>y six.x<=y six.x>=y six.x>>y six.x<<y six.x&y |six.x\|y| six.x^y six.x&&y |six.x\|\|y| )
     (string-append "("
                    (comp-rvalue-go ctx (cadr ast))
                    (case (car ast)
                      ((six.x+y) " + ")
                      ((six.x-y) " - ")
                      ((six.x*y) " * ")
                      ((six.x/y) " / ")
                      ((six.x%y) " % ")
                      ((six.x==y) " == ")
                      ((six.x!=y) " != ")
                      ((six.x<y) " < ")
                      ((six.x>y) " > ")
                      ((six.x<=y) " <= ")
                      ((six.x>=y) " >= ")
                      ((six.x>>y) " >> ")
                      ((six.x<<y) " << ")
                      ((six.x&y)  " & ")
                      ((|six.x\|y|) " | ")
                      ((six.x^y) " ^ ")
                      ((six.x&&y)  " && ")
                      ((|six.x\|\|y|)  " || ")
                      )
                    (comp-rvalue-go ctx (caddr ast))
                    ")"))
    ((six.-x)
     (string-append "-(" (comp-rvalue-go ctx (cadr ast)) ")"))
    ((six.x?y:z)
      (string-append "("
                    (comp-rvalue-go ctx (cadr ast))
                    " ? "
                    (comp-rvalue-go ctx (caddr ast))
                    " : "
                    (comp-rvalue-go ctx (cadddr ast))
                    " ) "))
    ((six.index)
     (string-append "_$((" (comp-array-lvalue ctx (cadr ast)) "+" (comp-rvalue-go ctx (caddr ast)) "))"))
    ((six.x++)
     (error "x++ should have been replaced with x - 1 (and ++x side effect) by comp-rvalue"))
    ((six.x--)
     (error "x-- should have been replaced with (x + 1) (and --x side effect) by comp-rvalue"))
    ((six.++x)
     (string-append "(" (comp-lvalue ctx (cadr ast)) " += 1)"))
    ((six.--x)
     (string-append "(" (comp-lvalue ctx (cadr ast)) " -= 1)"))
    ((six.!x)
     (string-append "!(" (comp-rvalue-go ctx (cadr ast)) ")" ))
    ((six.x+=y)
     (string-append "(" (comp-lvalue ctx (cadr ast)) " += " (comp-rvalue-go ctx (caddr ast)) ")"))
    ((six.x-=y)
     (string-append "(" (comp-lvalue ctx (cadr ast)) " -= " (comp-rvalue-go ctx (caddr ast)) ")"))
    ((six.x*=y)
     (string-append "(" (comp-lvalue ctx (cadr ast)) " *= " (comp-rvalue-go ctx (caddr ast)) ")"))
    ((six.x/=y)
     (string-append "(" (comp-lvalue ctx (cadr ast)) " /= " (comp-rvalue-go ctx (caddr ast)) ")"))
    ((six.x%=y)
     (string-append "(" (comp-lvalue ctx (cadr ast)) " %= " (comp-rvalue-go ctx (caddr ast)) ")"))
    ((six.x=y)
     ; Can't use comp-assignment here because it modifies the context and we instead want to return the code
     ; (comp-assignment ctx `(six.x=y ,(cadr ast) ,(caddr ast))))
     (string-append "(" (comp-rvalue-go ctx (cadr ast)) " = " (comp-rvalue-go ctx (caddr ast)) ")"))
    ((six.*x)
     (string-append "_$((" (comp-rvalue-go ctx (cadr ast)) "))"))
    ((six.**x)
     (string-append "_$(( _$((" (comp-rvalue-go ctx (cadr ast)) ")) ))"))
    ((six.call)
      (error "Function calls not supported in rvalue"))
    (else
     (error "unknown rvalue" ast))))

(define (comp-program ast)
  (let* ((ctx (make-ctx '() (make-table) '() 0 #f)))
    (comp-glo-decl-list ctx ast)
    (codegen ctx)))

(define (codegen ctx)
  (println runtime-prelude)
  (println (heap-prelude ctx))

  (for-each (lambda (decl)
              (let ((level (cdr decl)))
                (if (not (zero? level))
                    (print (make-string (* 2 level) #\space)))
                (println (car decl))))
            (reverse (ctx-glo-decls ctx)))
  (println runtime-postlude))

(define (unlines . lst)
  (string-concatenate lst "\n"))

(define runtime-prelude
  (unlines
   (if (and (equal? (car value-return-method) 'addr) (cadr value-return-method))
     "set -e"
     "set -e -u")
   ""
   "# Load runtime library and primitives"
   ". $(pwd)/runtime.sh # TODO: Do not use pwd"
   ""
   (if initialize-memory-when-alloc
    "STRICT_MODE=1"
    "STRICT_MODE=0"
   )
   ""
   (case (car value-return-method)
     ((variable)
      (string-append "prim_return_value() { : $((_" (symbol->string (cadr value-return-method)) " = $1)) ; }"))
     ((addr)
      "prim_return_value() { if [ $# -eq 2 ]; then : $(($2 = $1)) ; fi ; }")
     (else
      (error "Unknown value return method" value-return-method)))
   ""
   "defarr() { : $(($1 = ALLOC)) $((ALLOC = ALLOC+$2)) ; }"
   (if support-addr-of?
      "defglo_pointable() { : $(($1 = ALLOC)) $((_$ALLOC = $2)) $((ALLOC = ALLOC+1)) ; }"
      "defglo() { : $(($1 = $2)) ; }")
   ""
   "# Setup argc, argv"
   "argc=$(($# + 1));"
   "make_argv $argc \"$0\" $@; argv_ptr=make_argv_ptr;"
   ""))

(define runtime-postlude
  (string-append
   (function-name '(six.identifier main))
   (if (equal? 'addr (car value-return-method)) " _dummy_loc" "")
   " $argc"
   " $argv_ptr"))

(define (escape-string str)
  (define (escape-char c)
    (if (char=? c #\\) "\\\\" ; Escape backslash.
    (if (char=? c #\") "\\\"" ; Escape double quote.
    ; (if (char=? c #\newline) "\\n"  ; Escape newline.
    (list->string (list c)))))
  (string-concatenate (map escape-char (string->list str))))

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
                      (string-append
                        "unpack_string " "\"" (escape-string datum) "\""
                        "; __" (number->string ix) "=$unpack_string_addr"))))
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
  (for-each comp-file args))
