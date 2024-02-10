#!/usr/bin/env gsi

(define support-addr-of? #f)
; Use printf and var=$(function_name) to get return value
(define return-with-print? #f)
; Assign $1, $2, ... to local parameters.
; If #f, we use the parameters directly when referring to local variables when possible.
; This generates shorter code, but it may be harder to read and $1, $2, ... can't be assigned.
(define map-all-param-to-local-var? #t)
; Useful for debugging only
(define disable-save-restore-vars? #f)

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

(define (global-var ident)
  (if (symbol? (cadr ident))
    (string-append "_" (symbol->string (cadr ident)))
    (string-append "$" (number->string (cadr ident)))))

(define (global-ref ident)
  (if support-addr-of?
      (string-append "_$" (global-var ident))
      (global-var ident)))

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

(define (comp-loc-define-variable ctx ast)
  (let* ((name (cadr ast))
         (type (caddr ast))
         (dims (cadddr ast))
         (init (car (cddddr ast))))
    (if (pair? dims)
        (error "arrays not yet supported" dims)
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
              (let ((previous-value enum-value-counter)
                    (new-value (cadr (caddr decl))))
                (set! enum-value-counter (+ new-value 1))
                (list "_" (cdadr decl) "=" (number->string previous-value) "; ")))

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
    ;;    (print "#") (write ast) (newline)
    (ctx-add-glo-decl!
     ctx
     (list (function-name name) "() {"))
    (ctx-tail?-set! ctx #t)
    (nest ctx (comp-body ctx body parameters))

    (ctx-add-glo-decl!
     ctx
     (list "}"))))

(define (comp-body ctx lst #!optional (parameters '()))
  (let ((start-loc-env (table-copy (ctx-loc-env ctx)))
        (parameter-table (list->table (map (lambda (p) (cons (car p) #t)) parameters)))) ; Parameters are always initialized
    (ctx-loc-env-set! ctx (table-merge parameter-table (ctx-loc-env ctx)))
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

    (let loop ((lst lst))
      (if (and (pair? lst) (eq? (caar lst) 'six.define-variable))
          (let ((def-var (cadar lst)))
            (table-set! (ctx-loc-env ctx) def-var #f)
            ; TODO: Initialize var?
            (loop (cdr lst)))
          (begin
            (comp-statement-list ctx lst)
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

(define (comp-statement ctx ast)
  (case (car ast)
    ((six.while)
     (if #t ;; simple-expression
         (let ((code-test (comp-test ctx (cadr ast))))
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
       (let ((code-test (if expr2 (comp-test ctx expr2) ":")))
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
       (let ((code-test (comp-test ctx test)))
         (ctx-add-glo-decl!
          ctx
          (list "if " code-test " ; then"))
         (nest ctx (comp-statement ctx stat))
         (if (pair? (cdddr ast))
             (begin
               (ctx-add-glo-decl!
                ctx
                (list "else"))
               (nest ctx (comp-statement ctx (cadddr ast)))))
         (ctx-add-glo-decl!
          ctx
          (list "fi")))))
    ((six.return)
     (if (pair? (cdr ast))
         (let ((code-expr (comp-rvalue ctx (cadr ast) 'return)))
          (if return-with-print?
            (ctx-add-glo-decl!
              ctx
              (list "printf $((" code-expr "))"))
            (if (not (equal? code-expr "_0result"))
              (ctx-add-glo-decl!
                ctx
                (list ": $(( _0result = " code-expr " ))"))))))
     (if (not (ctx-tail? ctx))
         (ctx-add-glo-decl!
          ctx
          (list "return"))))
    ((six.break)
     (ctx-add-glo-decl!
      ctx
      (list "break")))
    (else
     (comp-statement-expr ctx ast))))

(define (comp-test ctx ast)
  (string-append
   "[ 0 != $(( "
   (comp-rvalue ctx ast 'test)
   " )) ]"))

(define runtime-primitives
  '(putchar
    exit
    malloc
    free
    printf
    open
    read
    close
    memset
    memcmp
    show_heap))

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
  ;    - Each function prints its return value and the caller takes it from stdout: var=$(function_name).
  ;       This wouldn't work for functions that want to print to stdout.
  ;    - Functions take an extra parameter that is the address/name of the variable where to store the return value.
  (let ((name (car ast))
        (params (cdr ast)))
    (let* ((code-params
            (map (lambda (p) (string-append "$((" (comp-rvalue ctx p 'argument) "))")) params))
          (call-code
            (string-concatenate (cons (function-name name) code-params) " "))
          (is-prim
            (member (cadr name) runtime-primitives))
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
          (active-local-vars
            (map car (filter cdr (table->list (ctx-loc-env ctx)))))
          (local-vars-to-save
            (filter (lambda (x) (not (equal? assign_to x))) active-local-vars)))

      ; Save local variables
      ; All primitives uses variables not starting with _, meaning that there can't
      ; be a conflicts
      (if (and (not disable-save-restore-vars?) (not is-prim) (not (null? local-vars-to-save)))
        (ctx-add-glo-decl!
          ctx
          (list "save_loc_var " (string-concatenate (reverse (map global-var local-vars-to-save)) " "))))

      (if assign_to
        (if return-with-print?
          (ctx-add-glo-decl!
            ctx
            (list (comp-lvalue ctx assign_to) "=$(" call-code ")"))
          (let ((assign-to-comped (comp-lvalue ctx assign_to)))
            (ctx-add-glo-decl!
              ctx
              (list call-code))
            ; (if (not (equal? assign-to-comped "_0result"))
            (ctx-add-glo-decl!
                ctx
                (list ": $(( " (comp-lvalue ctx assign_to) " = _0result ))"))))
         (ctx-add-glo-decl!
          ctx
          (list
           (string-concatenate
            (cons (function-name name) code-params)
            " "))))

      (if (and (not disable-save-restore-vars?) (not is-prim) (not (null? local-vars-to-save)))
        (ctx-add-glo-decl!
          ctx
          (list "rest_loc_var " (string-concatenate (map global-var local-vars-to-save) " ")))))))

(define (comp-statement-expr ctx ast)
  (case (car ast)
    ((six.call)
     (comp-fun-call ctx (cdr ast)))
    ((six.compound)
     (comp-body ctx (cdr ast)))
    ((six.x=y)
     (comp-assignment ctx ast))
    (else
     (ctx-add-glo-decl!
      ctx
      (list ": $(( " (comp-rvalue ctx ast 'statement-expr) " ))")))))

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
      ((six.identifier six.index)
       (case (car rhs)
        ((six.call)
          (comp-fun-call ctx (cdr rhs) lhs))
        (else
       (ctx-add-glo-decl!
        ctx
            (list ": $(( " (comp-lvalue ctx lhs) " = " (comp-rvalue ctx rhs 'assignment) " ))"))))

       ; Mark the variable as used written to, used to determine if we need to save and restore it
       ; We could do a much smarter lifetime analysis, where we also determine when the variable becomes dead,
       ; but this is simple and works for now.
       (if (eq? (car lhs) 'six.identifier)
           (table-set! (ctx-loc-env ctx) lhs #t))) ; Mark local var as used)
      (else
       (error "unknown lhs" lhs)))))

(define (comp-lvalue ctx ast)
  (case (car ast)
    ((six.identifier)
     (global-ref ast))
    ((six.index)
     (string-append "_$((" (comp-array-lvalue ctx (cadr ast)) "+" (comp-rvalue ctx (caddr ast) 'index-lvalue) "))"))
    (else
     (error "unknown lvalue" ast))))

(define (comp-array-lvalue ctx ast)
  (case (car ast)
    ((six.identifier)
     (global-var ast))
    (else
     (error "unknown array lvalue" ast))))

(define (replace-fun-calls ast)
  (define replaced-fun-calls '())
  (define (alloc-identifier)
    (let ((id (gensym)))
      (list 'six.identifier id)))
  (define (go ast)
    (case (car ast)
      ((six.call)
        (let ((id (alloc-identifier)))
          (set! replaced-fun-calls (cons (cons id ast) replaced-fun-calls))
          id))
      ((six.literal six.list six.identifier)
        ast)
      ((six.x+y six.x-y six.x*y six.x/y six.x%y six.x==y six.x!=y six.x<y six.x>y six.x<=y six.x>=y six.x>>y six.x<<y six.x&y six.x&&y |six.x\|\|y| six.index six.x+=y six.x=y)
        (list (car ast)
              (go (cadr ast))
              (go (caddr ast))))
      ((six.x++ six.x-- six.++x six.--x six.!x six.*x)
        (list (car ast)
              (go (cadr ast))))
      (else
        (error "unknown ast" ast))))
  (cons (go ast) replaced-fun-calls))

(define (replace-identifier ast old new)
  (case (car ast)
    ((six.identifier)
      (if (equal? ast old)
          new
          ast))
    ((six.literal six.list)
      ast)
    ((six.call six.x+y six.x-y six.x*y six.x/y six.x%y six.x==y six.x!=y six.x<y six.x>y six.x<=y six.x>=y six.x>>y six.x<<y six.x&y six.x&&y |six.x\|\|y| six.index six.x+=y six.x=y)
      (list (car ast)
            (replace-identifier (cadr ast) old new)
            (replace-identifier (caddr ast) old new)))
    ((six.x++ six.x-- six.++x six.--x six.!x six.*x)
      (list (car ast)
            (replace-identifier (cadr ast) old new)))
    (else
      (error "unknown ast" ast))))

(define (comp-rvalue ctx ast context-tag)
  (let* ((pre-side-effects '())
         (fun-calls-res (replace-fun-calls ast))
         (new-ast (car fun-calls-res))
         (fun-calls-to-replace (cdr fun-calls-res))
         (post-side-effects '()))
    (case context-tag
      ((index-lvalue return argument statement-expr)
        (if (eq? 1 (length fun-calls-to-replace))
            (let* ((fun-call (car fun-calls-to-replace))
                   (ast-with-0result (replace-identifier new-ast (car fun-call) '(six.identifier 0result))))
              (comp-fun-call ctx (cddr fun-call))
              (comp-rvalue-go ctx ast-with-0result))
            (begin
              (for-each
                (lambda (call) (comp-fun-call ctx (cddr call) (car call)))
                fun-calls-to-replace)
              (comp-rvalue-go ctx new-ast))))
      ((test)
        (if (null? fun-calls-to-replace)
            (comp-rvalue-go ctx new-ast)
            (error "comp-rvalue: Function calls not supported in condition")))
      ((assignment)
        (if (null? fun-calls-to-replace)
            (comp-rvalue-go ctx new-ast)
            (error "Not yet supported")))
      (else
        (error "unknown context tag" context-tag)))))

(define (comp-rvalue-go ctx ast)
  (case (car ast)
    ((six.literal)
     (let ((val (cadr ast)))
       (cond ((exact-integer? val)
              (number->string val))
             ((string? val)
              (ctx-data-set! ctx (cons val (ctx-data ctx)))
              (obj-ref (- (length (ctx-data ctx)) 1)))
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
    (global-ref ast))
    ((six.x+y six.x-y six.x*y six.x/y six.x%y six.x==y six.x!=y six.x<y six.x>y six.x<=y six.x>=y six.x>>y six.x<<y six.x&y six.x&&y |six.x\|\|y| )
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
                      ((six.x&&y)  " && ")
                      ((|six.x\|\|y|)  " || ")
                      )
                    (comp-rvalue-go ctx (caddr ast))
                    ")"))
    ((six.index)
     (string-append "_$((" (comp-array-lvalue ctx (cadr ast)) "+" (comp-rvalue-go ctx (caddr ast)) "))"))
    ((six.x++)
     (string-append "$((" (comp-lvalue ctx (cadr ast)) " += 1, " (comp-lvalue ctx (cadr ast)) " - 1" "))"))
    ((six.x--)
     (string-append "$((" (comp-lvalue ctx (cadr ast)) " -= 1, " (comp-lvalue ctx (cadr ast)) " + 1" "))"))
    ((six.++x)
     (string-append (comp-lvalue ctx (cadr ast)) " += 1"))
    ((six.--x)
     (string-append (comp-lvalue ctx (cadr ast)) " -= 1"))
    ((six.!x)
     (string-append "!" (comp-lvalue ctx (cadr ast))))
    ((six.x+=y)
     (string-append (comp-lvalue ctx (cadr ast)) " += " (comp-rvalue-go ctx (caddr ast))))
    ((six.x=y)
     (string-append (comp-lvalue ctx (cadr ast)) " = " (comp-rvalue-go ctx (caddr ast))))
    ((six.*x)
     (string-append "_$((" (comp-rvalue-go ctx (cadr ast)) "))"))
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
   "set -e -u"
   ""
   "# Load runtime library and primitives"
   "source runtime.sh"
   ""
   "defarr() { : $(($1 = ALLOC)) $((ALLOC = ALLOC+$2)) ; }"
   (if support-addr-of?
      "defglo_pointable() { : $(($1 = ALLOC)) $((_$ALLOC = $2)) $((ALLOC = ALLOC+1)) ; }"
      "defglo() { : $(($1 = $2)) ; }")
   ""
   "# Setup argc, argv"
   "argc=$#;"
   "make_argv $argc $@; argv_ptr=make_argv_ptr;"
   ""))

(define runtime-postlude
  (string-append
   (function-name '(six.identifier main))
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
  (##six-types-set! '((void . #f) (void_ptr . #f) (char . #f) (char_ptr . #f) (int . #f) (int_ptr . #f) (struct . #f) (enum . #f)))
  (let ((rt (input-port-readtable port)))
    (input-port-readtable-set! port (readtable-start-syntax-set rt 'six))
    (read-all port)))

(define (main . args)
  (for-each comp-file args))

;; Example:
;;
;; $ gsi six-cc.scm winter-pi2.c > winter-pi2.sh ; time dash winter-pi2.sh
;; 31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185
;;
;; real	0m2.394s
;; user	0m2.143s
;; sys	0m0.248s
;; $ cat winter-pi2.c
;; /* #include <stdio.h> */
;;
;; /* https://cs.uwaterloo.ca/~alopez-o/math-faq/mathtext/node12.html */
;;
;;
;; int r[2801];
;; int i;
;; int k;
;; int b;
;; int d;
;; int c = 0;
;;
;; int main() {
;;
;;     for (; i < 2800; i++) {
;; 	r[i] = 2000;
;;     }
;;     r[i] = 0;
;;
;;     for (k = 2800; k > 0; k = k - 14) {
;;
;; 	d = 0;
;;
;; 	i = k;
;; 	for (;;) {
;; 	    d = d + r[i] * 10000;
;; 	    b = 2 * i - 1;
;;
;; 	    r[i] = d % b;
;; 	    d = d / b;
;; 	    i--;
;; 	    if (i == 0) break;
;; 	    d = d * i;
;; 	}
;;         putchar(48 + (c + d / 10000) / 1000 % 10);
;;         putchar(48 + (c + d / 10000) / 100 % 10);
;;         putchar(48 + (c + d / 10000) / 10 % 10);
;;         putchar(48 + (c + d / 10000) % 10);
;; 	c = d % 10000;
;;     }
;;
;;     putchar(10);
;;
;;     return 0;
;; }
