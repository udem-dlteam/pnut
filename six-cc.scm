#!/usr/bin/env gsi

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
; Always use arithmetic expansion to do if/while conditions?
; ** Warning: Using this option breaks shortcut evaluation. **
(define arithmetic-conditions? #f)
; Always use arithmetic expansion to do assignments?
; Warning: Assignment using arithmetic expansion doesn't overflow while regular
; assignment to an overflowing expression does in ksh. The overflow breaks c4's
; hashing algorithm, and prevents it from compiling itself.
; ** Do not use this option if you want to compile c4 with c4 **
(define arithmetic-assignment? #f)
; Use $1 for the return location instead of result_loc.
(define use-$1-for-return-loc? #t)
; Define constants for characters in the data section.
(define define-constants-for-characters? #t)
; defarr initializes memory or not?
; The C semantic is to initialize memory to 0, but we can save a lot of time by
; only initializing memory that's actually needed.
(define initialize-globals? #f)
; Optimize readonly function parameters by keeping them in $1, $2, ...
(define optimize-readonly-params? #t)

(define (function-name ident)
  (string-append "_" (symbol->string (cadr ident))))

(define-type ctx
  glo-decls           ; Lines of code generated so far
  loc-env             ; Local environment
  all-variables       ; Set represented as a table
  literals            ; Literals that have been assigned to a variable
  characters          ; Characters that have been assigned to a variable
  enums               ; Enums that have been defined.
  structs             ; Structures that have been defined
  data                ; The data section
  level               ; The current level of nesting
  tail?               ; Is the current statement is in tail position?
  single-statement?   ; If the current block has only one statement
  loop-nested-level   ; Are many nested loops are we in?
  loop-end-actions    ; What to do at the end of a loop. Typically an increment.
  block-type          ; What kind of block are we in? (function, loop, switch, if, ...)
  gensym-counter      ; Counter for generating unique identifiers
  max-gensym-counter  ; Maximum value of the gensym counter
  )

(define (empty-ctx)
  (make-ctx
    '()          ; global declarations
    (make-table) ; local environment
    (make-table) ; all variables
    (make-table) ; literals
    (make-table) ; characters
    (make-table) ; enums
    (make-table) ; structs
    '()          ; data
    0            ; level
    #f           ; tail?
    #f           ; single-statement?
    0            ; loop-nested-level
    '()          ; loop-end-actions
    'default     ; block-type
    0            ; gensym-counter
    0            ; max-gensym-counter
    ))

(define (ctx-add-glo-decl! ctx decl #!optional (level #f) )
  (ctx-glo-decls-set! ctx (cons (cons decl (or level (ctx-level ctx))) (ctx-glo-decls ctx))))

(define (scope-glo-decls ctx body)
  (let ((old-glo-decls (ctx-glo-decls ctx)))
    (ctx-glo-decls-set! ctx '())
    (let ((res (body))
          (new-glo-decls (ctx-glo-decls ctx)))
      (ctx-glo-decls-set! ctx old-glo-decls) ; Reset the global declarations to the previous state
      (cons res (reverse (map car new-glo-decls))))))

(define (scope-glo-decls-with-level ctx body)
  (let ((old-glo-decls (ctx-glo-decls ctx)))
    (ctx-glo-decls-set! ctx '())
    (let ((res (body))
          (new-glo-decls (ctx-glo-decls ctx)))
      (ctx-glo-decls-set! ctx old-glo-decls) ; Reset the global declarations to the previous state
      (reverse new-glo-decls))))

(define (glo-decl-unlines decls)
  (define (concat-line l) (if (pair? l) (string-concatenate l) l))
  (if (null? decls)
    ""
    (string-append
      (string-concatenate (map concat-line decls) "; ")
      "; ")))

(define (nest-level ctx f)
  (ctx-level-set! ctx (+ (ctx-level ctx) 1))
  (f)
  (ctx-level-set! ctx (- (ctx-level ctx) 1)))

(define-macro (nest ctx . body)
  `(nest-level ,ctx (lambda () ,@body)))

(define-type local-var
  position    ; Position of the variable in the local environment
  type        ; Function param or local
  written-to  ; If the variable has been written to
  )

; (define (shift-ctx-loc-env-position ctx)
;   (for-each (lambda (p)
;               (if (local-var-position (cdr p))
;                 (local-var-position-set! (cdr p) (+ 1 (local-var-position (cdr p))))))
;             (table->list (ctx-loc-env ctx))))


; Assumes that l1 and l2 are sorted by identifier
; l1 and l2 are lists of (ident . local-var) objects
(define (merge-local-variables-helper tbl ls)
  (for-each
    (lambda (l)
      (let ((e (table-ref tbl (car l) #f)))
        (if e
          (table-set! tbl (car l)
                          (make-local-var
                            (or (local-var-position    e) (local-var-position    (cdr l)))
                            (or (local-var-type        e) (local-var-type        (cdr l)))
                            (or (local-var-written-to  e) (local-var-written-to  (cdr l)))))
          (table-set! tbl (car l) (cdr l)))))
    ls))

(define (merge-local-variables . ls)
  (define tbl (make-table))
  (for-each
    (lambda (l) (merge-local-variables-helper tbl l))
    ls)
  (table->list tbl))

(define (add-new-local-var ctx ident #!optional (position #f))
  (table-set! (ctx-loc-env ctx)
              ident
              (make-local-var
                (or position (+ 1 (table-length (ctx-loc-env ctx))))
                (if position 'param 'local)
                #f))
  ; Gather the list of all local variables.
  ; Used to initialize them all at the beginning of the execution so
  ; save_loc_var doesn't crash when saving an unitialized variable.
  ; result_loc is always initialized, so we don't need to add it to the list.
  (if (not (equal? 'six.internal-identifier (car ident)))
    (table-set! (ctx-all-variables ctx) ident '())))

(define (is-local-var ctx ident)
  (table-ref (ctx-loc-env ctx) ident #f))

(define (is-mutable-local-var ctx ident)
  (let ((var (table-ref (ctx-loc-env ctx) ident #f)))
    (if var
      (local-var-written-to var)
      #f)))

(define (env-var ctx ident #!optional (prefixed-by-dollar #f))
  (let ((local-var (table-ref (ctx-loc-env ctx) ident #f)))
    (cond
      ((and (number? (cadr ident)) (equal? 'six.identifier (car ident))) ; Number identifiers are always local and don't appear in the local environment
        (string-append (if prefixed-by-dollar "" "$") (number->string (cadr ident))))
      (local-var
        (if (or (or (local-var-written-to local-var) (equal? 'local (local-var-type local-var)))
                (not optimize-readonly-params?))
          (format-local-var ident)
          (if prefixed-by-dollar ; The variable is stored in $1, $2, ...
              (number->string (local-var-position local-var))
              (string-append "$" (number->string (local-var-position local-var))))))
      (else
        (format-non-local-var ident)))))

(define (format-non-local-var ident)
  (cond
    ((equal? 'six.internal-identifier (car ident))
      (internal-ref ident))
    (else ; Everything else is global
      (global-ref ident))))

(define (global-var ident)
  (string+symbol "_" (cadr ident)))

(define (global-ref ident)
  (global-var ident))

(define (enum-ref ctx ident)
  (let ((enum (table-ref (ctx-enums ctx) (cadr ident) #f)))
    (if enum
      (string+symbol "_" (cadr ident))
      (error "Unknown enum label constant" ident))))

(define (format-local-var ident)
  (if prefix-local-vars
    (string+symbol "_" (cadr ident))
    (symbol->string (cadr ident))))

(define (internal-ref ident)
  (if (exact-integer? (cadr ident))
    (string-append "__g" (number->string (cadr ident))) ; From alloc-identifier
    (string+symbol "__" (cadr ident))))

(define (obj-ref ident)
  (string-append "__" (number->string ident)))

(define (string+symbol str sym)
  (string-append str (symbol->string sym)))

(define result-loc-ident       '(six.internal-identifier result_loc))
(define no-result-loc-ident    '(six.internal-identifier ||))               ; || is empty symbol
(define argc-ident             '(six.internal-identifier argc_for_main))
(define argv-ident             '(six.internal-identifier argv_for_main))
(define sp-ident               '(six.internal-identifier SP))               ; Local variables stack pointer
(define strict-mode-ident      '(six.internal-identifier STRICT_MODE))      ; If malloc initializes the memory it allocates to 0
(define free-unsets-vars-ident '(six.internal-identifier FREE_UNSETS_VARS)) ; If free unsets variables or is NOP
(define init-globals-ident     '(six.internal-identifier INIT_GLOBALS))     ; If global variables are initialized

(define (make-string-ident ix)
  `(six.internal-identifier ,(string->symbol (string-append "str_" (number->string ix)))))

(define result-loc-var       (format-non-local-var result-loc-ident))
(define no-result-loc-var    (format-non-local-var no-result-loc-ident))
(define argc-var             (format-non-local-var argc-ident))
(define argv-var             (format-non-local-var argv-ident))
(define sp-var               (format-non-local-var sp-ident))
(define strict-mode-var      (format-non-local-var strict-mode-ident))
(define free-unsets-vars-var (format-non-local-var free-unsets-vars-ident))
(define init-globals-var     (format-non-local-var init-globals-ident))

(define (get-result-var ctx) (if use-$1-for-return-loc? "1" result-loc-ident))
(define (get-result-loc ctx) (if use-$1-for-return-loc? "1" result-loc-var))

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
          (comp-glo-define-struct ctx ast))
        (else
          (comp-glo-define-procedure ctx ast))))
    (else
     (error "unknown global declaration" ast))))

(define (character-ident c)
  (define (with-prefix prefix c)
    `(six.internal-identifier ,(string->symbol (string-append prefix (list->string (list c))))))

  (define (mk-ident c)
    `(six.internal-identifier ,(string->symbol c)))

  (cond
    ((char-lower-case? c) (with-prefix "LOWER_" c))
    ((char-upper-case? c) (with-prefix "UPPER_" c))
    ((char-numeric? c)    (with-prefix "DIGIT_" c))
    ((eq? c #\null)       (mk-ident "NULL_CH"))
    ((eq? c #\newline)    (mk-ident "NEWLINE_CH"))
    ((eq? c #\space)      (mk-ident "SPACE_CH"))
    ((eq? c #\!)          (mk-ident "EXCL_CH"))
    ((eq? c #\")          (mk-ident "DQUOTE_CH"))
    ((eq? c #\#)          (mk-ident "SHARP_CH"))
    ((eq? c #\$)          (mk-ident "DOLLAR_CH"))
    ((eq? c #\%)          (mk-ident "PERCENT_CH"))
    ((eq? c #\&)          (mk-ident "AMP_CH"))
    ((eq? c #\')          (mk-ident "QUOTE_CH"))
    ((eq? c #\()          (mk-ident "LPAREN_CH"))
    ((eq? c #\))          (mk-ident "RPAREN_CH"))
    ((eq? c #\*)          (mk-ident "STAR_CH"))
    ((eq? c #\+)          (mk-ident "PLUS_CH"))
    ((eq? c #\,)          (mk-ident "COMMA_CH"))
    ((eq? c #\-)          (mk-ident "MINUS_CH"))
    ((eq? c #\.)          (mk-ident "PERIOD_CH"))
    ((eq? c #\/)          (mk-ident "SLASH_CH"))
    ((eq? c #\:)          (mk-ident "COLON_CH"))
    ((eq? c #\;)          (mk-ident "SEMICOLON_CH"))
    ((eq? c #\<)          (mk-ident "LT_CH"))
    ((eq? c #\=)          (mk-ident "EQ_CH"))
    ((eq? c #\>)          (mk-ident "GT_CH"))
    ((eq? c #\?)          (mk-ident "QUESTION_CH"))
    ((eq? c #\@)          (mk-ident "AT_CH"))
    ((eq? c #\^)          (mk-ident "CARET_CH"))
    ((eq? c #\[)          (mk-ident "LBRACK_CH"))
    ((eq? c #\\)          (mk-ident "BACKSLASH_CH"))
    ((eq? c #\])          (mk-ident "RBRACK_CH"))
    ((eq? c #\_)          (mk-ident "UNDERSCORE_CH"))
    ((eq? c #\`)          (mk-ident "BACKTICK_CH"))
    ((eq? c #\{)          (mk-ident "LBRACE_CH"))
    ((eq? c #\|)          (mk-ident "BAR_CH"))
    ((eq? c #\})          (mk-ident "RBRACE_CH"))
    ((eq? c #\~)          (mk-ident "TILDE_CH"))
    ((eq? c #\alarm)      (mk-ident "ALARM_CH"))
    ((eq? c #\backspace)  (mk-ident "BACKSPACE_CH"))
    ((eq? c #\page)       (mk-ident "PAGE_CH"))
    ((eq? c #\return)     (mk-ident "RET_CH"))
    ((eq? c #\tab)        (mk-ident "TAB_CH"))
    ((eq? c #\vtab)       (mk-ident "VTAB_CH"))

    (else
      (error "Unknown character" c))))

(define (comp-constant ctx ast)
  (define (handle-literal lit transform)
    (let ((val (cadr lit)))
        (cond ((exact-integer? val)
                (number->string (transform val)))
              ((string? val)
                ; Hacky way to detect character literals since six doesn't distinguish between " and '
                (if (equal? 1 (string-length val))
                  (if define-constants-for-characters?
                    (let ((ident (character-ident (string-ref val 0))))
                      (table-set! (ctx-characters ctx) (string-ref val 0) ident)
                      (string-append "$" (format-non-local-var ident)))
                    (number->string (transform (char->integer (string-ref val 0)))))
                  (error "String literals are not supported in this context")))
              (else
                "unknown literal" ast))))
  (case (car ast)
    ((six.literal) (handle-literal ast identity))
    ((six.-x)
      (handle-literal (cadr ast) (lambda (x) (- x))))
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
            (list "defglo " (global-var name) " " val))))))

; An enum is defined with a function without parameters returning a value of
; type `enum` and that has a body containing only assignments or identifiers.
; Here's an example: enum MyEnum() { A = 128; B; C; }
(define (comp-glo-define-enum ctx ast)
  (define (enum-values lst)
    (let loop ((lst lst) (counter 0) (new-local-vars '()))
      (if (pair? lst)
        (let ((decl (car lst)))
          (case (car decl)
            ((six.x=y)
              (let ((new-counter (cadr (caddr decl))))
                (loop (cdr lst) (+ new-counter 1) (cons (cons (cadadr decl) new-counter) new-local-vars))))
            ((six.identifier)
              (loop (cdr lst) (+ counter 1) (cons (cons (cadr decl) counter) new-local-vars)))
            (else
              (error "Struct definition can only contain variable declarations. Got:" decl))))
        (reverse new-local-vars))))

  (let* ((name (cadr ast))
         (proc (caddr ast))
         (parameters (caddr proc))
         (body (cdar (cdddr proc)))
         (values (enum-values body)))

    (if (not (null? parameters))
      (error "Enums can't have parameters" proc))

    ; Check that there are no conflicts with other enums
    (for-each
      (lambda (enum)
        (let ((val (table-ref (ctx-enums ctx) (car enum) #f)))
          (if (and val (not (equal? val (cdr enum))))
            (error "Enum is already defined" enum))))
      values)

    ; Check that there are no conflicts with structs
    (for-each
      (lambda (enum)
        (let ((val (table-ref (ctx-structs ctx) (car enum) #f)))
          (if (and val (not (equal? val (cdr enum))))
            (error "Enum conflicts with struct field" enum))))
      values)

    (ctx-enums-set! ctx (table-merge (list->table values) (ctx-enums ctx)))

    (ctx-add-glo-decl!
      ctx
      (list "# Defining enum " (cdr name)))

    (ctx-add-glo-decl!
      ctx
      (map (lambda (decl) (list "_" (car decl) "=" (number->string (cdr decl)) " ")) values))

    (ctx-add-glo-decl! ctx '())))

; An struct is defined with a function without parameters returning a value of
; type `struct` and that has a body containing only assignments or identifiers.
; Here's an example: struct MyStruct() { int a; int b; char c; }
(define (comp-glo-define-struct ctx ast)
  (define (enum-values lst)
    (let loop ((lst lst) (counter 0) (new-local-vars '()))
      (if (pair? lst)
        (let ((decl (car lst)))
          (case (car decl)
            ; (six.define-variable (six.identifier val) void () #f)
            ((six.define-variable)
              (loop (cdr lst) (+ counter 1) (cons (cons (cadadr decl) counter) new-local-vars)))
            (else
              (error "Struct definition can only contain variable declarations. Got:" decl))))
        (reverse new-local-vars))))

  (let* ((name (cadr ast))
         (proc (caddr ast))
         (parameters (caddr proc))
         (body (cdar (cdddr proc)))
         (values (enum-values body)))

    (if (not (null? parameters))
      (error "Struct can't have parameters" proc))

    ; Check that there aren' no conflicts with enums
    (for-each
      (lambda (struct)
        (let ((val (table-ref (ctx-enums ctx) (car struct) #f)))
          (if (and val (not (equal? val (cdr struct))))
            (error "Struct field conflicts with enum" struct))))
      values)

    ; Check that there are no conflicts with other structs
    (for-each
      (lambda (struct)
        (let ((val (table-ref (ctx-structs ctx) (car struct) #f)))
          (if (and val (not (equal? val (cdr struct))))
            (error "Struct field is already defined" struct))))
      values)

    (ctx-structs-set! ctx (table-merge (list->table values) (ctx-structs ctx)))

    (ctx-add-glo-decl!
      ctx
      (list "# Defining struct fields of " (cdr name)))

    (ctx-add-glo-decl!
      ctx
      (map (lambda (decl) (list "_" (car decl) "=" (number->string (cdr decl)) " ")) values))

    (ctx-add-glo-decl! ctx '())))

(define reserved-variable-prefix
  (map symbol->string
  '(; Special variable for zsh. If we initialize the local variables before
    ; handling $# $@, writing to argv will overwrite the arguments those values.
    argv
    ; ### Variables starting with _ ###
    ; Prefix of variables used to store the address of a string literals
    ; __str_
    ; Variables storing the argc/argv during program initialization (before main function call)
    ; __argc_for_main
    ; __argv_for_main
    ; Determine if malloc initializes the memory it allocates to 0
    ; __STRICT_MODE
    ; Determine if malloc initializes the memory it allocates to 0
    ; __FREE_UNSETS_VARS
    ; Runtime library variables
    NULL
    EOF
    )))

; Defined in some versions of Gambit but not all, so including it here.
(define (any pred lst)
  (if (null? lst) #f (if (pred (car lst)) #t (any pred (cdr lst)))))

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

; Traverse the AST and gather all the local variables that are declared in the function.
; It also keep tracks of variables that are assigned to.
(define (gather-function-var-used ast)
  (define (go ast)
    (if (not (pair? ast)) (set! ast (list 'six.literal ast)))
    (case (car ast)
      ((six.return six.break six.continue six.literal six.list six.identifier six.internal-identifier) '())
      ((six.define-variable)
        (let* ((var-name (cadr ast)))
          (list (cons var-name (make-local-var #f 'local #f)))))
      ((six.while) (go (caddr ast)))
      ((six.for)

        (merge-local-variables (go (cadr ast)) (go (car (cddddr ast)))))
      ((six.if)    (merge-local-variables (go (caddr ast)) (if (pair? (cdddr ast)) (go (cadddr ast)) '())))
      ((six.x=y six.x+=y six.x-=y six.x*=y six.x/=y six.x%=y)
        (let ((lhs (cadr ast))
              (rhs (caddr ast)))
          (merge-local-variables
            (go rhs)
            (if (equal? (car lhs) 'six.identifier)
              (list (cons lhs (make-local-var #f #f #t)))
              '()))))
      ((six.x++ six.++x six.--x six.x--)
        (let ((lhs (cadr ast)))
          (if (equal? (car lhs) 'six.identifier)
            (list (cons lhs (make-local-var #f #f #t)))
            '())))
      ((six.switch)
        (apply merge-local-variables (map go (cdaddr ast))))
      ((six.case six.label) ; (six.case (six.literal ...) (six.case (six.literal ...) ... ( ... body ...)))
        (go (caddr ast)))
      ((six.compound)
        (apply merge-local-variables (map go (cdr ast))))
      ((six.call)
        (let* ((fun-params (cdr ast)))
          (apply merge-local-variables (map go fun-params))))
      ((six.-x six.!x six.*x six.**x)
        (go (cadr ast)))
      ((six.x+y six.x-y six.x*y six.x/y six.x%y six.x==y six.x!=y six.x<y six.x>y six.x<=y six.x>=y six.x>>y six.x<<y six.x&y |six.x\|y| six.x^y six.index six.arrow six.x&&y |six.x\|\|y|)
        (merge-local-variables (go (cadr ast)) (go (caddr ast))))
      ((six.x?y:z)
        (merge-local-variables (go (cadr ast)) (go (caddr ast)) (go (cadddr ast))))
      (else
        (error "unknown ast" ast))))

  (apply append (map go ast)))

(define (comp-glo-define-procedure ctx ast)
  (let* ((name (cadr ast))
         (proc (caddr ast))
         (result-type (cadr proc))
         (parameters (caddr proc))
         (body (cdar (cdddr proc)))
         (indexed-parameters
          (map cons parameters
                    (iota (length parameters)
                          (if use-$1-for-return-loc? 2 1)))))
    (ctx-add-glo-decl!
     ctx
     (list
      (function-name name) "() {"
      (if (pair? indexed-parameters) " # " "")
      (string-concatenate (map (lambda (p) (string-append (symbol->string (cadaar p)) ": $" (number->string (cdr p)))) indexed-parameters) ", ")))
    (ctx-tail?-set! ctx #t)
    (nest ctx
      (let* ((start-loc-env (table-copy (ctx-loc-env ctx)))
             (body-decls (get-body-declarations body))
             (body-rest (car body-decls))
             (new-local-vars (cdr body-decls))
             (variables-used (gather-function-var-used body)))

        (assert-variable-names-are-safe (map car parameters))
        (assert-variable-names-are-safe (map car new-local-vars))

        (add-new-local-var ctx result-loc-ident 1)

        (for-each
          (lambda (param-and-pos) (add-new-local-var ctx (caar param-and-pos) (cdr param-and-pos)))
          indexed-parameters)

        ; Add local variables to the environment
        (for-each
          (lambda (param) (add-new-local-var ctx (car param)))
          new-local-vars)

        ; Mark local variables as written to or constant
        (ctx-loc-env-set! ctx (list->table (filter (lambda (v) (local-var-position (cdr v))) (merge-local-variables (table->list (ctx-loc-env ctx)) variables-used))))

        ; After setting the environment, we compile the body of the function
        ; and then call save-local-variables so it can save the local variables
        ; and synthetic variables that were used in the function.
        (let ((comped-body (scope-glo-decls-with-level ctx (lambda () (comp-body ctx body-rest)))))

          ; (shift-ctx-loc-env-position ctx) ; Make room for the result_loc var
          (save-local-variables ctx)
          (if (not use-$1-for-return-loc?)
            (ctx-add-glo-decl!
              ctx
              (list result-loc-var
                    "=\"$1\"; shift "
                    "# Remove result_loc param")))

          (for-each
            (lambda (p)
              (if (or (is-mutable-local-var ctx (caar p)) (not optimize-readonly-params?))
                (comp-assignment ctx `(six.x=y ,(caar p) (six.identifier ,(cdr p))))))
            indexed-parameters)

          (for-each
            (lambda (new-local-var)
              (if (cdr new-local-var)
                (comp-assignment ctx `(six.x=y ,(car new-local-var) ,(cdr new-local-var)))
                (comp-assignment ctx `(six.x=y ,(car new-local-var) (six.literal 0)))))
            new-local-vars)

          (if (null? comped-body)
            (ctx-add-glo-decl! ctx (list ":"))
            (for-each (lambda (l) (ctx-add-glo-decl! ctx (car l) (cdr l))) comped-body))

          (restore-local-variables ctx)

          (ctx-max-gensym-counter-set! ctx (max (ctx-max-gensym-counter ctx) (ctx-gensym-counter ctx))) ; Keep track of the maximum gensym counter
          (ctx-gensym-counter-set! ctx 0) ; Reset the gensym counter

          (ctx-loc-env-set! ctx start-loc-env))))

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
      (error "Variables must be defined at the top of the function" new-local-vars))

    (ctx-single-statement?-set! ctx (and (pair? lst) (null? (cdr lst))))

    (for-each
      (lambda (new-local-var)
        (add-new-local-var ctx (car new-local-var))

        (if (cdr new-local-var)
          (comp-assignment ctx `(six.x=y ,(car new-local-var) ,(cdr new-local-var)))
          (comp-assignment ctx `(six.x=y ,(car new-local-var) (six.literal 0)))))
      new-local-vars)

    (comp-statement-list ctx body-rest)

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

;; Accumulate the body of a case statement until a break or another case statement is found.
;; Note that because Shell cases always break at the end, we can't have fallthrough, and so
;; it fails if it finds another case statement before a break.
(define (gather-case-body ast)
  (let loop ((ast ast) (body '()) (last-return? #f))
    (if (pair? ast)
      (let ((tag (caar ast)))
        (case tag
          ((six.break)
           (cons (reverse body) (cdr ast)))
          ((six.case six.label)
            (if last-return?
              (cons (reverse body) ast)
              (error "Switch case fallthrough not supported")))
          ((six.return)
           (loop (cdr ast) (cons (car ast) body) #t))
          (else
           (loop (cdr ast) (cons (car ast) body) #f))))
      (cons (reverse body) '()))))

(define (gather-case-patterns ast)
  (let loop ((ast ast) (patterns '()))
    (if (and (pair? ast) (equal? 'six.case (car ast)))
      (loop (caddr ast) (cons (cadr ast) patterns))
      (cons (reverse patterns) ast))))

(define (comp-pattern ctx ast)
  (case (car ast)
    ((six.literal)
     (comp-constant ctx ast))
    ((six.identifier)
     (string-append "$" (enum-ref ctx ast)))
    (else
     (error "Unsupported pattern" ast))))

(define (gather-case-data ctx cases)
  (cond
    ((equal? 'six.case (caar cases)) ;; (six.case (six.literal ...) ...)
      (let* ((pattern-and-body-start (gather-case-patterns (car cases)))
             (patterns (map (lambda (c) (comp-pattern ctx c)) (car pattern-and-body-start)))
             (patterns-string (string-concatenate patterns "|"))
             (rest (cons (cdr pattern-and-body-start) (cdr cases))))
        (cons patterns-string rest)))
    ((and (equal? 'six.label (caar cases)) (equal? 'default (cadar cases))) ;; (six.label default ...)
      (let* ((rest (cons (caddar cases) (cdr cases))))
        (cons "*" rest)))
    (else
      (error "Unknown case" (car cases)))))

(define (comp-cases ctx cases)
  (let loop ((cases cases))
    (if (pair? cases)
      (let* ((case-data (gather-case-data ctx cases))
             (patterns-string (car case-data))
             (rest (cdr case-data))
             (body-and-new-rest (gather-case-body rest))
             (body (car body-and-new-rest))
             (new-rest (cdr body-and-new-rest))
             (comped-body (cdr (scope-glo-decls ctx (lambda () (comp-body ctx body))))))
        ; If the body is a single statement, we may be able to put the pattern and ;; on the same line
        (cond
          ((and (pair? comped-body) (null? (cdr comped-body))
                (< (string-length patterns-string) 40))
            (ctx-add-glo-decl!
              ctx
              (list patterns-string ") " (car comped-body) " ;;")))

          ((and (pair? comped-body) (null? (cdr comped-body)))
            (ctx-add-glo-decl!
              ctx
              (list patterns-string ")"))
            (nest ctx
              (ctx-add-glo-decl!
                ctx
                (list (car comped-body) " ;;"))))
          (else
            (ctx-add-glo-decl! ctx (list patterns-string ")"))
            (nest ctx
              (for-each (lambda (l) (ctx-add-glo-decl! ctx l)) comped-body)
              (ctx-add-glo-decl! ctx (list ";;")))))
        (loop new-rest)))))

(define (comp-statement ctx ast #!optional (else-if? #f))
  (case (car ast)
    ((six.while)
      (let ((code-test (comp-loop-test ctx (cadr ast)))
            (body (caddr ast))
            (start-block-type (ctx-block-type ctx)))
        (ctx-add-glo-decl!
          ctx
          (list "while " code-test " ; do"))
        (ctx-loop-nested-level-set! ctx (+ (ctx-loop-nested-level ctx) 1))
        (ctx-block-type-set! ctx 'loop)
        (nest ctx
          ;; If the body is empty, add a : to avoid syntax errors
          (if (equal? '(six.compound) body)
            (ctx-add-glo-decl! ctx (list ":")))
          (comp-statement ctx (caddr ast)))
        (ctx-loop-nested-level-set! ctx (- (ctx-loop-nested-level ctx) 1))
        (ctx-block-type-set! ctx start-block-type)
        (ctx-add-glo-decl!
          ctx
          (list "done"))))
    ((six.for)
     ;;(six.for (six.x=y (six.identifier i) (six.literal 0))     (six.x<y (six.identifier i) (six.literal 2800))    (six.x++ (six.i...
     (let ((expr1 (cadr ast))
           (expr2 (caddr ast))
           (expr3 (cadddr ast))
           (body (car (cddddr ast)))
           (start-loop-end-actions (ctx-loop-end-actions ctx))
           (start-block-type (ctx-block-type ctx)))
       (comp-statement ctx expr1)
       (let ((code-test (if expr2 (comp-loop-test ctx expr2) ":"))
             (loop-end-actions
              (if expr3
                (cdr (scope-glo-decls ctx (lambda () (comp-statement ctx expr3))))
                '())))
         (ctx-add-glo-decl!
            ctx
            (list "while " code-test " ; do"))
         (ctx-loop-nested-level-set! ctx (+ (ctx-loop-nested-level ctx) 1))
         (ctx-loop-end-actions-set! ctx loop-end-actions)
         (ctx-block-type-set! ctx 'loop)
         (nest ctx
          ;; If the body is empty, add a : to avoid syntax errors
          (if (and (equal? '(six.compound) body) (null? loop-end-actions))
            (ctx-add-glo-decl! ctx (list ":")))
          (comp-statement ctx body)
          (for-each
            (lambda (action) (ctx-add-glo-decl! ctx action))
            loop-end-actions)
         )
         (ctx-loop-nested-level-set! ctx (- (ctx-loop-nested-level ctx) 1))
         (ctx-loop-end-actions-set! ctx start-loop-end-actions)
         (ctx-block-type-set! ctx start-block-type)
         (ctx-add-glo-decl!
            ctx
            (list "done")))))
    ((six.if)
     (let* ((test (cadr ast))
            (body (caddr ast))
            (code-test (comp-if-test ctx test))
            (start-block-type (ctx-block-type ctx)))
        (ctx-add-glo-decl!
          ctx
          (list (if else-if? "elif " "if ") code-test " ; then"))
        (ctx-block-type-set! ctx 'if)
        (nest ctx
          ;; If the body is empty, add a : to avoid syntax errors
          (if (equal? '(six.compound) body)
            (ctx-add-glo-decl! ctx (list ":")))
          (comp-statement ctx body))
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
                  (nest ctx
                    ;; If the body is empty, add a : to avoid syntax errors
                    (if (equal? '(six.compound) else-block)
                      (ctx-add-glo-decl! ctx (list ":")))
                    (comp-statement ctx else-block))))))
         (ctx-block-type-set! ctx start-block-type)
         (if (not else-if?)
          (ctx-add-glo-decl!
            ctx
            (list "fi")))))
    ((six.switch)
     (let* ((control-expr (cadr ast))
            (cases (cdaddr ast))
            (control-test (comp-rvalue ctx control-expr '(switch-control)))
            (start-block-type (ctx-block-type ctx)))
         (ctx-add-glo-decl!
            ctx
            (list "case " control-test " in"))
         (ctx-block-type-set! ctx 'switch)
         (nest ctx (comp-cases ctx cases))
         (ctx-block-type-set! ctx start-block-type)
         (ctx-add-glo-decl!
          ctx
          (list "esac"))))
    ((six.return)
     (if (pair? (cdr ast))
      (ctx-add-glo-decl!
        ctx
        (list ": $(( $" (get-result-loc ctx) " = " (comp-rvalue ctx (cadr ast) '(return #t)) " ))")))
     (if (ctx-tail? ctx)
      (begin
        ; We're in a loop, so we won't fall through to the next statement without a break statement
        (if (eq? 1 (ctx-loop-nested-level ctx))
          (ctx-add-glo-decl! ctx (list "break")) ; Does this work in 2 nested loops?
          ; The block only has one statement (a return without a value), and we
          ; can't have the block empty unless it's a case statement.
          (if (and (ctx-single-statement? ctx)
                   (not (pair? (cdr ast))) ;; We haven't returned using prim_return_value
                   (not (equal? 'case (ctx-block-type ctx)))) ; Not a case statement
            (begin
              (restore-local-variables ctx)
              (ctx-add-glo-decl! ctx (list "return"))))))
      (begin
        (restore-local-variables ctx)
        (ctx-add-glo-decl! ctx (list "return")))))
    ((six.break)
      (if (eq? 0 (ctx-loop-nested-level ctx)) (error "break statement outside of a loop"))
      (ctx-add-glo-decl!
            ctx
            (list "break")))
    ((six.continue)
      (if (eq? 0 (ctx-loop-nested-level ctx)) (error "continue statement outside of a loop"))
      (if (or (not (ctx-tail? ctx)) (ctx-single-statement? ctx))
        (begin
          (for-each
            (lambda (action) (ctx-add-glo-decl! ctx action))
            (ctx-loop-end-actions ctx))
          (ctx-add-glo-decl!
            ctx
            (list "continue")))))
    (else
     (comp-statement-expr ctx ast))))

(define (comp-if-test ctx ast)
  (comp-rvalue ctx ast '(test-if)))

(define (comp-loop-test ctx ast)
  (comp-rvalue ctx ast '(test-loop)))

; Remove the variable we are assigning to from the list of local variables to save.
; Also, saving and restoring parameters on every function call will likely make the code harder to read and slower.
; Some ideas on how to reduce the number of variables saved:
;  [ ] Only save the variables that are used by the function, or used by functions called transitively.
;     - [x] Primitives
;     - [ ] For user-defined functions
;  [ ] Only restore variables lazily, so consecutive function calls don't save and restore needlessly.
;      This doesn't seem to be worth its complexity, and consecutive function calls may not be that common.
;      Note: on branch laurent/six-cc-lazy-var-restore.
;  [ ] Use a smarter data structure, so we can save and restore variables in different orders.
;  [ ] Use dynamic variables only ( $((_$i_{var_name})) ), with a counter that we increment on each function call so we don't need to save and restore them. The counter could be $1, since it's scoped locally and doesn't need to be restored
(define (save-local-variables ctx)
  (let* ((sorted-local-vars
          (list-sort (lambda (v1 v2) (< (local-var-position (cdr v1)) (local-var-position (cdr v2)))) (table->list (ctx-loc-env ctx))))
         (local-vars-ident (map car sorted-local-vars)) ; Only keep ident
         (local-var-need-to-be-saved
          (lambda (ident)
            (let ((local-var (table-ref (ctx-loc-env ctx) ident #f)))
              (and (not (equal? ident result-loc-ident))          ; The result-loc-ident is always saved by save_loc_var
                   (or (local-var-written-to local-var)           ; Save because it's a mutable variable
                       (equal? 'local (local-var-type local-var)) ; Or save because it's a local (i.e. not param)
                       (not optimize-readonly-params?))))))       ; or save because optimization is disabled
         (local-vars-to-save (filter local-var-need-to-be-saved local-vars-ident))
         (synthetic-vars (map (lambda (c) (list 'six.internal-identifier c)) (iota (ctx-gensym-counter ctx) 1))))

      (if (or (pair? local-vars-to-save) (pair? synthetic-vars) (not use-$1-for-return-loc?))
        ; Idea: Pass the value to save directly to save_loc_var instead of the variable name
        (ctx-add-glo-decl!
          ctx
          (list "save_loc_var " (string-concatenate (reverse (map (lambda (l) (env-var ctx l)) (append local-vars-to-save synthetic-vars))) " "))))))

(define (restore-local-variables ctx)
  (let* ((sorted-local-vars
          (list-sort (lambda (v1 v2) (< (local-var-position (cdr v1)) (local-var-position (cdr v2)))) (table->list (ctx-loc-env ctx))))
         (local-vars-ident (map car sorted-local-vars)) ; Only keep ident
         (local-var-need-to-be-saved
          (lambda (ident)
            (let ((local-var (table-ref (ctx-loc-env ctx) ident #f)))
              (and (not (equal? ident result-loc-ident))          ; The result-loc-ident is always saved by save_loc_var
                   (or (local-var-written-to local-var)           ; Save because it's a mutable variable
                       (equal? 'local (local-var-type local-var)) ; Or save because it's a local (i.e. not param)
                       (not optimize-readonly-params?))))))       ; or save because optimization is disabled
         (local-vars-to-save (filter local-var-need-to-be-saved local-vars-ident))
         (synthetic-vars (map (lambda (c) (list 'six.internal-identifier c)) (iota (ctx-gensym-counter ctx) 1))))

      (if (or (pair? local-vars-to-save) (pair? synthetic-vars) (not use-$1-for-return-loc?))
        (ctx-add-glo-decl!
          ctx
          (list "rest_loc_var "
                (if use-$1-for-return-loc? "$1 " "")
                (string-concatenate (map (lambda (l) (env-var ctx l)) (append local-vars-to-save synthetic-vars)) " "))))))

; Like comp-fun-call, but checks that the parameters are simple and not nested function calls.
; This function is not necessary, but we don't want to silently compile complexe function calls
; that we've tried to simplify.
(define (comp-simple-fun-call ctx ast #!optional (assign_to #f))
  (let* ((params (cdr ast))
         (func-call-params (filter (lambda (p) (and (pair? p) (equal? (car p) 'six.call))) params)))
    (if (pair? func-call-params)
      (error "Expected a simple function call, but got a nested function call" ast)
      (comp-fun-call ctx ast assign_to))))

(define (comp-fun-call ctx ast #!optional (assign_to #f))
  (let* ((name (car ast))
         (params (cdr ast))
         (code-params (map (lambda (p) (comp-rvalue ctx p `(argument))) params)))

    (let ((return-var
            (if assign_to (comp-lvalue ctx assign_to) no-result-loc-var))) ;; '|| is empty symbol. Maps to __
      (ctx-add-glo-decl!
        ctx
        (list (string-concatenate (cons (function-name name)
                                    (cons return-var
                                    code-params))
                                  " "))))))

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
     (let ((rvalue-res (comp-rvalue ctx ast '(statement-expr))))
      (if (car rvalue-res) ; If there are side-effects in the rvalue
        (ctx-add-glo-decl!
          ctx
          (list ": " (cdr rvalue-res))))))))

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
      ((six.identifier six.internal-identifier six.index six.*x six.**x six.arrow)
       (case (car rhs)
        ((six.call)
          (comp-fun-call ctx (cdr rhs) lhs))
        (else
          (let* ((simple-assignment (and (equal? (car lhs) 'six.identifier) arithmetic-assignment?))
                 (rvalue (comp-rvalue ctx rhs `(assignment ,(not simple-assignment)))))
          (if simple-assignment
            (ctx-add-glo-decl!
              ctx
              (list (comp-lvalue ctx lhs) "=" rvalue))
            (ctx-add-glo-decl!
              ctx
              (list ": $(( " (comp-lvalue ctx lhs) " = " rvalue " ))")))))))
      (else
       (error "unknown lhs" lhs)))))

(define (comp-lvalue ctx ast)
  (case (car ast)
    ((six.identifier six.internal-identifier)
     (env-var ctx ast))
    ((six.index)
     (string-append "_$((" (comp-array-lvalue ctx (cadr ast)) "+" (comp-rvalue ctx (caddr ast) '(index-lvalue #t)) "))"))
    ((six.*x)
     ; Not sure if comp-rvalue is right here
     (string-append "_" (comp-rvalue ctx (cadr ast) '(index-lvalue #f))))
    ((six.**x)
     ; Not sure if comp-rvalue is right here
     (string-append "_$(( _" (comp-rvalue ctx (cadr ast) '(index-lvalue #f)) " ))"))
    ((six.arrow)
     (string-append "_$((" (comp-array-lvalue ctx (cadr ast)) "+" (env-var ctx (caddr ast)) "))"))
    (else
     (error "unknown lvalue" ast))))

(define (comp-array-lvalue ctx ast)
  (case (car ast)
    ((six.identifier)
     (env-var ctx ast))
    ((six.*x)
     (string-append "_" (comp-rvalue ctx (cadr ast) '(index-lvalue #f))))
    (else
     (error "unknown array lvalue" ast))))

; We can't have function calls and other side effects in $(( ... )), so we need to handle them separately.
; For unconditional function calls, they are replaced with unique identifiers are returned as a list with their new identifiers.
; For pre/post-increments/decrements, we map them to a pre-side-effects and replace with the corresponding operation.
; Note that pre/post-increments/decrements of function calls are not supported.
(define (handle-side-effects ctx ast)
  (define replaced-fun-calls '())
  (define conditional-fun-calls '())
  (define pre-side-effects '())
  (define literal-inits (make-table))
  (define contains-side-effect #f)
  (define (alloc-identifier)
    (ctx-gensym-counter-set! ctx (+ (ctx-gensym-counter ctx) 1))
    (list 'six.internal-identifier (ctx-gensym-counter ctx)))

  (define (go-inplace-arithmetic-op assigned-op inplace-arith #!optional (side-effect #f))
    (if inline-inplace-arithmetic-ops
      (begin
        (set! contains-side-effect #t)
        inplace-arith)
      (begin
        (set! pre-side-effects (cons (or side-effect inplace-arith) pre-side-effects))
        assigned-op)))

  (define (go ast executes-conditionally)
    (if (not (pair? ast)) (set! ast (list 'six.literal ast)))
    (case (car ast)
      ((six.call)
        (let* ((id (alloc-identifier))
               (fun-name (car ast))
               (fun-params (cdr ast))
               (replaced-params (map (lambda (p) (go p executes-conditionally)) fun-params))
               (new-fun-call (cons fun-name replaced-params)))
          (if (and executes-conditionally (not arithmetic-conditions?))
            (set! conditional-fun-calls (cons (cons id new-fun-call) conditional-fun-calls))
            (set! replaced-fun-calls (cons (cons id new-fun-call) replaced-fun-calls)))
          id))
      ((six.literal)
        (let ((val (cadr ast)))
          (if (and inline-string-init
                  (string? val)
                  (not (equal? 1 (string-length val)))) ; We don't inline single character strings because those are interpreted as characters
            (let* ((string-variable (string->variable-name ctx val))
                   (table-val (table-ref literal-inits string-variable #f)))
              ; FIXME: This check can be removed because there are no collision by construction
              (if table-val
                (if (equal? val table-val)
                  string-variable
                  (error "Two different string literals map to same variable" string-variable val))
                (begin
                  (table-set! literal-inits string-variable val)
                  string-variable))
              )
            ast)))
      ((six.list six.identifier six.internal-identifier)
        ast)
      ((six.x+y six.x-y six.x*y six.x/y six.x%y six.x==y six.x!=y six.x<y six.x>y six.x<=y six.x>=y six.x>>y six.x<<y six.x&y |six.x\|y| six.x^y six.index six.arrow six.x=y)
        (list (car ast)
              (go (cadr ast) executes-conditionally)
              (go (caddr ast) executes-conditionally)))
      ((six.x&&y |six.x\|\|y|)
        (let* ((previous-conditional-fun-calls conditional-fun-calls)
               (_1 (set! conditional-fun-calls '()))
               ; Left side is always executed, unless the whole expression is executed conditionally.
               ; We can compile it as always executed, but it makes the Shell code less regular.
               ; Also, propagating the ast to executes-conditonally for better error messages
               (left-side (go (cadr ast) ast))
               (left-side-conditional-fun-calls conditional-fun-calls)
               (_2 (set! conditional-fun-calls '()))
               (right-side (go (caddr ast) ast))
               (right-side-conditional-fun-calls conditional-fun-calls)
               (_3 (set! previous-conditional-fun-calls '())))
        (list (car ast)
              left-side
              right-side
              left-side-conditional-fun-calls
              right-side-conditional-fun-calls)))
      ((six.x?y:z)
        (list (car ast)
              (go (cadr ast) executes-conditionally)
              (go (caddr ast) ast)
              (go (cadddr ast) ast)))
      ((six.-x six.!x six.*x six.**x)
        (list (car ast)
              (go (cadr ast) executes-conditionally)))
      ((six.++x)
        (let ((new-op (go (cadr ast) executes-conditionally)))
          (go-inplace-arithmetic-op new-op
                                    `(six.++x ,new-op))))
      ((six.--x)
        (let ((new-op (go (cadr ast) executes-conditionally)))
          (go-inplace-arithmetic-op new-op
                                    `(six.--x ,new-op))))
      ((six.x++)
        ; We map post increments to a pre-increment and replace x++ with x - 1 to compensate
        (let ((new-op (go (cadr ast) executes-conditionally)))
          (go-inplace-arithmetic-op `(six.x-y ,new-op (six.literal 1))
                                    `(six.x-y (six.++x ,new-op) (six.literal 1))
                                    `(six.++x ,new-op))))
      ((six.x--)
        ; We map post decrements to a pre-decrement and replace x-- with x + 1 to compensate
        (let ((new-op (go (cadr ast) executes-conditionally)))
          (go-inplace-arithmetic-op `(six.x+y ,new-op (six.literal 1))
                                    `(six.x+y (six.--x ,new-op) (six.literal 1))
                                    `(six.--x ,new-op))))
      ((six.x+=y six.x-=y six.x*=y six.x/=y six.x%=y)
        (let ((new-op1 (go (cadr ast) executes-conditionally)) (new-op2 (go (caddr ast) executes-conditionally)))
          (go-inplace-arithmetic-op new-op1
                                    `(,(car ast) ,new-op1 ,new-op2))))
      (else
        (error "unknown ast" ast))))

  (list (go ast #f)
        (reverse replaced-fun-calls)
        (reverse pre-side-effects)
        (table->list literal-inits)
        contains-side-effect))

(define (comp-side-effects ctx side-effects)
  (define (comp-side-effect ast)
      (case (car ast)
        ((six.++x)
          (string-append ": $((" (comp-lvalue ctx (cadr ast)) " += 1 ))"))
        ((six.--x)
          (string-append ": $((" (comp-lvalue ctx (cadr ast)) " -= 1 ))"))
        ((six.x+=y six.x-=y six.x*=y six.x/=y six.x%=y)
          (string-append ": $((" (comp-lvalue ctx (cadr ast)) (six-op-string (car ast)) (comp-rvalue ctx (caddr ast) '(pure)) "))"))
        (else
          (error "unknown side effect" ast))))

  (map comp-side-effect side-effects))

(define (comp-literal-inits ctx literal-inits)
  (define (comp-literal-init literal-init)
    (let ((id (car literal-init))
          (val (cdr literal-init)))
      (ctx-add-glo-decl! ctx (def_str_code (env-var ctx id) val))))
  (for-each comp-literal-init literal-inits))

(define (comp-rvalue ctx ast context)
  (let* ((side-effects-res (handle-side-effects ctx ast))
         (new-ast (car side-effects-res))
         (fun-calls-to-replace (cadr side-effects-res))
         (pre-side-effects (caddr side-effects-res))
         (literal-inits (cadddr side-effects-res))
         (contains-side-effect (car (cddddr side-effects-res))))
    (define (comp-side-effects-then-rvalue rvalue-context)
      (begin
        (for-each
          (lambda (e) (ctx-add-glo-decl! ctx e))
          (comp-side-effects ctx pre-side-effects))
        (comp-literal-inits ctx literal-inits)
        (for-each
          (lambda (call) (comp-fun-call ctx (cddr call) (car call)))
          fun-calls-to-replace)
        (comp-rvalue-go ctx new-ast rvalue-context '())))

    (case (car context)
      ((index-lvalue assignment)
        (comp-side-effects-then-rvalue (if (cadr context) 'in-arithmetic-expansion 'need-arithmetic-expansion)))
      ((return)
        (comp-side-effects-then-rvalue 'in-arithmetic-expansion))
      ((argument switch-control)
        (comp-side-effects-then-rvalue 'need-arithmetic-expansion))
      ((statement-expr)
        (cons contains-side-effect
              (comp-side-effects-then-rvalue 'need-arithmetic-expansion)))
      ((test-if test-loop)
        ; TODO: This can create long lines, use \ to split them.
        (let ((side-effects-and-res
                (scope-glo-decls ctx
                  (lambda () (comp-side-effects-then-rvalue (if arithmetic-conditions? 'need-arithmetic-expansion 'test))))))
          (if arithmetic-conditions?
            (string-append
              (glo-decl-unlines (cdr side-effects-and-res))
              "[ "
              (car side-effects-and-res)
              " -ne 0 ]")
            (string-append (glo-decl-unlines (cdr side-effects-and-res)) (car side-effects-and-res)))))
      ((pure)
        (if (and (null? fun-calls-to-replace) (null? pre-side-effects) (null? literal-inits))
          (comp-rvalue-go ctx new-ast 'in-arithmetic-expansion '())
          (error "function calls, side effects and string literals in pure context not supported" ast)))
      (else
        (error "unknown context tag" context)))))

(define (six-op-string op #!optional (test-operand #f))
  (case op
    ((six.x+y)      " + ")
    ((six.x-y)      " - ")
    ((six.x*y)      " * ")
    ((six.x/y)      " / ")
    ((six.x%y)      " % ")
    ((six.x>>y)     " >> ")
    ((six.x<<y)     " << ")
    ((six.x&y)      " & ")
    ((|six.x\|y|)   " | ")
    ((six.x^y)      " ^ ")
    ((six.x+=y)     " += ")
    ((six.x-=y)     " -= ")
    ((six.x*=y)     " *= ")
    ((six.x/=y)     " /= ")
    ((six.x%=y)     " %= ")
    ((six.x=y)      " = ")
    ((six.x&&y)     " && ")
    ((|six.x\|\|y|) " || ")
    ((six.x==y)     (if test-operand " -eq " " == "))
    ((six.x!=y)     (if test-operand " -ne " " != "))
    ((six.x<y)      (if test-operand " -lt " " < "))
    ((six.x>y)      (if test-operand " -gt " " > "))
    ((six.x<=y)     (if test-operand " -le " " <= "))
    ((six.x>=y)     (if test-operand " -ge " " >= "))
    (else           (error "unknown six op" op))))

; TODO: Distinguish between wrapped in $(( ... )) and wrapped in ( ... )?
(define (comp-rvalue-go ctx ast rvalue-context test-side-effects)
  (define (with-prefixed-side-effects . strs)
    (let ((comped-side-effects
            (cdr (scope-glo-decls ctx
              (lambda ()
                (for-each
                  (lambda (se) (comp-simple-fun-call ctx (cddr se) (car se)))
                  test-side-effects))))))
      (if (null? test-side-effects)
        (apply string-append strs)
        (string-append "{ "
                       (glo-decl-unlines comped-side-effects)
                       (apply string-append strs)
                       "; }"))))

  (define (wrap-if-needed parens-otherwise? . lines)
    (case rvalue-context
      ((in-arithmetic-expansion)
        (if parens-otherwise?
          (string-append "(" (apply string-append lines) ")")
          (apply string-append lines)))
      ((need-arithmetic-expansion)
        (string-append "$((" (apply string-append lines) "))"))
      ((test)
        (with-prefixed-side-effects "[ $((" (apply string-append lines) ")) -ne 0 ]"))
      (else
        (error "unknown rvalue context" rvalue-context))))

  ; Used to supports the case `if/while (c) { ... }`, where c is a variable.
  ; This is otherwise handled by wrap-if-needed, but we don't want to wrap
  ; in $(( ... )) here.
  (define (wrap-in-condition-if-needed . lines)
    (if (eq? rvalue-context 'test)
      (with-prefixed-side-effects "[ " (apply string-append lines) " -ne 0 ]")
      (apply string-append lines)))

  (if (and (not (eq? rvalue-context 'test)) (pair? test-side-effects))
    (error "Can't have test side effects in a non-test context" ast test-side-effects))

  (case (car ast)
    ((six.literal)
     (let ((val (cadr ast)))
       (cond ((exact-integer? val)
              (wrap-in-condition-if-needed (number->string val)))
             ((string? val)
              ; Hacky way to detect character literals since six doesn't distinguish between " and '
              (if (equal? 1 (string-length val))
                (if define-constants-for-characters?
                  (let ((ident (character-ident (string-ref val 0))))
                    (table-set! (ctx-characters ctx) (string-ref val 0) ident)
                    (wrap-in-condition-if-needed
                      (string-append (if (eq? rvalue-context 'in-arithmetic-expansion) "" "$") (format-non-local-var ident))))
                  (wrap-in-condition-if-needed (number->string (char->integer (string-ref val 0)))))
                (begin
                  (ctx-data-set! ctx (cons val (ctx-data ctx)))
                  (wrap-in-condition-if-needed
                    (string-append (if (eq? rvalue-context 'in-arithmetic-expansion) "" "$") (obj-ref (- (length (ctx-data ctx)) 1)))))))
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
    ((six.identifier six.internal-identifier)
      ; This may be wrong when the identifier is modified by the arithmetic operation,
      ; since the value is only read once before the arithmetic expansion is evaluated.
      (if (eq? rvalue-context 'in-arithmetic-expansion)
        (env-var ctx ast)
        (wrap-in-condition-if-needed "$" (env-var ctx ast #t) "")))
    ((six.x&&y |six.x\|\|y|)
      ; Note, this could also be compiled in a single [ ] block using -a and -o,
      ; which I think are POSIX compliant but are deprecated.
      (if (eq? rvalue-context 'test)
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
                        (if wrap-left "{ " "")
                        (comp-rvalue-go ctx (cadr ast) 'test (cadddr ast))
                        (if wrap-left "; }" "")
                        (six-op-string (car ast) #t)
                        (if wrap-right "{ " "")
                        (comp-rvalue-go ctx (caddr ast) 'test (car (cddddr ast)))
                        (if wrap-right "; }" "")))
        (begin
          ; Check if left-side-conditional-fun-calls and right-side-conditional-fun-calls are empty
          (if (or (not (null? (cadddr ast))) (not (null? (car (cddddr ast)))))
            (begin
              (display "Ast: ") (display ast) (newline)
              (error "Arithmetic with function calls in && and || not supported")))

          (wrap-if-needed #f
                          (comp-rvalue-go ctx (cadr ast) 'in-arithmetic-expansion '())
                          (six-op-string (car ast) #t)
                          (comp-rvalue-go ctx (caddr ast) 'in-arithmetic-expansion '())))))
    ((six.x==y six.x!=y six.x<y six.x>y six.x<=y six.x>=y)
      (if (equal? rvalue-context 'test)
        (with-prefixed-side-effects "[ "
                                    (comp-rvalue-go ctx (cadr ast) 'need-arithmetic-expansion '())
                                    (six-op-string (car ast) #t)
                                    (comp-rvalue-go ctx (caddr ast) 'need-arithmetic-expansion '())
                                    " ]"))
         (wrap-if-needed #t
                        (comp-rvalue-go ctx (cadr ast) 'in-arithmetic-expansion '())
                        (six-op-string (car ast))
                        (comp-rvalue-go ctx (caddr ast) 'in-arithmetic-expansion '())))
    ((six.x+y six.x-y six.x*y six.x/y six.x%y six.x>>y six.x<<y six.x&y |six.x\|y| six.x^y)
     (wrap-if-needed #t
                    (comp-rvalue-go ctx (cadr ast) 'in-arithmetic-expansion'())
                    (six-op-string (car ast))
                    (comp-rvalue-go ctx (caddr ast) 'in-arithmetic-expansion '())))
    ((six.-x)
     ; Check if the rest of ast is a literal, if so directly return the negated value
     (if (and (equal? 'six.literal (caadr ast)) (exact-integer? (cadadr ast)))
       (comp-rvalue-go ctx `(six.literal ,(- (cadadr ast))) rvalue-context test-side-effects)
       (wrap-if-needed #f (string-append "-(" (comp-rvalue-go ctx (cadr ast) 'in-arithmetic-expansion '()) ")"))))
    ((six.x?y:z)
      (wrap-if-needed #t
                      (comp-rvalue-go ctx (cadr ast) 'in-arithmetic-expansion '())
                      " ? "
                      (comp-rvalue-go ctx (caddr ast) 'in-arithmetic-expansion '())
                      " : "
                      (comp-rvalue-go ctx (cadddr ast) 'in-arithmetic-expansion '())))
    ((six.index)
     (wrap-if-needed #f "_$((" (comp-array-lvalue ctx (cadr ast)) "+" (comp-rvalue-go ctx (caddr ast) 'in-arithmetic-expansion '()) "))"))
    ((six.arrow)
     (wrap-if-needed #f "_$((" (comp-array-lvalue ctx (cadr ast)) "+" (comp-rvalue-go ctx (caddr ast) 'in-arithmetic-expansion '()) "))"))
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
     (wrap-if-needed #f "!" (comp-rvalue-go ctx (cadr ast) 'in-arithmetic-expansion '())))
    ((six.x+=y six.x-=y six.x*=y six.x/=y six.x%=y six.x=y)
     (wrap-if-needed #t (comp-lvalue ctx (cadr ast))
                        (six-op-string (car ast))
                        (comp-rvalue-go ctx (caddr ast) 'in-arithmetic-expansion '())))
    ((six.*x)
     ; Using need-arithmetic-expansion even if it's wrapped in $(( ... )) because
     ; we need another layer of wrapping if it's a complex expression, i.e. not
     ; a literal or a variable.
     (wrap-if-needed #f "_" (comp-rvalue-go ctx (cadr ast) 'need-arithmetic-expansion '())))
    ((six.**x)
     ; ^ Same reason here
     (wrap-if-needed #f "_$(( _" (comp-rvalue-go ctx (cadr ast) 'need-arithmetic-expansion '()) "))"))
    ((six.x++)
     (error "x++ should have been replaced with x - 1 (and ++x side effect) by comp-rvalue"))
    ((six.x--)
     (error "x-- should have been replaced with (x + 1) (and --x side effect) by comp-rvalue"))
    ((six.call)
      (error "Function calls not supported in rvalue"))
    (else
     (error "unknown rvalue" ast))))

(define (comp-program ast)
  (let* ((ctx (empty-ctx)))
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
  (unlines
    "set -e -u"
    ""
    "# Handle runtime options"
    (string-append strict-mode-var      "=" (if initialize-memory-when-alloc? "1" "0"))
    (string-append free-unsets-vars-var "=" (if free-unsets-variables?        "1" "0"))
    (string-append init-globals-var     "=" (if initialize-globals?           "1" "0"))
    ""
    (string-append
    "if [ $# -gt 0 ] && [ $1 = \"--malloc-init\" ] ;      then " strict-mode-var "=1; shift; fi")
    (string-append
    "if [ $# -gt 0 ] && [ $1 = \"--malloc-no-init\" ] ;   then " strict-mode-var "=0; shift; fi")
    (string-append
    "if [ $# -gt 0 ] && [ $1 = \"--free-unsets-vars\" ] ; then " free-unsets-vars-var "=1; shift; fi")
    (string-append
    "if [ $# -gt 0 ] && [ $1 = \"--free-noop\" ] ;        then " free-unsets-vars-var "=0; shift; fi")
    (string-append
    "if [ $# -gt 0 ] && [ $1 = \"--zero-globals\" ] ;     then " init-globals-var "=1; shift; fi")
    (string-append
    "if [ $# -gt 0 ] && [ $1 = \"--no-zero-globals\" ] ;  then " init-globals-var "=0; shift; fi")
    ""
    "# Load runtime library and primitives"
    ". ./runtime.sh"
    ""
    "# Local variables"
    ""
    (string-append sp-var "=0 # Note: Stack grows up, not down")
    ""
    "save_loc_var() {"
    "  __count=$#"
    (if (not use-$1-for-return-loc?)
      (unlines
      (string-append
      "  : $((" sp-var " += 1))")
      (string-append
      "  unset \"save_loc_var_$" sp-var "\" # For some reason, zsh behaves incorrectly when the variable is already set")
      "  # We must use eval to set a string to a dynamic variable"
      (string-append
      "  eval \"save_loc_var_$" sp-var "=\\$" result-loc-var "\"")))
    "  while [ $# -gt 0 ]; do"
    (string-append
    "    : $((" sp-var " += 1))")
    (string-append
    "    : $((save_loc_var_$" sp-var "=$1))")
    "    shift"
    "  done"
    "  # Save the number of arguments. Used in rest_loc_var"
    "  : $((__SP += 1))"
    "  : $((save_loc_var_$__SP=$__count))"
    "}"
    ""
    "rest_loc_var() {"
    (if use-$1-for-return-loc?
    "  __result_loc=$1; shift")
    "  __adjust=$((save_loc_var_$__SP - $#)) # Number of saved variables not being restored"
    "  : $((__SP -= 1))"
    "  while [ $# -gt 0 ]; do"
    "    # Make sure result_loc is not overwritten"
    (if use-$1-for-return-loc?
      (string-append
      "    if [ $1 != \"$__result_loc\" ]; then : $(($1=save_loc_var_$" sp-var ")); fi")
      (string-append
      "    if [ $1 != \"$" result-loc-var "\" ]; then : $(($1=save_loc_var_$" sp-var ")); fi"))
    (string-append
    "    : $((" sp-var " -= 1))")
    "    shift"
    "  done"
    (if (not use-$1-for-return-loc?)
    (unlines
    (string-append
    "  eval \"" result-loc-var "=\\$save_loc_var_$" sp-var "\"")
    (string-append
    "  : $((" sp-var " -= 1))")))
    (string-append
    "  : $((" sp-var " -= __adjust))")
    "}"
    ""
    ""
    (string-append
    "defarr() { alloc $2; : $(( $1 = __addr )) ; if [ $" init-globals-var  " -ne 0 ]; then initialize_memory $1 $2; fi; }")
    "defglo() { : $(($1 = $2)) ; }"
    ""
    "# Setup argc, argv"
    (string-append argc-var "=$(($# + 1))")
    (string-append "make_argv $" argc-var " \"$0\" $@; " argv-var "=$__argv")
    ; This must be after make_argv because if one of the local variable is argv,
    ; writing to it will overwrite the $@ array in zsh.
    (let ((local-vars
              (map format-local-var
                   (map car (table->list (ctx-all-variables ctx)))))
          (synthetic-vars
              (map (lambda (c) (internal-ref (list 'six.internal-identifier c)))
                   (iota (ctx-max-gensym-counter ctx) 1))))
      (if (not (null? local-vars))
        (unlines
          ""
          "# Initialize local vars so set -u can be used"
          (apply unlines
          (map
            (lambda (s) (string-append ": $(( " s " = 0 ))"))
            ; 107 = 120 - 13 (length of ": $(( " + " = 0 ))")
            (string-concatenate-with-breaks 107 " = " (append local-vars synthetic-vars)))))))))

; Like string-concatenate, but returns a list of string with maximum length N.
(define (string-concatenate-with-breaks N sep lst)
  (let loop ((lst lst) (acc '()))
    (if (null? lst)
      (list (string-concatenate (reverse acc) sep))
      (let ((new-acc (cons (car lst) acc)))
        (if (<= N (string-length (string-concatenate new-acc sep)))
          (cons (string-concatenate (reverse acc) sep)
                (loop lst '()))
          (loop (cdr lst) new-acc))))))

(define (runtime-postlude)
  (unlines
    ""
    (string-append result-loc-var "=__ # Dummy result location")
    "__=0 # Required for zsh" ; TODO: Investigate why this is necessary for zsh
    ""
    (string-append
    (function-name '(six.identifier main))
    " __"
    (string-append " $" argc-var)
    (string-append " $" argv-var))))

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
      (let ((new-var-name (make-string-ident (table-length (ctx-literals ctx)))))
        (table-set! (ctx-literals ctx) str new-var-name)
        new-var-name))))

; Initialize the heap, allocating memory for hardcoded strings and arrays.
; It also defines the variables for the hardcoded characters.
(define (heap-prelude ctx)
  (unlines
    (if (not (null? (ctx-data ctx)))
      (unlines
        "# Heap initialization"
        (apply unlines
          (map (lambda (datum ix)
                (cond ((list? datum)
                        (string-append
                          "unpack_array " (string-concatenate (map number->string datum) " ")
                          "; __" (number->string ix) "=$__addr"))
                      ((string? datum)
                        (def_str_code (string-append "__" (number->string ix)) datum))))
            (reverse (ctx-data ctx))
            (iota (length (ctx-data ctx)))))
        "")
      "")
  (if (> (table-length (ctx-characters ctx)) 0)
    (unlines
      "# Character constants"
      (apply unlines
        (map (lambda (char)
          (string-append (format-non-local-var (cdr char)) "=" (number->string (char->integer (car char)))))
          (list-sort (lambda (c1 c2) (char<? (car c1) (car c2))) (table->list (ctx-characters ctx)))))
      "")
    "")))

(define (comp-file path)
  (let ((ast (call-with-input-file path read-six)))
    (comp-program ast)))

(define (read-six port)
  (##six-types-set!
    '((void     . #f) (void_ptr . #f)
      (char     . #f) (char_ptr . #f) (char_ptr_ptr . #f)
      (int      . #f) (int_ptr . #f)  (int_ptr_ptr  . #f)
      (FILE_ptr . #f)
      (struct   . #f)
      (enum     . #f)))
  (let ((rt (input-port-readtable port)))
    (input-port-readtable-set! port (readtable-start-syntax-set rt 'six))
    (read-all port)))

(define (main . args)
  (for-each comp-file (parse-cmd-line args)))

; Runtime options:
; --malloc-init:                  Malloc initializes memory to zero
; --malloc-no-init:               Malloc does not initialize memory
; --free-unsets-vars:             Free unsets variables
; --free-noop:                    Free does nothing
; --zero-globals:                 Initialize global variables to zero
; --no-zero-globals:              Do not initialize global variables (faster, not compliant with C semantic)
;
; Code generation options:
; --inline-inplace-arithmetic:    Do arithmetic side effects in arithmetic expansion
; --no-inline-inplace-arithmetic: Do arithmetic side effects before the arithmetic expansion
; --prefix-local-vars:            Prefix local variables with _
; --no-prefix-local-vars:         Do not prefix local variables
; --init-string-inline:           defstr is called before using the string
; --init-string-upfront:          defstr is called during program initialization
; --use-arithmetic-conditions:    Do && and || in arithmetic expansion (!!Currently broken!!)
; --use-shell-conditions:         Do && and || using Shell's operator
; --use-arithmetic-assignment:    Assign to variables using $(( var = ... ))
; --use-regular-assignment:       Assign to variables using var=...
; --optimize-simple-functions:    Optimize local variables for simple functions
; --no-optimize-simple-functions: Treat all functions the same
; --optimize-return-loc:          Use $1 for return location
; --no-optimize-return-loc:       Use __result_loc to store the return location
; --numeric-chars:                Replace characters with their ASCII values
; --no-numeric-chars:             Replace characters with a constant containing the ASCII value
(define (parse-cmd-line args)
  (let loop ((args args) (files '()))
    (if (null? args)
        files
        (let ((arg (car args))
              (rest (cdr args)))
          (cond
                ; Runtime options
                ((equal? arg "--malloc-init")
                  (set! initialize-memory-when-alloc? #t)
                  (loop rest files))
                ((equal? arg "--malloc-no-init")
                  (set! initialize-memory-when-alloc? #f)
                  (loop rest files))
                ((equal? arg "--free-unsets-vars")
                  (set! free-unsets-variables? #t)
                  (loop rest files))
                ((equal? arg "--free-noop")
                  (set! free-unsets-variables? #f)
                  (loop rest files))
                ((equal? arg "--zero-globals")
                  (set! initialize-globals? #t)
                  (loop rest files))
                ((equal? arg "--no-zero-globals")
                  (set! initialize-globals? #f)
                  (loop rest files))

                ; Codegen options
                ((equal? arg "--inline-inplace-arithmetic")
                  (set! inline-inplace-arithmetic-ops #t)
                  (loop rest files))
                ((equal? arg "--no-inline-inplace-arithmetic")
                  (set! inline-inplace-arithmetic-ops #f)
                  (loop rest files))
                ((equal? arg "--prefix-local-vars")
                  (set! prefix-local-vars #t)
                  (loop rest files))
                ((equal? arg "--no-prefix-local-vars")
                  (set! prefix-local-vars #f)
                  (loop rest files))
                ((equal? arg "--init-string-inline")
                  (set! inline-string-init #t)
                  (loop rest files))
                ((equal? arg "--init-string-upfront")
                  (set! inline-string-init #f)
                  (loop rest files))
                ((equal? arg "--use-arithmetic-conditions")
                  (set! arithmetic-conditions? #t)
                  (loop rest files))
                ((equal? arg "--use-shell-conditions")
                  (set! arithmetic-conditions? #f)
                  (loop rest files))
                ((equal? arg "--use-arithmetic-assignment")
                  (set! arithmetic-assignment? #t)
                  (loop rest files))
                ((equal? arg "--use-regular-assignment")
                  (set! arithmetic-assignment? #f)
                  (loop rest files))
                ((equal? arg "--optimize-readonly-params")
                  (set! optimize-readonly-params? #t)
                  (loop rest files))
                ((equal? arg "--no-optimize-readonly-params")
                  (set! optimize-readonly-params? #f)
                  (loop rest files))
                ((equal? arg "--optimize-return-loc")
                  (set! use-$1-for-return-loc? #t)
                  (loop rest files))
                ((equal? arg "--no-optimize-return-loc")
                  (set! use-$1-for-return-loc? #f)
                  (loop rest files))
                ((equal? arg "--numeric-chars")
                  (set! define-constants-for-characters? #f)
                  (loop rest files))
                ((equal? arg "--no-numeric-chars")
                  (set! define-constants-for-characters? #t)
                  (loop rest files))
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
