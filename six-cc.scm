#!/usr/bin/env gsi

(define support-addr-of? #t)

(define-type ctx
  glo-decls)

(define (ctx-add-glo-decl! ctx decl)
  (ctx-glo-decls-set! ctx (cons decl (ctx-glo-decls ctx))))

(define (global-var ident)
  (string-append "_" (symbol->string (cadr ident))))

(define (global-ref ident)
  (if support-addr-of?
      (string-append "_$" (global-var ident))
      (global-var ident)))

(define (comp-glo-decl ctx ast)
  (case (car ast)

    ((six.define-variable)
     (comp-glo-define-variable ctx ast))

    ((six.define-procedure)
     (comp-glo-define-procedure ctx ast))

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
        (error "arrays not yet supported")
        (let ((val (if init (comp-constant ctx init) "0")))
          (ctx-add-glo-decl!
           ctx
           (list "defglo " (global-var name) " " val))))))

(define (comp-glo-define-procedure ctx ast)
  (let* ((name (cadr ast))
         (proc (caddr ast))
         (result-type (cadr proc))
         (parameters (caddr proc)))
    (print "#") (write ast) (newline)))

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
      ((six.identifier)
       (ctx-add-glo-decl!
        ctx
        (list ": $(( " (global-ref lhs) " = " (comp-rvalue ctx rhs) " ))")))
      (else
       (error "unknown lhs" lhs)))))

(define (comp-lvalue ctx ast)
  (case (car ast)
    ((six.identifier)
     (global-ref ast))
    (else
     (error "unknown lvalue" ast))))

(define (comp-rvalue ctx ast)
  (case (car ast)
    ((six.literal)
     (let ((val (cadr ast)))
       (cond ((exact-integer? val)
              (number->string val))
             (else
              "unknown literal" ast))))
    ((six.identifier)
     (global-ref ast))
    ((six.x+y six.x-y six.x*y)
     (string-append "("
                    (comp-rvalue ctx (cadr ast))
                    (case (car ast)
                      ((six.x+y) " + ")
                      ((six.x-y) " - ")
                      ((six.x*y) " * "))
                    (comp-rvalue ctx (caddr ast))
                    ")"))
    (else
     (error "unknown rvalue" ast))))

(define (comp-program ast)
  (let ((ctx (make-ctx '())))
    (comp-glo-decl-list ctx ast)
    (codegen ctx)))

(define (codegen ctx)
  (println runtime-prelude)
  (for-each (lambda (decl)
              (println decl))
            (reverse (ctx-glo-decls ctx)))
  (println runtime-postlude))

(define runtime-prelude
  (if support-addr-of?
      (string-append
       "defglo() { : $((ALLOC = ALLOC+1)) $(($1 = ALLOC)) $((_$ALLOC = $2)) ; }\n")
      (string-append
       "defglo() { : $(($1 = $2)) ; }\n")))

(define runtime-postlude
  (string-append
   "printf \"%d\\n\" $(( " (global-ref '(six.identifier n)) " ))\n"))

(define (comp-file path)
  (let ((ast (call-with-input-file path read-six)))
    (comp-program ast)))

(define (read-six port)
  (##six-types-set! '((void . #f) (char . #f) (int . #f)))
  (let ((rt (input-port-readtable port)))
    (input-port-readtable-set! port (readtable-start-syntax-set rt 'six))
    (read-all port)))

(define (main . args)
  (for-each comp-file args))

;; Example:
;;
;; $ cat test3.c
;; int n = 6;
;; 
;; n = n*7;
;; 
;; ---------- with: (define support-addr-of? #f)
;; $ gsi six-cc.scm test3.c 
;; defglo() { : $(($1 = $2)) ; }
;; 
;; defglo _n 6
;; : $(( _n = (_n * 7) ))
;; printf "%d\n" $(( _n ))
;; 
;; $ gsi six-cc.scm test3.c | sh
;; 42
;; 
;; ---------- with: (define support-addr-of? #t)
;; $ gsi six-cc.scm test3.c 
;; defglo() { : $((ALLOC = ALLOC+1)) $(($1 = ALLOC)) $((_$ALLOC = $2)) ; }
;; 
;; defglo _n 6
;; : $(( _$_n = (_$_n * 7) ))
;; printf "%d\n" $(( _$_n ))
;; 
;; $ gsi six-cc.scm test3.c | sh
;; 42
