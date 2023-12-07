#!/usr/bin/env gsi

(define (read-six port)
  (##six-types-set! '((void . #f) (char . #f) (int . #f)))
  (let ((rt (input-port-readtable port)))
    (input-port-readtable-set! port (readtable-start-syntax-set rt 'six))
    (read-all port)))

(define (comp-program ast)
  (pp ast)) ;; TODO!

(define (comp-file path)
  (let ((ast (call-with-input-file path read-six)))
    (comp-program ast)))

(define (main . args)
  (for-each comp-file args))

;; Example:
;;
;; $ cat test2.c
;; int square(int x) {
;;   return x*x;
;; }
;; 
;; void main()
;; {
;;   int n = square(10);
;; 
;;   /* print a number to stdout */
;; 
;;   int p = 1;
;; 
;;   while (p * 10 <= n) p *= 10;
;; 
;;   while (p > 0) {
;;     int digit = n / p;
;;     putchar(48 + digit);
;;     n %= p;
;;     p /= 10;
;;   }
;; 
;;   putchar(10);
;; }
;; $ gsi six-cc.scm test2.c 
;; ((six.define-procedure
;;   (six.identifier square)
;;   (six.procedure
;;    int
;;    (((six.identifier x) int))
;;    (six.procedure-body
;;     (six.return (six.x*y (six.identifier x) (six.identifier x))))))
;;  (six.define-procedure
;;   (six.identifier main)
;;   (six.procedure
;;    void
;;    ()
;;    (six.procedure-body
;;     (six.define-variable
;;      (six.identifier n)
;;      int
;;      ()
;;      (six.call (six.identifier square) (six.literal 10)))
;;     (six.define-variable (six.identifier p) int () (six.literal 1))
;;     (six.while
;;      (six.x<=y
;;       (six.x*y (six.identifier p) (six.literal 10))
;;       (six.identifier n))
;;      (six.x*=y (six.identifier p) (six.literal 10)))
;;     (six.while
;;      (six.x>y (six.identifier p) (six.literal 0))
;;      (six.compound
;;       (six.define-variable
;;        (six.identifier digit)
;;        int
;;        ()
;;        (six.x/y (six.identifier n) (six.identifier p)))
;;       (six.call
;;        (six.identifier putchar)
;;        (six.x+y (six.literal 48) (six.identifier digit)))
;;       (six.x%=y (six.identifier n) (six.identifier p))
;;       (six.x/=y (six.identifier p) (six.literal 10))))
;;     (six.call (six.identifier putchar) (six.literal 10))))))
