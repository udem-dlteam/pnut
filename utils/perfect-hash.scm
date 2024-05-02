;; Compile and execute with:
;;
;; $ gsc -exe perfect-hash.scm
;; $ ./perfect-hash

(declare (standard-bindings) (extended-bindings) (not safe) (fixnum) (block))

(define keywords
  (map symbol->string (read-all (open-input-file "keywords.txt"))))

(define param 1)

(define (hash-string str n)
  (let loop ((h 0) (i 0))
    (if (< i (string-length str))
        (loop (modulo
               (+ (bitwise-xor h param)
                  (char->integer
                   (string-ref str i)))
               n)
              (+ i 1))
        h)))

(define (make-htable n)
  (make-vector n #f))

(define (htable-member ht key)
  (let* ((n (vector-length ht))
         (i (hash-string key n))
         (x (vector-ref ht i)))
    (and x
         (string=? x key)
         i)))

(define (htable-add! ht key)
  (let* ((n (vector-length ht))
         (i (hash-string key n))
         (x (vector-ref ht i)))
    (if x
        #f
        (begin
          (vector-set! ht i key)
          #t))))

(define (list->htable elems)
  (let loop1 ((n (##smallest-prime-no-less-than (length elems))))
    (let ((limit (expt n 3)))
      (let loop2 ((p n))
        (if (> p limit)
            (loop1 (##smallest-prime-no-less-than (+ n 1)))
            (begin
              (set! param p)
              (let ((ht (make-htable n)))
                (let loop3 ((lst elems))
                  (cond ((null? lst)
                         ht)
                        ((htable-add! ht (car lst))
                         (loop3 (cdr lst)))
                        (else
                         (loop2 (+ p 1))))))))))))

(let* ((ht (list->htable keywords))
       (n (vector-length ht)))
  (pp (list 'n= n))
  (pp (list 'param= param))
  (for-each (lambda (i k)
              (if k
                  (begin
                    (write i) (display " ") (write k) (newline))))
            (iota n)
            (vector->list ht)))
