#lang eopl

(define (list-set lst n x)
  (if (null? lst)
      (eopl:error 'list-set
                  "List is too short.~%")
      (if (zero? n)
          (cons x (cdr lst))
          (cons (car lst) (list-set (cdr lst) (- n 1) x)))))

(eopl:pretty-print (list-set '(a b c d) 2 '(1 2)))
(eopl:pretty-print (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3))
; (list-set '() 0 'x)
; => List is too short.