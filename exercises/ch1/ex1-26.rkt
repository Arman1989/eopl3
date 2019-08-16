#lang eopl

(define (up lst)
  (define (prepend front back)
    (if (null? front)
        back
        (cons (car front) (prepend (cdr front) back))))
  (if (null? lst)
      '()
      ((if (list? (car lst)) prepend cons)
       (car lst)
       (up (cdr lst)))))

(eopl:pretty-print (up '((1 2) (3 4))))
(eopl:pretty-print (up '((x (y)) z)))
(eopl:pretty-print (up '((x ()) () y (1 2 3))))