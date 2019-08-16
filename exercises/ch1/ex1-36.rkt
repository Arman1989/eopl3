#lang eopl

(define (number-elements lst)
  (if (null? lst)
      '()
      (g (list 0 (car lst))
         (number-elements (cdr lst)))))

(define (g first rest)
  (define (iter index lst)
    (if (null? lst)
        '()
        (cons (list index (cadr (car lst)))
              (iter (+ index 1) (cdr lst)))))
  (iter 0 (cons first rest)))

(eopl:pretty-print (number-elements '(a b c d e f)))