#lang eopl

(define (every? pred lst)
  (if (null? lst)
      #t
      (and (pred (car lst)) (every? pred (cdr lst)))))

(eopl:pretty-print (every? number? '(a b c 3 e)))
(eopl:pretty-print (every? number? '(1 2 3 5 4)))