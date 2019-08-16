#lang eopl

(define (exists? pred lst)
  (if (null? lst)
      #f
      (or (pred (car lst)) (exists? pred (cdr lst)))))

(eopl:pretty-print (exists? number? '(a b c 3 e)))
(eopl:pretty-print (exists? number? '(a b c d e)))