#lang eopl

(define (filter-in pred lst)
  (if (null? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (filter-in pred (cdr lst)))
          (filter-in pred (cdr lst)))))

(eopl:pretty-print (filter-in number? '(a 2 (1 3) b 7)))
(eopl:pretty-print (filter-in symbol? '(a (b c) 17 foo)))