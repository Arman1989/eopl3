#lang eopl

(define (list-index pred lst)
  (define (iter index lst)
    (if (null? lst)
        #f
        (let [(x (car lst))]
          (if (pred x)
              index
              (iter (+ index 1) (cdr lst))))))
  (iter 0 lst))

(eopl:pretty-print (list-index number? '(a 2 (1 3) b 7)))
(eopl:pretty-print (list-index symbol? '(a (b c) 17 foo)))
(eopl:pretty-print (list-index symbol? '(1 2 (a b) 3)))