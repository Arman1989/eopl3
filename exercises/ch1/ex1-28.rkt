#lang eopl

(define (merge loi1 loi2)
  (cond
    [(null? loi1) loi2]
    [(null? loi2) loi1]
    [else
     (let [(a (car loi1))
           (b (car loi2))]
       (if (<= a b)
           (cons a (merge (cdr loi1) loi2))
           (cons b (merge loi1 (cdr loi2)))))]))

(eopl:pretty-print (merge '(1 4) '(1 2 8)))
(eopl:pretty-print (merge '(35 62 81 90 91) '(3 83 85 90)))