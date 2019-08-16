#lang eopl

(define (down lst)
  (map (lambda (x) (cons x '())) lst))

(eopl:pretty-print (down '(1 2 3)))
(eopl:pretty-print (down '((a) (fine) (idea))))
(eopl:pretty-print (down '(a (more (complicated)) object)))