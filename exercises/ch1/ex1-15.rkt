#lang eopl

(define (duple n x)
  (if (zero? n)
      '()
      (cons x (duple (- n 1) x))))

(eopl:pretty-print (duple 2 3))
(eopl:pretty-print (duple 4 '(ha ha)))
(eopl:pretty-print (duple 0 'blah))