#lang eopl

(define (product sos1 sos2)
  (if (null? sos1)
      '()
      (append (pairs (car sos1) sos2)
              (product (cdr sos1) sos2))))

(define (pairs s sos)
  (if (null? sos)
      '()
      (cons (list s (car sos))
            (pairs s (cdr sos)))))
; == (map (lambda (t) (list s t)) sos)

(eopl:pretty-print (product '(a b c) '(x y)))