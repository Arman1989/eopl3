#lang eopl

(define (flatten slist)
  (if (null? slist)
      '()
      (let [(sexp (car slist))]
        (if (symbol? sexp)
            (cons sexp (flatten (cdr slist)))
            (append (flatten sexp) (flatten (cdr slist)))))))

(eopl:pretty-print (flatten '(a b c)))
(eopl:pretty-print (flatten '((a) () (b ()) () (c))))
(eopl:pretty-print (flatten '((a b) c (((d)) e))))
(eopl:pretty-print (flatten '(a b (() (c)))))