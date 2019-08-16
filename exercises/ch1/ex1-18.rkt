#lang eopl

(define (swapper s1 s2 slist)
  (if (null? slist)
      '()
      (cons (swapper-in-s-exp s1 s2 (car slist))
            (swapper s1 s2 (cdr slist)))))

(define (swapper-in-s-exp s1 s2 sexp)
  (if (symbol? sexp)
      (cond ((eqv? sexp s1) s2)
            ((eqv? sexp s2) s1)
            (else sexp))
      (swapper s1 s2 sexp)))

(eopl:pretty-print (swapper 'a 'd '(a b c d)))
(eopl:pretty-print (swapper 'a 'd '(a d () c d)))
(eopl:pretty-print (swapper 'x 'y '((x) y (z (x)))))