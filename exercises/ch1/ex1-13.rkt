#lang eopl

; subst : Sym x Sym x S-list -> S-list
(define subst
  (lambda (new old slist)
    (map (lambda (sexp) (subst-in-s-exp new old sexp)) slist)))

; subst-in-s-exp : Sym x Sym x S-exp -> S-exp
(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))

(eopl:pretty-print "Testing subst")

(eopl:pretty-print
 (subst 'y 'x '()))

(eopl:pretty-print
 (subst 'y 'x '(x x (a b (c (x))))))

;; We can also inline the definition of subst-in-s-exp as follows:

(define subst2
  (lambda (new old slist)
    (map (lambda (sexp)
           (if (symbol? sexp)
               (if (eqv? sexp old) new sexp)
               (subst2 new old sexp)))
         slist)))

(eopl:pretty-print "Testing subst2")

(eopl:pretty-print
 (subst 'y 'x '()))

(eopl:pretty-print
 (subst 'y 'x '(x x (a b (c (x))))))