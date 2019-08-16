#lang eopl

(define (has-binding? env search-var)
  (if (null? env)
      #f
      (let [(saved-var (caar env))
            (saved-env (cdr env))]
        (or (eqv? search-var saved-var)
            (has-binding? saved-env search-var)))))
