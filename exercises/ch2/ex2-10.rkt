#lang eopl

(define (empty-env)
  '())

(define (extend-env var val env)
  (cons (cons var val) env))

(define (extend-env* vars vals env)
  ; assumes (length vars) = (length vals)
  (if (null? vars)
      env
      (extend-env*
       (cdr vars)
       (cdr vals)
       (extend-env (car vars) (car vals) env))))

(eopl:pretty-print (extend-env* '(y x y d) '(14 7 8 6) (empty-env)))