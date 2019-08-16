#lang eopl

;; An implementation of environments using an a-list or association-list
;; representation.

;; env = '((var1 . val1) (var2 . val2) ... (varn . valn))

(define (empty-env)
  '())

(define (extend-env var val env)
  (cons (cons var val) env))

(define (apply-env env search-var)
  (if (null? env)
      (report-no-binding-found search-var)
      (let [(saved-var (caar env))
            (saved-val (cdar env))
            (saved-env (cdr env))]
        (if (eqv? search-var saved-var)
            saved-val
            (apply-env saved-env search-var)))))

(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No binding for ~s" search-var))

(define e
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'x 7
                                      (extend-env 'y 14
                                                  (empty-env))))))

(eopl:pretty-print (apply-env e 'd))
(eopl:pretty-print (apply-env e 'x))
(eopl:pretty-print (apply-env e 'y))
;(eopl:pretty-print (apply-env e 'z))
; => No binding for z
