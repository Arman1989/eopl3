#lang racket

(define (empty-env) (list 'empty-env))
(define (extend-env var val env) (list 'extend-env var val env))

(define (apply-env env search-var)
  (match env
    [(list 'empty-env)
     (error 'apply-env "No binding for ~s" search-var)]
    [(list 'extend-env saved-var saved-val saved-env)
     (if (eqv? search-var saved-var)
         saved-val
         (apply-env saved-env search-var))]
    [_ (error 'apply-env "Bad environment: ~s" env)]))

;; Using the implementation from Figure 2.1 in the book:
;;
;; > (apply-env (list 'extend-env 'd) 'd)
;;
;; caddr: contract violation
;; expected: (cons/c any/c (cons/c any/c pair?))
;; given: '(extend-env d)
;;
;; Using the implementation above:
;;
;; > (apply-env (list 'extend-env 'd) 'd)
;;
;; apply-env: Bad environment: (extend-env d)
