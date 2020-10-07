#lang racket

;; LcExp ::= Identifier
;;       ::= (lambda (Identifier) LcExp)
;;       ::= (LcExp LcExp)

;; symbol? lc-exp? . -> . boolean?
;;
;; Returns #t if the symbol var occurs free in exp, otherwise returns #f.
(define (occurs-free? var exp)
  (cond
    [(symbol? exp) (symbol=? var exp)]
    [(eq? (car exp) 'lambda)
     (and
      (not (symbol=? var (caadr exp)))
      (occurs-free? var (caddr exp)))]
    [else
     (or
      (occurs-free? var (car exp))
      (occurs-free? var (cadr exp)))]))

(module+ test
  (require rackunit)

  (check-true (occurs-free? 'x 'x))
  (check-true (occurs-free? 'x '(lambda (y) (x y))))
  (check-true (occurs-free? 'x '((lambda (x) x) (x y))))
  (check-true (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))))

  (check-false (occurs-free? 'x 'y))
  (check-false (occurs-free? 'x '(lambda (x) (x y)))))
