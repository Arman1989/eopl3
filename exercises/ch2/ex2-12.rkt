#lang eopl

;; An implementation of the stack data type using a procedural
;; representation.

(define (empty-stack)
  (lambda (op)
    (cond
      [(eqv? op 'empty-stack?) #t]
      [(eqv? op 'pop) (eopl:error 'pop "stack is empty")]
      [(eqv? op 'top) (eopl:error 'top "stack is empty")])))

(define (push v s)
  (lambda (op)
    (cond
      [(eqv? op 'empty-stack?) #f]
      [(eqv? op 'pop) s]
      [(eqv? op 'top) v])))

(define (empty-stack? s)
  (s 'empty-stack?))

(define (pop s)
  (s 'pop))

(define (top s)
  (s 'top))

(define STACK
  (push 'a
        (push 'b
              (push 'c (empty-stack)))))

(eopl:pretty-print (empty-stack? STACK))
(eopl:pretty-print (top STACK))
(eopl:pretty-print (top (pop STACK)))
(eopl:pretty-print (top (pop (pop STACK))))
(eopl:pretty-print (empty-stack? (pop (pop (pop STACK)))))