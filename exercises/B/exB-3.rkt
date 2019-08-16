#lang eopl

(define scanner-spec
  '((number (digit (arbno digit)) number)))

(define grammar-spec
  '((expr
     (term (arbno additive-op term))
     sum)
    (term
     (factor (arbno multiplicative-op factor))
     product)
    (factor
     (number)
     constant)
    (factor
     ("(" expr ")")
     group)
    (additive-op
     ("+")
     add)
    (additive-op
     ("-")
     sub)
    (multiplicative-op
     ("*")
     mul)
    (multiplicative-op
     ("/")
     div)))

(sllgen:make-define-datatypes scanner-spec grammar-spec)

;; Uncomment to see the datatypes that get defined
;(sllgen:show-define-datatypes scanner-spec grammar-spec)

(define (interp e)
  (cases expr e
    (sum (first ops rest)
         (reduce first ops rest interp-additive-op interp-term))))

(define (interp-term t)
  (cases term t
    (product (first ops rest)
             (reduce first ops rest interp-multiplicative-op interp-factor))))

(define (reduce x ops xs interp-op interp-x)
  (define (iter result ops xs)
    (if (null? ops)
        result
        (iter ((interp-op (car ops)) result (interp-x (car xs)))
              (cdr ops)
              (cdr xs))))
  (iter (interp-x x) ops xs))

(define (interp-factor f)
  (cases factor f
    (constant (n) n)
    (group (e) (interp e))))

(define (interp-additive-op op)
  (cases additive-op op
    (add () +)
    (sub () -)))

(define (interp-multiplicative-op op)
  (cases multiplicative-op op
    (mul () *)
    (div () /)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar-spec))

(define (eval s)
  (interp (scan&parse s)))
;; Try
;;
;; > (eval "3+2*66-5")

(define repl
  (sllgen:make-rep-loop
   "--> "
   interp
   (sllgen:make-stream-parser scanner-spec grammar-spec)))