#lang eopl

;; An implementation for environments
;;
;; It's based on the a-lists representation from exercise 2.5

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

;; Language and interpreter extension to include variables

(define scanner-spec
  '(;; add a regular expression and action to match for identifiers
    ;; a letter is an identifier
    (identifier (letter) symbol)
    (number (digit (arbno digit)) number)))

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
    ;; add a production for identifiers
    (factor
     (identifier)
     var)
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

;; pass in the environment so that variable lookup works
(define (interp env e)
  (cases expr e
    (sum (first ops rest)
         (reduce env first ops rest interp-additive-op interp-term))))

(define (interp-term env t)
  (cases term t
    (product (first ops rest)
             (reduce env first ops rest interp-multiplicative-op interp-factor))))

(define (reduce env x ops xs interp-op interp-x)
  (define (iter result ops xs)
    (if (null? ops)
        result
        (iter ((interp-op (car ops)) result (interp-x env (car xs)))
              (cdr ops)
              (cdr xs))))
  (iter (interp-x env x) ops xs))

(define (interp-factor env f)
  (cases factor f
    (constant (n) n)
    (var (v) (apply-env env v))
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

(define (eval env s)
  (interp env (scan&parse s)))
;; Try
;;
;; > (eval (extend-env 'x 2 (extend-env 'y 66 (empty-env))) "3+x*y-5")