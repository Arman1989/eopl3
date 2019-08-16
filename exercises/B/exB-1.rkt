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

(define just-scan
  (sllgen:make-string-scanner scanner-spec grammar-spec))
;; Try:
;;
;; > (just-scan "3+2*66-5")

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar-spec))
;; Try:
;;
;; > (scan&parse "3+2*66-5")