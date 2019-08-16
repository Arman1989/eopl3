#lang eopl

(define (bst-empty? t) (null? t))
(define (bst-content t) (car t))
(define (bst-left t) (cadr t))
(define (bst-right t) (caddr t))

(define (path n t)
  (if (bst-empty? t)
      (eopl:error 'path "The binary search tree does not contain ~s.~%" n)
      (let [(m (bst-content t))]
        (cond
          [(= n m) '()]
          [(< n m) (cons 'left (path n (bst-left t)))]
          [else (cons 'right (path n (bst-right t)))]))))

(define TREE
  '(14 (7 () (12 () ()))
       (26 (20 (17 () ())
               ())
           (31 () ()))))


(eopl:pretty-print (path 12 TREE))
(eopl:pretty-print (path 17 TREE))
(eopl:pretty-print (path 31 TREE))

;(eopl:pretty-print (path 32 TREE))
; => path: The binary search tree does not contain 32.
       
