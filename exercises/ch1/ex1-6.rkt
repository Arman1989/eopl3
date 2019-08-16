#lang eopl

;; Definition of nth-element as given in the book
(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

(define report-list-too-short
  (lambda (n)
    (eopl:error 'nth-element
                "List too short by ~s elements.~%" (+ n 1))))

;(eopl:pretty-print (nth-element '() 0))
(eopl:pretty-print (nth-element '(1) 0))
(eopl:pretty-print (nth-element '(1 2) 1))
(eopl:pretty-print (nth-element '(1 2 3) 2))

;; My rewrite using cond
(define nth-element-using-cond
  (lambda (lst n)
    (cond
      ((null? lst) (report-list-too-short n))
      ((zero? n) (car lst))
      (else (nth-element (cdr lst) (- n 1))))))

;(eopl:pretty-print (nth-element-using-cond '() 0))
(eopl:pretty-print (nth-element-using-cond '(1) 0))
(eopl:pretty-print (nth-element-using-cond '(1 2) 1))
(eopl:pretty-print (nth-element-using-cond '(1 2 3) 2))

;; My interpretation of what it means to reverse the tests
(define nth-element-reverse-tests
  (lambda (lst n)
    (cond
      ((zero? n) (car lst))
      ((null? lst) (report-list-too-short n))
      (else (nth-element (cdr lst) (- n 1))))))

; In this case the function would fail as usual but it would
; be because car complains. Uncomment to see how unhelpful the
; error message becomes.
;(eopl:pretty-print (nth-element-reverse-tests '() 0))
(eopl:pretty-print (nth-element-reverse-tests '(1) 0))
(eopl:pretty-print (nth-element-reverse-tests '(1 2) 1))
(eopl:pretty-print (nth-element-reverse-tests '(1 2 3) 2))