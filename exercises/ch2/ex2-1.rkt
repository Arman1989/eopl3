#lang eopl

(define N 10)

(define (zero) '())
(define (is-zero? n) (null? n))
(define (successor n)
  (if (null? n)
      '(1)
      (let [(bigit (+ (car n) 1))]
        (if (eqv? bigit N)
            (cons 0 (successor (cdr n)))
            (cons bigit (cdr n))))))
(define (predecessor n)
  (cond
    [(null? n)
     (eopl:error 'predecessor "0 does not have a predecessor")]
    [(null? (cdr n))
     (let [(bigit (- (car n) 1))]
       (if (zero? bigit)
           '()
           (list bigit)))]
    [else
     (if (zero? (car n))
         (cons (- N 1) (predecessor (cdr n)))
         (cons (- (car n) 1) (cdr n)))]))

(define (add m n)
  (if (is-zero? m)
      n
      (successor (add (predecessor m) n))))

(define (mult m n)
  (if (is-zero? m)
      m
      (add (mult (predecessor m) n) n)))

(define one
  (successor (zero)))

(define ten
  (successor
   (successor
    (successor
     (successor
      (successor
       (successor
        (successor
         (successor
          (successor
           (successor (zero))))))))))))

(define (factorial n)
  (if (is-zero? n)
      one
      (mult n (factorial (predecessor n)))))

; 0! = 1
(eopl:pretty-print (factorial (zero)))
; 1! = 1
(eopl:pretty-print (factorial one))
; 2! = 2
(eopl:pretty-print (factorial (successor one)))
; 3! = 6
(eopl:pretty-print (factorial (successor (successor one))))
; 4! = 24
(eopl:pretty-print (factorial (successor (successor (successor one)))))
; 5! = 120
(eopl:pretty-print (factorial (successor (successor (successor (successor one))))))

;; Q: How does the execution time vary as the argument to factorial changes?
;;
;; A: For a fixed base, N:
;;
;; As the argument to factorial increases the execution time increases because the
;; computation uses larger numbers with more bigits. And, as the argument to
;; factorial decreases the execution time decreases because the computation uses
;; smaller numbers with less bigits.

;; Q: How does the execution time vary as the base changes?
;;
;; A: For a fixed non-negative integer, n:
;;
;; As the base increases the execution time to compute (factorial n) decreases
;; because the number of bigits used to represent n decreases. And, as the base
;; decreases the execution time to compute (factorial n) increases because the
;; number of bigits used to represent n increases.