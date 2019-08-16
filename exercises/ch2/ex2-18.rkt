#lang eopl

(define (number->sequence n)
  (list n '() '()))

(define (current-element seq)
  (car seq))

; Helpers

(define (sequence->left seq)
  (cadr seq))

(define (sequence->right seq)
  (caddr seq))

; More procedures

(define (move-to-left seq)
  (let [(current (current-element seq))
        (left (sequence->left seq))
        (right (sequence->right seq))]
    (if (null? left)
        (eopl:error 'move-to-left "cannot move left anymore")
        (list (car left)
              (cdr left)
              (cons current right)))))

(define (move-to-right seq)
  (let [(current (current-element seq))
        (left (sequence->left seq))
        (right (sequence->right seq))]
    (if (null? right)
        (eopl:error 'move-to-right "cannot move right anymore")
        (list (car right)
              (cons current left)
              (cdr right)))))

(define (insert-to-left n seq)
  (list (current-element seq)
        (cons n (sequence->left seq))
        (sequence->right seq)))

(define (insert-to-right n seq)
  (list (current-element seq)
        (sequence->left seq)
        (cons n (sequence->right seq))))

(define (at-left-end? seq)
  (null? (sequence->left seq)))

(define (at-right-end? seq)
  (null? (sequence->right seq)))

(eopl:pretty-print (move-to-left '(6 (5 4 3 2 1) (7 8 9))))
(eopl:pretty-print (move-to-right '(6 (5 4 3 2 1) (7 8 9))))
(eopl:pretty-print (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))))
(eopl:pretty-print (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))))
(eopl:pretty-print (at-left-end? (number->sequence 1)))
(eopl:pretty-print (at-right-end? (number->sequence 1)))