#lang eopl

(define nth-element
  (lambda (lst n)
    (define iter
      (lambda (current-lst current-n)
        (if (null? current-lst)
            (report-list-too-short lst n)
            (if (zero? current-n)
                (car current-lst)
                (iter (cdr current-lst) (- current-n 1))))))
    (iter lst n)))

(define report-list-too-short
  (lambda (lst n)
    (if (zero? n)
        (eopl:error 'nth-element
                    "~s does not have 1 element.~%" lst)
        (eopl:error 'nth-element
                    "~s does not have ~s elements.~%" lst (+ n 1)))))

;(nth-element '() 0)
; => () does not have 1 element

;(nth-element '(a b c) 7)
; => (a b c) does not have 8 elements