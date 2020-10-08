#lang racket

(define (down lst)
  (map list lst))

(module+ test
  (require rackunit)

  (check-equal?
   (down '(1 2 3))
   '((1) (2) (3)))

  (check-equal?
   (down '((a) (fine) (idea)))
   '(((a)) ((fine)) ((idea))))

  (check-equal?
   (down '(a (more (complicated)) object))
   '((a) ((more (complicated))) (object))))
