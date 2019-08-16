#lang eopl

(define (leaf n)
  (cons 'leaf n))
(define (interior-node s left right)
  (cons 'interior-node (cons s (cons left right))))

(define (leaf? t)
  (and (pair? t) (eqv? (car t) 'leaf)))

(define (lson inode)
  (caddr inode))
(define (rson inode)
  (cdddr inode))

(define (contents-of t)
  (if (leaf? t)
      (cdr t)
      (cadr t)))

(define TREE
  (interior-node
   'baz
   (interior-node
    'bar
    (leaf 1)
    (interior-node 'foo (leaf 1) (leaf 2)))
   (interior-node 'biz (leaf 4) (leaf 5))))

(eopl:pretty-print (contents-of TREE))
(eopl:pretty-print (contents-of (lson TREE)))
(eopl:pretty-print (contents-of (lson (lson TREE))))
(eopl:pretty-print (contents-of (rson (lson TREE))))
(eopl:pretty-print (contents-of (lson (rson (lson TREE)))))
(eopl:pretty-print (contents-of (rson (rson (lson TREE)))))
(eopl:pretty-print (contents-of (rson TREE)))
(eopl:pretty-print (contents-of (lson (rson TREE))))
(eopl:pretty-print (contents-of (rson (rson TREE))))