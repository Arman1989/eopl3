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

(define (double-tree t)
  (if (leaf? t)
      (leaf (* 2 (contents-of t)))
      (interior-node
       (contents-of t)
       (double-tree (lson t))
       (double-tree (rson t)))))

(define DTREE (double-tree TREE))

(eopl:pretty-print (contents-of DTREE))
(eopl:pretty-print (contents-of (lson DTREE)))
(eopl:pretty-print (contents-of (lson (lson DTREE))))
(eopl:pretty-print (contents-of (rson (lson DTREE))))
(eopl:pretty-print (contents-of (lson (rson (lson DTREE)))))
(eopl:pretty-print (contents-of (rson (rson (lson DTREE)))))
(eopl:pretty-print (contents-of (rson DTREE)))
(eopl:pretty-print (contents-of (lson (rson DTREE))))
(eopl:pretty-print (contents-of (rson (rson DTREE))))