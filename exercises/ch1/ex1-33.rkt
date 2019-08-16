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
   'red
   (interior-node
    'bar
    (leaf 26)
    (leaf 12))
   (interior-node
    'red
    (leaf 11)
    (interior-node 'quux (leaf 117) (leaf 14)))))

(define (mark-leaves-with-red-depth t)
  (define (mark count t)
    (if (leaf? t)
        (leaf count)
        (if (eqv? (contents-of t) 'red)
            (interior-node
             'red
             (mark (+ count 1) (lson t))
             (mark (+ count 1) (rson t)))
            (interior-node
             (contents-of t)
             (mark count (lson t))
             (mark count (rson t))))))
  (mark 0 t))

(define RTREE (mark-leaves-with-red-depth TREE))

(eopl:pretty-print (contents-of (lson (lson RTREE))))
(eopl:pretty-print (contents-of (rson (lson RTREE))))
(eopl:pretty-print (contents-of (lson (rson RTREE))))
(eopl:pretty-print (contents-of (lson (rson (rson RTREE)))))
(eopl:pretty-print (contents-of (rson (rson (rson RTREE)))))