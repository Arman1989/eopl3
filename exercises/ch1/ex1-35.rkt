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
   'foo
   (interior-node
    'bar
    (leaf 26)
    (leaf 12))
   (interior-node
    'baz
    (leaf 11)
    (interior-node 'quux (leaf 117) (leaf 14)))))

(define (number-leaves t)
  (define (iter count t)
    (if (leaf? t)
        (cons (+ count 1) (leaf count))
        (let [(left-result (iter count (lson t)))]
          (let [(left-count (car left-result))
                (left-tree (cdr left-result))]
            (let [(right-result (iter left-count (rson t)))]
              (let [(right-count (car right-result))
                    (right-tree (cdr right-result))]
                (cons right-count
                      (interior-node (contents-of t) left-tree right-tree))))))))
  (cdr (iter 0 t)))

(define RTREE (number-leaves TREE))

(eopl:pretty-print (contents-of (lson (lson RTREE))))
(eopl:pretty-print (contents-of (rson (lson RTREE))))
(eopl:pretty-print (contents-of (lson (rson RTREE))))
(eopl:pretty-print (contents-of (lson (rson (rson RTREE)))))
(eopl:pretty-print (contents-of (rson (rson (rson RTREE)))))