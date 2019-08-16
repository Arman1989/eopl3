#lang eopl

(define (count-occurrences s slist)
  (define (iter count slist)
    (if (null? slist)
        count
        (let [(sexp (car slist))]
          (if (symbol? sexp)
              (if (eqv? s sexp)
                  (iter (+ count 1) (cdr slist))
                  (iter count (cdr slist)))
              (iter (+ count (iter 0 sexp))
                    (cdr slist))))))
  (iter 0 slist))

(eopl:pretty-print (count-occurrences 'x '((f x) y (((x z) x)))))
(eopl:pretty-print (count-occurrences 'x '((f x) y (((x z) () x)))))
(eopl:pretty-print (count-occurrences 'w '((f x) y (((x z) x)))))
(eopl:pretty-print (count-occurrences 'a '()))
(eopl:pretty-print (count-occurrences 'b '(a a (b (b (a))))))