#lang racket

(provide empty-set singleton-set union-set diff-set set->list)

(define (empty-set) (set))
(define (singleton-set x) (set x))
(define (union-set a b) (set-union a b))
(define (diff-set a b) (set-subtract a b))
