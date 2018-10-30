#lang racket

(define square
  (lambda (a)
    (* a a)))

(define find-smallest
  (lambda (a b c)
    (and (>= a b) (>= b c))))

(define maxSqSum
  (lambda (a b c)
     (cond ((or (find-smallest a b c) (find-smallest b a c))
           (+ (square a) (square b)))
           ((or (find-smallest a c b) (find-smallest c a b))
           (+ (square a) (square c)))
           (else (+ (square b) (square c))))))

(maxSqSum 3 2 1)