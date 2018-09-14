#lang racket
(define maxSqSum
  (lambda (a b c)
     (cond ((or (and (>= a b) (>= b c)) (and (>= b a) (>= a c)))
           (+ (* a a) (* b b)))
           ((or (and (>= a c) (>= c b)) (and (>= c a) (>= a b)))
           (+ (* a a) (* c c)))
           (else (+ (* b b) (* c c))))))

(maxSqSum 3 1 1)