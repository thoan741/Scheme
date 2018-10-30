#lang racket

;Föreläsning 4

(define sot
  (lambda (z)
    (define blahonga 23)
      (+ blahonga z)))

(define sum-of-square
  (lambda (x y)
    (define square
      (lambda (z)
        (* z z)))
    (+ (square x) (square y))))

(let* ((local-x 1)
      (local-y (+ local-x 9)))
    (+ local-x local-y))


