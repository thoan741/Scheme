#lang racket
#|
(define +rat
  (lambda (x y)
    (cons (+ (* (car x) (cdr y))
             (* (car y) (cdr x)))
          (* (cdr x) (cdr y)))))

(define *rat
  (lambda (x y)
    (cons (* (car x) (car y))
          (* (cadr x) (cadr y)))))
|#

#|
(define x (list 2 3))
(define y (list 3 4))

(define make-rat
  (lambda (numer denom)
    (list numer denom)))
|#

(define numer
  (lambda (rat)
    (car rat)))

(define denom
  (lambda (rat)
    (cadr rat)))
#|
(define +rat
  (lambda (x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))))

(define *rat
  (lambda (x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y)))))

(define make-rat
  (lambda (numer denom)
    (cons numer denom)))

(define denom
  (lambda (rat)
    (cdr rat)))

(define numer
  (lambda (rat)
    (let ((g (gcd (car rat) (cdr rat))))
      (/ (car rat) g))))

(define denom
  (lambda (rat)
    (let ((g (gcd (car rat) (cdr rat))))
      (/ (cdr rat) g))))
|#
(define make-rat
  (lambda (numer denom)
    (let ((g (gcd numer denom)))
    (cons (/ numer g)
          (/ denom g)))))

(define -rat
  (lambda (x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))))

(define /rat
  (lambda (x y)
    (make-rat (* (numer x) (denom y))
              (* (numer y) (denom x)))))

(define =rat
  (lambda (x y)
    (= (* (numer x) (denom y))
       (* (denom x) (numer y)))))

(define print-rat
  (lambda (x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x))))


;tentamenuppgift
;(spelplan 6 5) -> (list (list '- '- '- '- '-) (list '- '- '- '- '-) (list '- '- '- '- '-) (list '- '- '- '- '-) (list '- '- '- '- '-) (list '- '- '- '- '-))

(define make-col
  (lambda (cols)
    (if (= cols 0)
        '()
        (cons '- (make-col (- cols 1))))))

(define spelplan
  (lambda (rows cols)
    (if (= 0 rows)
        '()
        (cons (make-col cols) (spelplan (- rows 1) cols)))))



    