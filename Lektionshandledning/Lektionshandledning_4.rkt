#lang racket

;Lektionshandledning 4

(define pred
  (lambda (x)
    (- x 1)))

(define succ
  (lambda (x)
    (+ x 1)))

(define even
  (lambda (x)
    (cond ((= x 0) #t)
          ((= x 1) #f)
          (else (even (pred (pred x)))))))

;alternativt

(define even?
  (lambda (n)
    (cond ((= n 0) #t)
          (else (not (even? (pred n)))))))

;12c)

;variant 1

(define odd?
  (lambda (n)
    (if (even? n)
        #f
        #t)))

;variant 2

(define odd
  (lambda (n)
    (if (= n 0)
        #f
        (even? (pred n)))))

;variant 3

(define odd??
  (lambda (n)
    (not (even n))))

;uppgift 13

;13c)

(define cut-off
  (lambda (x y)
    (cond ((= y 0) x)
          ((= x 0) 0)
          (else (cut-off (pred x) (pred y))))))

;13d)

(define min
  (lambda (x y)
    (if (= (cut-off x y) 0)
        x
        y)))