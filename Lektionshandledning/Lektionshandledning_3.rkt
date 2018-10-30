#lang racket

;Lektionshandledning 3

;Uppgift 9

(define a-plus-abs-b
  (lambda (a b)
    ((if (> b 0) + -) a b)))
;if-satsen väljer operatorn.
;kommer skriva ut a + |b| men värden på a & b när vi kallar.
#|
rekursion:
Procedurer anropar sig själva.
|#

(define factorial
  (lambda (n)
    (if (<= n 1)                      ;basfall
        1
        (* n (factorial (- n 1))))))

#|
(factorial 4) -> (* 4 (factorial 3)) -> (* 4 3 (factorial 2)) -> (* 4 3 2 (factorial 1)) -> (* 4 3 2 1) = 24

svansrekursion:
|#

(define factorial-t
  (lambda (n acc)
    (if (<= n 0)
        acc
        (factorial (- n 1) (* n acc)))))
#|
(factorial-t 4 1) -> (factorial-t 3 4) -> (factorial-t 2 12) -> (factorial-t 1 24) -> 24

#lang racket

strängar:
display: skriver ut en sträng
(define message "hej")
(display message) -> hej
(write message) -> "hej"
(string-length message) -> 3
(string? message) -> #t
(string? 3) -> #f
|#

;Uppgift 10

(define succ
  (lambda (x)
    (+ x 1)))

(define pred
  (lambda (x)
    (- x 1)))

;a)
(define plus
  (lambda (a b)
    (if (= a 0)
        b
        (succ (plus (pred a) b)))))
#|
(plus 3 2) -> (+ 1 (plus 2 2)) -> (+ 1 1 (plus 1 2)) -> (+ 1 1 1 (plus 0 2)) -> (+ 1 1 1 2) -> 5
rekursiv
|#

;b
(define plus-
  (lambda (a b)
    (if (= a 0)
        b
        (plus- (pred a) (succ b)))))

#|
svansrekursiv
(plus- 3 2) -> (plus- 2 3) -> (plus- 1 4) -> (plus- 0 5) -> 5
|#

#|
Svansrekursion:
Det sista som händer är att proceduren anropar sig själv.
|#