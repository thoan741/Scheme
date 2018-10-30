#lang racket

;Föreläsning 7

#|
Anonyma procedurer:
En anonym procedur kan direkt användas i en procedurapplikation:

(define square
  (lambda (x)
    (* x x)))

((lambda (x) (* x x)) 3)

;f(i) = i
(define sum-int
  (lambda (a b)
    (if (> a b)
        0
        (+ a (sum-int (+ a 1) b)))))

;f(i) = i^2
(define sum-square
  (lambda (a b)
    (if (> a b)
        0
        (+ (square a) (sum-square (+ a 1) b)))))

;f(i) = 1/i
(define sum-harm
  (lambda (a b)
    (if (> a b)
        0
        (+ (/ 1 a) (sum-harm (+ a 1) b)))))

;alla tidigare kan användas mha:

(define sum
(lambda (a b f)
(if (> a b)
0
(+ (f a) (sum (+ a 1) b f)))))


;definierar Sum-int mha sum, där vi skickar med vad f ska göra
(define Sum-int
  (lambda (a b)
    (sum a b (lambda (x) x))))

;definierar Sum-square mha sum, där vi skickar med vad f ska göra
(define Sum-square
  (lambda (a b)
    (sum a b (lambda (x) (* x x)))))

(define p1
  (lambda (a p)
    (p a)))

(define p2
  (lambda (x)
    (+ x 1)))

(define sum-odd
  (lambda (a b)
    (if (> a b)
        0
        (+ a (sum-odd (+ a 2) b)))))
|#
;vi kan definiera om sum, om vi definierar term och next

(define sum
  (lambda (a b term next)
    (if (> a b)
        0
        (+ (term a)
           (sum (next a) b term next)))))


;Då kan sum-int skrivas
(define sum-int
  (lambda (a b)
    (sum a b (lambda (x) x)
         (lambda (a) (+ a 1)))))

;sum-square kan skrivas
(define sum-square
  (lambda (a b)
    (sum a b (lambda (x) (* x x))
         (lambda (a) (+ a 1)))))

;sum-harm kan skrivas
(define sum-harm
  (lambda (a b)
    (sum a b (lambda (x) (/ 1 x))
         (lambda (a) (+ a 1)))))

;sum-odd kan skrivas
(define sum-odd
  (lambda (a b)
    (sum a b (lambda (x) x)
         (lambda (a) (+ a 2)))))

(define term
  (lambda (a)
    (/ 1 (* (+ (* 4 a) 1) (+ (* 4 a) 3)))))

(define next
  (lambda (a) (+ a 1)))

(define min-pi
  (lambda (n)
    (* 8 (sum 0 n term next))))


(define antal-help
  (lambda (lista n)
    (cond ((null? (cdr lista)) '())
          ((eq? (car lista) (cadr lista))
           (antal-help (cdr lista) (+ n 1)))
          (else (list n (car lista))))))

(define antal
  (lambda (list)
    (antal-help list 1)))


(define remove
  (lambda (lista)
    (cond ((null? (cdr lista)) '())
          ((= (car lista) (cadr lista))
           (remove (cdr lista)))
          (else cdr lista))))

(define look&say
  (lambda (lista)
    (cond ((null? lista) '())
          (cons (antal lista) (look&say (remove lista))))))

(define Antal
  (lambda (tal lista)
    (cond ((null? lista) 0)
          ((= tal (car lista))
           (+ 1 (Antal tal (cdr lista))))
          (else 0))))

(define Look&say
  (lambda (lst)
    (if (null? lst)
        '()
        (append (list (Antal (car lst) lst) (car lst))
              (Look&say (resten (Antal (car lst) lst) lst))))))

(define resten
  (lambda (tal lst)
    (if (= tal 0)
        lst
        (resten (- tal 1) (cdr lst)))))

