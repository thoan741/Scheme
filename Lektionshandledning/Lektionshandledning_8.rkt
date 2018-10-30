#lang racket


(define (square x)
  (* x x))
;Lektionshandledning 8

(range 5) ;skapar en lista '(0 1 2 3 4)
(range 1 5) ;skapar en lista, men börjar på 1. tar bort elle element och ger '(1 2 3 4)
(range 7 3 -1) ;går från 7 till 3, men inte till trean med steg -1. '(7 6 5 4)
(identity 2) ;=> 2, returnerar det värde vi ger den
(identity #t) ;=> #t

(define id
  (lambda (x) x))
(add1 2) ;=>3 (2+1)
(sub1 2) ;=>1 (2-1)

(map (lambda (x) (* x x)) '(1 2 3)) ;applicerar x*x på varje element i listan.

(define (deep-reverse lst)
  (if (list? lst)
      (reverse (map deep-reverse lst))
      lst))

;uppgift 28

;Proceduren sum är bara en av de enklaste av en mängd liknande abstraktioner (generaliseringar) som kan definieras som högre ordningens procedurer.

;a)
;Skriv en analog procedur product som returnerar produkten av alla funktionsvärden (för en given funktion) i punkter i ett intervall.
;Visa hur man kan definiera fact (factorial) i termer av product.

(define sum
  (lambda (a b term next)
    (if (> a b)
        0
        (+ (term a)
           (sum (next a) b term next)))))

(sum 1 4 (lambda (x)x) (lambda (a) (+ a 1))) ;=> 10 (1+2+3+4)
(sum 1 4 identity add1) ;gör samma som ovan
(sum 2 1000 square (lambda (a) (+ a 2))) ;adderar kvadraten av alla jämna tal mellan 2 och 1000

;b)

(define product
  (lambda (a b term next)
    (if (> a b)
        1
        (* (term a)
           (product (next a) b term next)))))

;fakultet
;(product 1 n identity add1) (n!)
(product 1 5 identity add1)

;definiera formeln pi/4= (2*4*4*6*6*...)/(3*3*5*5*7*7...)

(define make-even
  (lambda (n)
    (if (odd? n)
        (+ 1 n)
        n)))

(define make-odd
  (lambda (n)
    (if (even? n)
        (+ 1 n)
        n)))

(define pi-approx
  (lambda (iterations)
    (* (/ (product 2 iterations make-even add1)
          (product 2 iterations make-odd add1))
       4.0)))

;Uppgift 29

;(accumulate combiner null-value a b term next)

(define accumulate
  (lambda (combiner null-value a b term next)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value (next a) b term next)))))
(display "summa mellan 1-100:")
(accumulate + 0 1 100 identity add1)