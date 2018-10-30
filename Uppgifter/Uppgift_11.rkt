#lang racket

(displayln "Uppgift 11")
(newline)

;Implementera den rekursiva funktionen nedan i Scheme. 

;f(n)={0 om n=0, 2 om n=1,37 om n=2, f(n−3)+1 om n är udda f(n/2−1)+7annars

(define f
  (lambda (n)
    (f-svans n 0)))

(define f-svans
  (lambda (n ans)
    (cond ((= n 0) ans)
          ((= n 1) (+ ans 2))
          ((= n 2) (+ ans 37))
          ((odd? n) (f-svans (- n 3) (+ ans 1)))
          (else (f-svans (- (/ n 2) 1) (+ ans 7))))))