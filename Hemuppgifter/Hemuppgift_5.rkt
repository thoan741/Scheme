#lang racket

;hemuppgift 5

(define square
      (lambda (a)
        (* a a)))

(define x (list 1 2 3))
(define y (list 4 5 6))

;15b,15c,16

;uppgift 15b)

(cons x y)
;'((1 2 3) 4 5 6)





;uppgift 15c)

(list x y)
;((1 2 3) (4 5 6))




;uppgift 16)


(define square-list
  (lambda (a-list)
   (if (null? a-list)
        '()
        (cons (square (car a-list))
              (square-list (cdr a-list))))))

