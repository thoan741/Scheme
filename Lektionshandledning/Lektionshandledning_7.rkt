#lang racket

;Lektionshandledning 7

;18 & 19

;uppgift 18:

#|
I ett försök att skriva squarelist svansrekursivt har följande procedur kommit till världen:
(define square-list
  (lambda (x)
    (define iter
      (lambda (list answer)
        (if (null? list)
            answer
            (iter (cdr list)
                  (cons (square (car list))
                        answer)))))
    (iter x '())))

>(iter '(1 2 3) '())
>(iter '(2 3) 1.'())
>(iter '(3) 4.1.'())
>(iter '() 9.4.1.'())

Den fungerar inte så bra eftersom resultatet kommer reverserat jämfört med vad man väntar sig. Förklara varför. I ett försök att rätta till saken införs i stället följande
(define square-list
  (lambda (x)
    (define iter
      (lambda (list answer)
        (if (null? list)
            answer
            (iter (cdr list)
                  (cons answer
                        (square (car list)))))))
    (iter x '())))

>(iter '(1 2 3) '())
>(iter '(2 3) '().1)
>(iter '(3) '().1.4)
>(iter '() '().1.4.9)

Lösning:
(define square-list-working
   (lambda (x)
      (reverse (square-list x))))
Där square-list är den från det övre exemplet.

Det fungerar inte heller något vidare. Förklara varför.

|#

;uppgift 19

(define mean
  (lambda (lst)
    (define sum
      (lambda (x)
        (if (null? x)
            0
            (+ (car x) (sum (cdr x))))))
    (define length
      (lambda (x)
        (if (null? x)
            0
            (+ 1 (length (cdr x))))))
    (/ (sum lst) (length lst))))

;svansrekursiv variant
(define Mean
  (lambda (lst)
    (define sum
      (lambda (x acc)
        (if (null? x)
            acc
            (sum (cdr x) (+ acc (car x))))))
    (define length
      (lambda (x acc)
        (if (null? x)
            acc
            (length (cdr x) (+ acc 1)))))
    (/ (sum lst 0) (length lst 0))))

