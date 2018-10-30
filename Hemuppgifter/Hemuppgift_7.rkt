#lang racket

;Uppgift 20)

;minimumvärde i en lista

(define minimum-value
  (lambda (lst)
    (define minimum-value-inner
      (lambda (lst value)
        (cond ((null? (cdr lst)) value)
              ((< (car list) (cadr lst)) (minimum-value-inner (cdr lst) value))
              (else (minimum-value-inner (cdr lst) (cadr lst))))))
      (minimum-value-inner lst (car lst))))

;alternativt

(define minimum
  (lambda (lst)
    (cond ((null? (cdr lst)) (car lst))
          ((< (car lst) (minimum (cdr lst))) (car lst))
          (else (minimum (cdr lst))))))
;Uppgift 21)

;Definiera en procedur filter som tar två argument varav den ena är ett predikat och den andra är en lista.
;Proceduren filter filtrera ut de elementen i listan för vilka predikatet ger värdet #t.


(define filter
  (lambda (lst pred)
    (cond ((null? lst) '())
          ((pred (car lst)) (cons (car lst) (filter (cdr lst) pred)))
          (else (filter (cdr lst) pred)))))

;Uppgift 22)
;reverse vänder på en lista men inte på de listor som är element i listan (om det finns).
;Sålunda kommer resultatet av (reverse '((1 2) (3 4))) att bli ((3 4) (1 2)).
;Skriv proceduren deep-reverse som vänder på alla listor på alla nivåer, så att (deep-reverse '((1 2) (3 4))) blir ((4 3) (2 1))


(define reverse-inner
  (lambda (lst)
    (cond ((null? lst) '())
          ((list? (car lst))
           (cons
            (reverse (reverse-inner (car lst)))
            (reverse-inner (cdr lst))))
          (else
           (cons
            (car lst)
            (reverse-inner (cdr lst)))))))
;Tittar på varje element enskilt och ser om något element i den yttersta listan är en lista.
;Skapar en ny lista med elementen i samma ordning, men alla element som är listor kommer att vara i omvänd ordning, på alla nivåer.

(define deep-reverse
  (lambda (lst)
    (reverse (reverse-inner lst))))
;Vänder på alla element i yttersta listan efter att reverse-inner vänt på elementen i alla inre listor.



