#lang racket

;Lektionshandledning 5

#|
cons:
Skapar par av element
(cons 1 5) -> '(1 . 5)
(cons 'hej 'då) -> '(hej.då)              skapar ett par.
(cons (cons 1 2) 3) -> '((1 . 2) . 3)
car: returnerar första elementet i ett par eller en lista.
cdr: returnerar allt utom första elementet.

Listor:
Sekvenser som avslutas med null/'()
(cons (cons 1 2) '())
(list? x) kollar om x är en lista, returnerar #t eller #f
(list 1 2)
(define ls (list 1 2 3))
(car ls) -> 1
(cdr ls) -> '(2 3)
(car (cdr ls)) -> 2
(cadr ls) -> 2
(list-ref ls 0) -> 1 (index 0 ger första element, index 1 andra osv...)
(length ls) -> 3
Append skapar en ny lista av alla element
|#

(define myLength
  (lambda (ls)
    (if (null? ls)
        0
        (+ 1 (myLength (cdr ls))))))

(define myLength_help
  (lambda (ls acc)
    (if (null? ls)
        acc
        (myLength_help (cdr ls) (+ acc 1)))))

(define myLength_
  (lambda (ls)
    (myLength_help ls 0)))

(define mySum-inner
  (lambda (ls acc)
    (if (null? ls)
        acc
        (mySum-inner (cdr ls) (+ (car ls) acc)))))

(define mySum
  (lambda (ls)
    (mySum-inner ls 0)))

#|
;Datatyper
integer      - heltal
real         - decimaltal
rational     - bråk/rationella tal
boolean      - #t, #f
symbol       - 'hej (text)
string       -
list         - lista
pair         - par
procedure    - procedur

Varje datatyp har ett motsvarande predikat för att kontrollera tillhörighet.
(procedure? myLength) -> #t
(bool? #f) -> #t
|#

;uppgift 17)
;definiera en lista som bara returnerar en lista med sista elementet i en lista

(define last-pair
  (lambda (ls)
    (if (null? (cdr ls))
        ls
        (last-pair (cdr ls)))))
;svansrekursiv utan någon accumulator, pga att vi bara är ute efter sista elementet, de tidigare elementen kan bara kastas bort
