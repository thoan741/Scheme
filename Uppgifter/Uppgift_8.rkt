#lang racket

(displayln "Uppgift 8")

;Definiera en procedur som beräknar volymen av en elliptisk cylinder med radierna
;a och b och höjd h som argument.

(define area
  (lambda (a b)
    (* a b pi)))

(define volym
  (lambda (a b h)
    (* h (area a b))))
