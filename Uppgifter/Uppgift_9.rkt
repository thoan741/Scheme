#lang racket

(displayln "uppgift 9")

;beskriv hur följande procedur fungerar

(define a-plus-abs-b            ;Här definieras funktionen
  (lambda (a b)                 ;Funktionen ges 2 invärden
    ((if (> b 0) + -) a b)))    ;Här kollas om b är positiv. Om b positiv, returneras a+b, annars returneras a-b. Alltså returneras a + |b|.