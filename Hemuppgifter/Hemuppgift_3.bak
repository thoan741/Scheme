#lang racket

;Uppgift 7)

;Definiera en procedur som beräknar ytan av en ellips med radierna a och b:

(define area
  (lambda (a b)
    (* pi a b)))

;Visa beräkningsstegen för uttrycket:

(area 3 4)

;(area a b)
;(area 3 4)
;(* pi 3 4)
;37.699...

;Uppgift 8)

;Definiera en procedur som beräknar volymen av en elliptisk cylinder med radierna a och b samt höjden h som argument.

;där area(a, b) är proceduren definierad i föregående uppgift. Din volume-procedur ska alltså använda sig av proceduren i föregående uppgift.

(define volume
  (lambda (a b h)
    (* h (area a b))))

;Visa beräkningsstegen för uttrycket

(volume 3 4 5)
;(volume a b h)
;(volume 3 4 5)
;(* 5 (area 3 4))
;(* 5 (* 3 4 pi))
;(* 5 37.699...)
;188.495...

;Uppgift 11)

;Implementera den rekursiva funktionen nedan i Scheme. 

;f(n)=0 om n=0, 2 om n=1, 37 om n=2, f(n−3)+1 om n är udda, f(n/2−1)+7 annars:

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

; a)  Visa med hjälp av substitutioner hur beräkningen av din implementation sker då anropet är:

(f 17)
;(f n)
;(f-svans n 0)
;(f-svans 17 0)
;(f-svans 14 1)
;(f-svans 6 8)
;(f-svans 2 15)
;(+ 15 37)
;52

; b) Är din lösning svansrekursiv eller ej? Motivera!

;Ja, min lösning är svansrekursiv.
;Värdet som senare skrivs ut sparas på en egen plats i funktionen och uppdateras vid varje loop, och är därmed svansrekursiv.