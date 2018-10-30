#lang racket

;Hemuppgift 4

(require (lib "trace.ss"))

;fördefinierade funktioner
(define succ
  (lambda (x)
    (+ x 1)))

(define pred
  (lambda (x)
    (- x 1)))

(define cut-off
  (lambda (a b)
    (cond ((= a 0) 0)
          ((= b 0) a)
          (else (cut-off (pred a) (pred b)))))) ;behövs för 13e)

(define plus
  (lambda (a b)
    (if (= a 0)
        b
        (plus (pred a) (succ b))))) ;behövs för 13a)

;12b, 13a,13b,13e,13f

;12b) f(n)={ jämnt om n=0, udda om n=1,f(n−2) annars. implementera odd? mha pred:

(define odd?
  (lambda (n)
    (cond ((= n 0) #f)                      ;är n=0? returnera falskt.
          ((= n 1) #t)                      ;är n=1? returnera sant.
          (else (odd? (pred (pred n)))))))  ;annars subtrahera 2 från n och kör tills ett av fallen ovan uppfylls.

;alternativt

(define odd
  (lambda (n)
    (if (= n 0)
        #f
        (not (odd (pred n))))))

;13a) skriv multiplikation

(define mult
  (lambda (x y)
    (define loop
      (lambda (x y z)
        (cond ((= z 0) 0)
              ((= z 1) x)
              (else (loop (plus x y) y (pred z))))))
    (loop x x y)))

;alternativt

(define mul
  (lambda (x y)
    (cond ((= y 0) 0)                           ;är y = 0? returnera svaret
          (else (plus x (mul x (pred y)))))))   ;annars (+ x (mul x (- y 1)))

;13b) skriv fakultetsberäkning:

(define fac
 (lambda (n)
   (define loop
     (lambda (n ans)
       (if (<= n 0)                          ;är n=0? skriv ut svaret
           ans
           (loop (pred n) (mult ans n)))))   ;annars kör loop igen med (loop (- n 1) (* ans n))
   (loop n 1)))

;alternativt

(define Fac
  (lambda (n)
    (if (= n 0)
        1
        (mul n (Fac (pred n))))))

;13e) uttryck hitta största värdet a eller b
(define max
  (lambda (a b)
    (if (> (cut-off a b) 0)          ;är cutoff mellan a & b större än 0? isåfall är a större, skriv ut värdet för a.
        a
        b)))                         ;annars, om 0 så betyder det att b är större eller lika med a, skriv då ut b.

;13f) uttryck upphöjt till

(define power
  (lambda (a b)
    (define loop
      (lambda (a b ans)
        (if (= b 0)                          ;är b=0? skriv ut svar
            ans
            (loop a (pred b) (mult a ans))))) ;annars, kör loop igen med värden (loop a (- b 1) (* a ans))
    (loop a b 1)))

;alternativt

(define Power
  (lambda (a b)
    (cond ((= b 0) 1)
          (else (mul a (Power a (pred b)))))))

(trace Power)
(trace plus)
(trace mul)
(trace succ)
(trace pred)
(trace fac)
