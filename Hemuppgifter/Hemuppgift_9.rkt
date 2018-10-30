;hemuppgift 9

;(require plot)


;30

;Definiera en procedur cubic som tar tre argument och genererar en funktion för att beräkna värdet av ett tredjegradspolynom.

(define cubic
  (lambda (a b c)
    (lambda (x)
      (+ (expt x 3) (* a (expt x 2)) (* b x) c))))

(define f (cubic -1 1 -1))

;(define test (cubic 1 1 1))
;(test 2)

;intervallhalvering

(define intervallhalvering
  (lambda (f x0 x1 eps)
    (define close-enough?
      (lambda (a b)
        (< (abs (- a b)) eps)))
    (let ((m (/ (+ x0 x1) 2)))
      (if (close-enough? x1 x0)
          m
          (if (negative? (* (f x0) (f m)))
              (intervallhalvering f x0 m eps)
              (intervallhalvering f m x1 eps))))))

;newton-raphson

(define newton-raphson
  (lambda (f guess)
    (if (good-enough? f guess)
        guess
        (newton-raphson f (improve guess f)))))

(define good-enough?
  (lambda (func guess)
    (< (abs (func guess)) 0.000001)))

(define improve
  (lambda (guess f)
    (- guess (/ (f guess) ((derivata f 0.001) guess)))))

(define derivata
  (lambda (f dx)
    (lambda (x)
      (/ (- (f (+ x dx)) (f (- x dx))) (* dx 2)))))

(intervallhalvering f -100.0 100.0 0.000000001)   ;0,999999....
(newton-raphson f 5)                              ;1,00000...

;31

;Om f är en funktion och n är ett positivt heltal kan man generera den n-te repeterade applikationen av f som definieras som den funktion vars värde i punkten x.

(define repeated
  (lambda (f n)
    (lambda (x)
      (if (= n 0)
          x
          (f ((repeated f (sub1 n)) x))
          ))))

(define g (repeated add1 3))

(g 5) ;8


;32

(define smooth
  (lambda (f dx)
    (lambda (x)
      (/ (+ (f (- x dx))
            (f x)
            (f (+ x dx)))
         3))))

(define (smooth-constant dx)
   (lambda (f)
     (smooth f dx)))

(define h ((repeated (smooth-constant 0.001) 8) f))

(h 3) ;20,00...

;(plot-new-window? #t)
;(plot (function h 2 4))