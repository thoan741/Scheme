

;Föreläsning 8

;Intervallhalveringsmetoden:

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

(* 1.0 (intervallhalvering
        (lambda (x) (- (* x x) 2))
        -5 5 0.000000000000001))

;newton-raphsons metod
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

;fibonacci rekursiv:
(define fib
  (lambda (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2)))))))

;svansrekursiv fibonacci:
(define fib-2
  (lambda (n)
    (define fib-2-iter
      (lambda (fibm fibm-1 m)
        (if (<= n m)
            fibm
            (fib-2-iter (+ fibm fibm-1) fibm (+ m 1)))))
    (fib-2-iter 1 0 1)))

(fib-2 10)

(define expt-1
  (lambda (b n)
    (if (= n 0)
        1
        (* b (expt-1 b (- n 1))))))

;en annan metod:

(define expt-2
  (lambda (b n)
    (cond (( = n 0) 1)
          ((odd? n)
           (* b (sqr (expt-2 b (/ (- n 1) 2)))))
          (else (sqr (expt-2 b (/ n 2)))))))
#|
;Mängdtillhörighet (∈):
(define element-of-set?
  (lambda (element set)
    (cond ((null? set) #f)
          ((= element (car set)) #t)
          (else (element-of-set? element (cdr set))))))

;Lägga till ett element (+):
(define adjoin-set
  (lambda (element set)
    (if (element-of-set? element set)
        set
        (cons element set))))

;Snittet mellan två mängder (∩):

(define intersect
  (lambda (set-1 set-2)
    (cond ((or (null? set-1) (null? set-2)) ’())
          ((element-of-set? (car set-1) set-2)
           (cons (car set-1)
                 (intersect (cdr set-1) set-2)))
          (else (intersect (cdr set-1) set-2)))))
|#
;Vi byter representation till ordnade listor.
;Mängdtillhörighet (∈):
(define element-of-set?
  (lambda (element set)
    (cond ((null? set) #f)
          ((< element (car set)) #f)
          ((= element (car set)) #t)
          (else (element-of-set? element (cdr set))))))

;Lägga till ett element (+):
(define adjoin-set
  (lambda (element set)
    (cond ((null? set) (list element))
          ((= element (car set)) set)
          ((< element (car set)) (cons element set))
          (else
           (cons (car set)
                 (adjoin-set element (cdr set)))))))

;Snittet mellan två mängder (∩):
(define intersect
  (lambda (set-1 set-2)
    (if (or (null? set-1) (null? set-2))
        '()
        (let ((x1 (car set-1)) (x2 (car set-2)))
          (cond ((= x1 x2)
                 (cons x1 (intersect (cdr set-1)
                                     (cdr set-2))))
                ((< x1 x2)
                 (intersect (cdr set-1) set-2))
                (else (intersect set-1 (cdr set-2))))))))
