#lang racket

;lektionshandledning 6

;24

;26b,c

;24:

#|
Originalkod:
(make-rat
   (lambda (numer denom)
      (list numer denom)))
|#

(define (make-rat numer denom)
  (let* ((sign (if (< (* numer denom) 0) -1 1))
         (numer_abs (abs numer))
         (denom_abs (abs denom))
         (rat_gcd (gcd numer_abs denom_abs))
         (numer_norm (* sign (/ numer_abs rat_gcd)))
         (denom_norm (/ denom_abs rat_gcd))
         )
    (list numer_norm denom_norm)))

(let* ((a 1)
      (b (+ a 1)))
  (+ a b))
#|
(let ((a 1)
      (b (+ a 1)))
  (+ a b))
|#

;26 hjälpfunktioner:

;;; Addera två komplexa tal
(define cplx-add
  (lambda (c d)
    (make-cplx (+ (re c) (re d)) (+ (im c) (im d)))))

;;; Skapa komplext tal utifrån polära representationen
(define make-cplx-polar
  (lambda (r theta)
    (make-cplx (* r (cos theta)) (* r (sin theta)))))

;;; Argumentet av ett komplext tal
;;; arctan(b/a) i 1:a & 4:e kvadranten; andra kvadranter blir lite svårare
(define arg
  (lambda (c)
    (let ((a (re c)) (b (im c)))
      (let ((at (atan (/ b a))))
        (cond ((> a 0) at)
              ((and (>= b 0) (< a 0)) (+ at pi))
              ((and (< b 0) (< a 0)) (- at pi)))))))

;;; Beloppet av ett komplext tal
(define abs-cplx
  (lambda (c)
    (let ((a (re c)) (b (im c)))
      (sqrt (+ (* a a) (* b b))))))

;;; Multiplicera två komplexa tal
(define cplx-mul
  (lambda (c d)
    (make-cplx-polar (* (abs-cplx c) (abs-cplx d)) (+ (arg c) (arg d)))))

;26 b)
(define cplx-sub
  (lambda (c d)
    (make-cplx (- (re c) (re d)) (- (im c) (im d)))))

;26b)
(define (cplx-div x y)
  (let* ((a (re x))
         (b (im x))
         (c (re y))
         (d (im y))
         (n (+ (expt c 2) (expt d 2))))
    (make-cplx (/ (+ (* a c) (* b d)))
               (/ (- (* b c) (* a d))))
    ))

;26d)

(define (cplx-mul-2 x y)
  (let* ((a (re x))
         (b (im x))
         (c (re y))
         (d (im y))
         (n (+ (expt c 2) (expt d 2))))
    (make-cplx (- (* a c) (* b d))
               (+ (* b c) (* a d)))
    ))

(define (gcd a b)
  (define rest (modulo a b))
  (if (= rest 0)
      b
      (gcd b rest)))