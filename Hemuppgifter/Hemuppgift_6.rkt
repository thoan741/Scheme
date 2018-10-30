#lang racket


;25a)

;konstruktor som skapar ett par.
(define point
  (lambda (x y)
    (cons x y)))

;första elementet i paret som konstuktorn skapade blir x-koordinat, vilket denna selektor hämtar ut.
(define x-koord
  (lambda (ls)
    (car ls)))

;andra elementet i paret blir y-koordinat, vilket denna selektor hämtar ut.
(define y-koord
  (lambda (ls)
    (cdr ls)))
;25b)

;konstruktor för en linje definieras som ett par av två punkter.
(define line
  (lambda (x0 y0 x1 y1)
    (cons (point x0 y0) (point x1 y1))))

;första punkten hämtas ut med denna selektor och blir startpunkt
(define start_point
  (lambda (line)
    (car line)))

;andra punkten hämtas ut med denna och blir slutpunkt
(define end_point
  (lambda (line)
    (cdr line)))

;25c)

;en selektor som beräknar medelvärdet av två punkter.
(define average
  (lambda (line selector)
    (/ (+ (selector (start_point line)) (selector (end_point line))) 2)))

;konstruktor för mittpunkten i en linje, där en ny punkt skapas som medelvärdet av x-värdena och y-värdena i en linje
(define middle
  (lambda (line)
    (point (average line x-koord)
           (average line y-koord))))

;;; Addera två komplexa tal
(define cplx-add
  (lambda (c d)
    (make-cplx (+ (re c) (re d)) (+ (im c) (im d)))))

;;; Skapa komplext tal utifrån polära representationen
(define make-cplx-polar
  (lambda (r theta)
    (make-cplx (* r (cos theta)) (* r (sin theta)))))

;26 d) (omskrivning)
;;; Argumentet av ett komplext tal
;;; arctan(b/a) i 1:a & 4:e kvadranten; andra kvadranter blir lite svårare
(define arg
  (lambda (c)
    (let ((a (re c)) (b (im c)))
      (cond
        ((and (= a 0) (= b 0)) (error 'undefined))        ;om re och im båda 0, odefinierat argument.
        ((and (= a 0) (> b 0)) (/ pi 2))                  ;om re är 0 och im större än 0, argumentet blir pi/2.
        ((and (= a 0) (< b 0)) (/ pi -2))                 ;om re är 0 och im mindre än 0, argumentet blir -pi/2.
        ((let ((at (atan (/ b a))))                       ;här är problemet om vi inte skriver om, b/a är odefinierad om a är 0.
          (cond
            ((> a 0) at)
            ((>= b 0) (+ at pi))
            (else (- at pi)))))))))

;;; Beloppet av ett komplext tal
(define abs-cplx
  (lambda (c)
    (let ((a (re c)) (b (im c)))
      (sqrt (+ (* a a) (* b b))))))

;;; Multiplicera två komplexa tal
(define cplx-mul
  (lambda (c d)
    (make-cplx-polar (* (abs-cplx c) (abs-cplx d)) (+ (arg c) (arg d)))))

;26 a)

;konstruktor som skapar ett par, där första elementet är realdel och andra imaginärdel.
(define make-cplx
  (lambda (re im)
    (cons re im)))

;selektor som hämtar ut realdelen ur ett komplext tal.

(define re
  (lambda (cplx)
    (car cplx)))

;selektor som hämtar ut imaginärdelen från komplext tal.

(define im
  (lambda (cplx)
    (cdr cplx)))

