#lang racket

;Föreläsning 3

(require (lib "trace.ss")) ;gör det möjligt att använda trace funktion

(cond ((< 1 3) '(0 2)))

;n-fakultet (svansrekursivt):

(define fakultet
  (lambda (n)
    (define loop
      (lambda (n var)
        (if (= n 0)
            var
            (loop (- n 1) (* var n)))))
     (loop n 1)))

;alternativt (icke-svansrekursivt):

(define factorial
  (lambda (n)
    (cond ((= n 0) 1)
          ((> n 0) (* n (factorial (- n 1)))))))
(trace factorial)



(define power-close-to
  (lambda (bas tal n)
    (if (>= (expt bas n) tal)
        n
        (power-close-to bas tal (+ n 1)))))  ;räknar upp värdet på n tills dess att bas^n > tal

(define en_funktion
  (lambda (ett_argument)
    (if (= ett_argument 0)
        (+ ett_argument 1)
        (en_funktion (- ett_argument 1)))))

;exponent b^n

(define expt-1
   (lambda (b n)
      (if (= n 0)
         1
         (* b (expt-1 b (- n 1))))))

(trace expt-1)

;svansrekursivt

(define expt-2
   (lambda (b n)
      (expt-2-svans b n 1)))

(define expt-2-svans
   (lambda (b n res)
      (if (= n 0)
         res
         (expt-2-svans b (- n 1) (* b res)))))
(trace expt-2-svans)

;factorial svansrekursivt

(define fac-2
   (lambda (n)
      (fac-2-svans n 2 1)))

(define fac-2-svans
   (lambda (n m res)
      (if (> m n)
         res
         (fac-2-svans n (+ m 1) (* res m)))))
(trace fac-2-svans)

;rekursivt

(define add-1
   (lambda (a b)
      (if (= a 0)
         b
         (+ 1 (add-1 (- a 1) b)))))

;svansrekursivt
(define add-2
   (lambda (a b)
      (if (= a 0)
         b
         (add-2 (- a 1) (+ b 1)))))

;Fibonacci

;rekursiv, långsam för höga nummer
(define fib
  (lambda (n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))

;svansrekursiv
  
    