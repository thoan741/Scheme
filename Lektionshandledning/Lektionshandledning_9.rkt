;lektionshandledning 9

;(define (funktionsnamn parameter)
;  (gör-något parameter))

;samma som

;(define funktionsnamn
;  (lambda (parameter)
;    (gör-något parameter)))

;lambda = anonym procedur

(define make-adder
  (lambda (n)
    (lambda (x) (+ x n))))

(define add2 (make-adder 2))

(add2 4) ;=> 6

((make-adder 4) 3) ;=> 7

(define double-invoke
  (lambda (f)
    (lambda (x)
      (f(f x)))))

(define add-2 (double-invoke add1))

(add-2 5) ;=> 7

((double-invoke add1) 1)  ;=> 3

(define composition
  (lambda (f g)
    (lambda (x)
      (f(g x)))))

((composition add1 sub1) 1) ;=> 1

;kan även skrivas

(define (composition2 f g)
  (lambda (x)
    (f (g x))))

;ett annat sätt att göra uppgift 36

(define next-biggest
  (lambda (x)
    (define next-biggest-inner
      (lambda (lst max näst-max)
        (if (null? lst)
             näst-max
             (if (> (car lst) max)
                 (next-biggest-inner (cdr lst) (car lst) max)
                 (if (> (car lst) näst-max)
                     (next-biggest-inner (cdr lst) max (car lst))
                     (next-biggest-inner (cdr lst) max näst-max))))))
    (next-biggest-inner x (car x) (cadr x))))

;(define blabla (a b . funktioner)) tar emot godtyckligt många funktioner