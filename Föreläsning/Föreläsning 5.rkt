#lang racket

;Föreläsning 5

(define adress "testvägen 2")

adress

(define namelist '(kalle pelle lasse))


'() ;tom lista

(define numberlist '(1 2 3 4 5))


(list 'kalle 'pelle 'lasse)

(cons 'kalle '(pelle lasse))

(cons 'kalle (cons 'pelle (cons 'lasse '())))

(cdr namelist) ;alla element i en lista utan det första

(car namelist) ;första elementet i en lista

(cadr namelist) ;andra elementet i en lista 

(define nth-element
  (lambda (n a-list)
    (if (= n 1)
        (car a-list)
        (nth-element (- n 1) (cdr a-list)))))

(define enlista (cons 1 (cons 3 (cons 4 '()))))

(define ettpar (cons 3 5))

(car enlista)

(car ettpar)

(cdr enlista)

(cdr ettpar)

(cdr ettpar)

(cdr enlista)

(car (cdr enlista))

(cdr (cdr (cdr enlista)))

;((a b) . (c d)) = ((a b) c d)

;(0 1 2 . 3) sista elementet är inte en tom lista, därför ingen lista.

(define nth-element2
  (lambda (n a-list)
    (cond ((not (list? a-list)) '())
          ((null? a-list) '())
          ((<= n 0) '())
          ((= n 1) (car a-list))
          (else (nth-element2 (- n 1) (cdr a-list))))))

(define nth-element3
  (lambda (n a-list)
    (define inner
      (lambda (n a-list)
        (if (= n 1)
            (car a-list)
            (inner (- n 1) (cdr a-list)))))
    (cond ((not (list? a-list)) '())
          ((null? a-list) '())
          ((<= n 0) '())
          (else (inner n a-list)))))

(define finns?
  (lambda (n a-list)
    (cond
      ((null? a-list) #f)
      ((= (car a-list) n) #t)
      (else (finns? n (cdr a-list))))))
  

(define varfinns?
  (lambda (n a-list)
    (define inreprocedur
      (lambda (n a-list platsen)
        (cond
          ((null? a-list) #f)
          ((= (car a-list) n) platsen)
          (else (inreprocedur n (cdr a-list) (+ platsen 1))))))
    (inreprocedur n a-list 1)))


;i en lista är sista referensen '() (en tom lista)

;(cadr lista) = (car (cdr lista))
;(cddr lista) = (cdr (cdr lista))
;(cdddr lista) = (cdr (cdr (cdr lista)))
;(caddr lista) = (car (cdr (cdr lista)))
;osv...

(define length
  (lambda (a-list)
    (if (null? a-list)
        0
        (+ 1 (length (cdr a-list))))))

(define append-element
  (lambda (x a-list)
    (if (null? a-list)
        (cons x '())
        (cons (car a-list)
              (append-element x (cdr a-list))))))

(define add-element
  (lambda (n a-list)
    (append a-list (cons n '()))))

(define append
  (lambda (x y)
    (if (null? x)
        y
        (cons (car x) (append (cdr x) y)))))

(define add-items
  (lambda (a-list)
    (if (null? a-list)
        0
        (+ (car a-list) (add-items (cdr a-list))))))

(define repeat
  (lambda (n element)
    (if (= n 0)
        '()
        (cons element (repeat (- n 1) element)))))


(define double-all
  (lambda (a-list)
    (if (null? a-list)
        '()
        (cons (* 2 (car a-list))
              (double-all (cdr a-list))))))

(define delete
  (lambda (element a-list)
    (cond ((null? a-list) '())
          ((= element (car a-list))
           (delete element (cdr a-list)))
          (else (cons (car a-list)
                      (delete element (cdr a-list)))))))

(define filter
  (lambda (p? a-list)
    (cond ((null? a-list) '())
          ((p? (car a-list))
           (cons (car a-list) (filter p? (cdr a-list))))
          (else (filter p? (cdr a-list))))))

