
;hemuppgift 8

;uppgift 35

;skriv en procedur som tar ett argument och kollar om det är en lista. får ej använda list?

(define check-if-list?
  (lambda (lst)
    (cond ((null? lst) #t)
          ((pair? lst) (check-if-list? (cdr lst)))
          (else #f)
          )))

(check-if-list? '(1 2 3 4))  ;=> #t
(check-if-list? '())         ;=> #t
(check-if-list? (cons 1 2))  ;=> #f


;uppgift 36

;välj ut det näst största elementet i en lista

;jämföra 2 element.


;hitta största elementet.
;(define largest
;  (lambda (lst)
;    (cond ((null? lst) '())
;          ((null? (cdr lst)) (car lst))
;          (else (max (car lst) (largest (cdr lst))))
;      )))

;alternativt

(define largest
  (lambda (lst)
    (if (null? lst)
        '()
        (car (sort lst >)))))

;sort sorterar i det här fallet i storleksordning med största elementet först.
;Vi plockar därför ut det första värdet i den sorterade listan.

;ta bort alla förekomster av det största elementet

;(define remove-el
;  (lambda (el lst)
;    (cond ((null? lst) '())
;          ((= el (car lst))
;           (remove-el el (cdr lst)))
;          (else (cons (car lst)
;                      (remove-el el (cdr lst)))))))
;
;(define remove-largest
;  (lambda (lst)
;    (remove-el (largest lst) lst)))

;alternativt

(define remove-largest
  (lambda (lst)
    (remove* (list (largest lst)) lst)))
;där remove* tar bort alla förekomster av det största talet

;välj ut nästa största element i en lista:

(define second-largest
  (lambda (lst)
    (if (null? (remove-largest lst))
        #f
        (largest (remove-largest lst)))))


;utan hjälpfunktion

(define second-largest2
  (lambda (lst)
    (cond ((null? lst) #f)
          ((eq? (sort lst >) (sort lst <))
           #f)
          (else (car
                 (sort (remove*
                        (list (car (sort lst >)))
                        lst)
                       >))))))