#lang racket

(require (lib "trace.ss"))

;Egen uppgift

;Svansrekursiv fibonacci:

(define fib
  (lambda (a)
    (fib-tail a 0 1)))

(define fib-tail
  (lambda (a b c)
    (cond ((= a 0) 0)
          ((= a 1) + b c)
          (else (fib-tail (- a 1) c (+ b c))))))

(displayln "fibonacci 10")
(fib 10)

;definiera SGD(a,b)

(define rest
  (lambda (a b)
    (remainder a b)))

  
(define SGD
  (lambda (a b)
    (cond ((integer? (/ a b)) b)
          (else (SGD b (rest a b))))))

(newline)
(displayln "SGD 90/12")
(SGD 90 12)

(newline)

;listor

(define numberlist '(1 2 3 4 5))

;listlängd

(define list-length
  (lambda (a-list)
    (list-length-tail a-list 0)))

(define list-length-tail
  (lambda (a-list n)
    (if (null? a-list)
        n
        (list-length-tail (cdr a-list) (+ n 1)))))

(displayln "listlängd (1 2 3 4 5)")

(list-length numberlist)

(newline)

;multiplikation av varje tal i en lista med dess respektive position i listan:

(define rising-mult-list
  (lambda (a-list)
    (rising-mult-list-tail a-list '() 1)))

(define rising-mult-list-tail
  (lambda (a-list new-list mult)
    (if (null? a-list)
        new-list
        (rising-mult-list-tail (cdr a-list) (append  new-list (list (* mult (car a-list)))) (+ mult 1)))))

(displayln "listelement * dess placering (1 2 3 4 5)")

(rising-mult-list numberlist)

;bakvänd lista:

(define reverse-list
  (lambda (list)
    (reverse-list-tail list '())))

(define reverse-list-tail
  (lambda (list ans)
    (if (null? list)
        ans
        (reverse-list-tail (cdr list) (cons (car list) ans)))))

(newline)

(displayln "bakvänd lista (1 2 3 4 5)")

(reverse-list numberlist)

(newline)

;kolla om ett tal finns i en lista:

(define check-for-n-in-list
 (lambda (n list)
   (cond ((null? list) #f)
         ((= (car list) n) #t)
         (else (check-for-n-in-list n (cdr list))))))

;kolla om ett tal finns i en lista, och dess position i listan:

(define check-position
  (lambda (n list)
    (check-position-tail n list 1)))

(define check-position-tail
  (lambda (n list ans)
    (cond ((null? list) (display "Det sökta elementet finns ej i listan"))
          ((= (car list) n) ans)
          (else (check-position-tail n (cdr list) (+ ans 1))))))

(displayln "position av ett element i en lista: 6, (1 3 5 7 9 2 4 6 8 0)")

(check-position 6 '(1 3 5 7 9 2 4 6 8 0))

(newline)

;addition av tal i en lista:

(define addition-list
  (lambda (list)
    (if (null? list)
        0
        (+ (car list) (addition-list (cdr list))))))

(displayln "addition av tal i lista (1 2 3 4 5)")

(addition-list numberlist)

(newline)

;skriv ut den längsta listan av 2.

(define write-longest
  (lambda (a-list b-list)
    (write-longest-help a-list b-list a-list b-list)))

(define write-longest-help
  (lambda (a-list b-list c-list d-list)
    (cond ((null? a-list) d-list)
          ((null? b-list) c-list)
          (else (write-longest-help (cdr a-list) (cdr b-list) c-list d-list)))))

;hitta n-te elementet i en lista:

(define find-n
  (lambda (n a-list)
    (cond ((= n 1)(car a-list))
          ((null? a-list) (display "list shorter than n numbers"))
          (else (find-n (- n 1) (cdr a-list))))))

;plocka ut alla av ett visst element ur en lista, returnera listan utan de elementen:

(define pick-out
  (lambda (n a-list)
    (pick-out-help n a-list '())))

(define pick-out-help
  (lambda (n a-list b-list)
    (cond ((null? a-list) b-list)
          ((= n (car a-list)) (pick-out-help n (cdr a-list) b-list))
          (else (pick-out-help n (cdr a-list) (append b-list (list (car a-list))))))))

;ta in en lista, skriv ut lista innehållandes samma element i storleksordning:

(define find-biggest
  (lambda (a-list)
    (find-biggest-help a-list (car a-list))))

(define find-biggest-help
  (lambda (a-list ans)
    (cond ((null? a-list) ans)
          ((> (car a-list) ans) (find-biggest-help (cdr a-list) (car a-list)))
          (else (find-biggest-help (cdr a-list) ans)))))

(define find-smallest
  (lambda (a-list)
    (find-smallest-help a-list (car a-list))))

(define find-smallest-help
  (lambda (a-list ans)
    (cond ((null? a-list) ans)
          ((< (car a-list) ans) (find-smallest-help (cdr a-list) (car a-list)))
          (else (find-smallest-help (cdr a-list) ans)))))

(define remove-smallest
  (lambda (a-list)
    (remove-smallest-help a-list (car a-list) '() a-list 0)))

(define remove-smallest-help
  (lambda (a-list smallest b-list c-list check)
    (cond ((null? a-list) b-list)
          ((= check 1)(remove-smallest-help (cdr a-list) (find-smallest a-list) (append b-list (list (car a-list))) c-list check))
          ((= (find-smallest c-list) (car a-list)) (remove-smallest-help (cdr a-list) smallest b-list c-list(+ check 1)))
          (else (remove-smallest-help (cdr a-list) (find-smallest a-list) (append b-list (list (car a-list))) c-list check)))))

(define sort
  (lambda (a-list)
    (sort-help a-list '())))

(define sort-help
  (lambda (a-list b-list)
    (cond ((null? a-list) b-list)
          (else (sort-help (remove-smallest a-list) (append b-list (list (find-smallest a-list))))))))
                                                                                                                     

;hitta minsta värdet i a-listan
;flytta det till första platsen i b-listan
;ta bort talet från a listan
;kör tills a-listan är tom.


