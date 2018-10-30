(require racket/vector)

;Lektionshandledning 11


;scheme-lista = länkad lista

(define a (vector 1 2 3 4 5))

(vector-ref a 2) ;=> index 2 i vektorn a.

(make-vector 2 5) ;=> skapa en vektor med 2 index, båda med värdet 5

(vector 1 2 3) ;=> skapar vektor med 1 på index 0, 2 på index 1 osv...

(define v #(3 9 12)) ;=> skapar vektor #(3 9 12)

v

;Indexering

(vector-ref v 0) ; första elementet i vektor v => 3

(vector-set! a 1 15) ;index 1 i vektor a byts till siffran 15 => a = #(1 15 3 4 5)

a

(vector-length v) ;=> 3

;Append

(vector-append v #(19 27))


;Uppgift 42

(define ködagar (list (cons "Kalle" 125)
                      (cons "Eva" 1072)
                      (cons "Per" 1546)
                      (cons "Lisa" 779)))

(define Assoc
  (lambda (key assoc-list)
    (if (eq? (car (car assoc-list)) key)
        (cdr (car assoc-list))
        (Assoc key (cdr assoc-list)))))

(Assoc "Per" ködagar)  ;=> 1546

