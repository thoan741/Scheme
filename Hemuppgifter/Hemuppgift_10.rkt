;hemuppgift 10

(require racket/vector)

;Uppgift 40

;a)

;skriv en procedur som tar en vektor med heltal och returnerar det minsta talet i vektorn.

(define min-vector
  (lambda (vec)
    (define inner
      (lambda (vec comp)
        (if (= comp (vector-length vec))
            (vector-ref vec (- comp 1))
            (min (vector-ref vec (- comp 1)) (inner vec (+ comp 1))))))
    (inner vec 1)))

;alt

(define min-vektor
  (lambda (vec)
    (vector-ref (vector-sort vec <) 0)))


;b)

;Skriv en procedur som tar två vektorer med tal,
;tolkar dem som matematiska vektorer och returnerar skalärprodukten av vektorerna.
;(Du kan anta att de båda vektorerna har samma längd.)

(define scalar-product
  (lambda (vec1 vec2)
    (define inner
      (lambda (vec1 vec2 comp)
        (if (= comp (vector-length vec1))
            (* (vector-ref vec1 (- comp 1)) (vector-ref vec2 (- comp 1)))
            (+ (* (vector-ref vec1 (- comp 1)) (vector-ref vec2 (- comp 1)))
               (inner vec1 vec2 (+ comp 1))))))
    (inner vec1 vec2 1)))