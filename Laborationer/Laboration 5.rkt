

(require (lib "trace.ss"))

;Laboration 5

;Jag väljer att representera mängder som listor.
;Jag är van att arbeta med listor så jag valde det sättet.

;uppgift 1

;Hjälpfunktioner

;first-of är en selektor som givet en lista returnerar det första elementet i en lista.

(define first-of
  (lambda (lst)
    (car lst)))

;rest-of är en selektor som givet en lista returnerar listan med första elementet bortkapat.

(define rest-of
  (lambda (lst)
    (cdr lst)))

;empty-set är ett predikat som undersöker om en lista är tom.

(define empty-set?
  (lambda (lst)
    (null? lst)))

;make-empty är en procedur utan något argument som skapar en tom lista.

(define make-empty
  (lambda ()
    '()))

;insert-el är en selektor som givet ett element och en lista, lägger till elementet på första platsen i listan.

(define insert-el
  (lambda (el lst)
    (cons el lst)))

;remove-el är en selektor som givet ett element och en lista, tar bort alla förekomster av elementet i listan.
;denna används inte längre någonstans i programmet.

;(define remove-el
;  (lambda (el lst)
;    (cond ((null? lst) '())
;          ((same-elem? el (car lst)) (remove-el el (cdr lst)))
;          (else (cons (car lst) (remove-el el (cdr lst)))))))
;

;remove-duplicate är en selektor som givet en lista, tar bort alla dubletter i listan.
;denna används inte längre någonstans i programmet.

;(define remove-duplicate
;  (lambda (lst)
;    (cond ((null? lst)
;           '())
;          ((member? (car lst) (cdr lst))
;           (remove-duplicate (cdr lst)))
;          (else (cons (car lst) (remove-duplicate (cdr lst)))))))

;rem är en selektor, som givet ett element och en lista, tar bort en förekomst av elementet i listan

(define rem
  (lambda (el lst)
    (remove el lst)))

;combine-sets är en selektor, som givet två listor, skapar en ny lista bestående av alla element i första och andra listan.

(define combine-sets
  (lambda (lst-1 lst-2)
    (append lst-1 lst-2)))

;set? är ett predikat som givet ett argument, kollar om argumentet är en lista.
(define set?
  (lambda (lst)
    (list? lst)))

;do-to-each är en selektor som givet en procedur och en lista, returnerar en ny lista där proceduren applicerats på varje element i listan.

(define do-to-each
  (lambda (proc lst)
    (map proc lst)))

;set-length är en selektor som givet en lista, returnerar hur många element som finns i listan.
;denna används inte någonstans i koden längre.

;(define set-length
;  (lambda (set)
;    (length set)))

;make-set (dum)

;den första varianten av make-set är en konstruktor som givet en lista, returnerar listan.

(define make-set
  (lambda (lst)
    lst))

;make-set (smart)

;den smartare versionen av make-set är en konstruktor som givet en lista, returnerar en lista med alla dubletter borttagna, det är på denna form alla mängder är definierade.
;denna finns identisk lite längre ner i koden med ett par tester, och member? förklaras även den senare vad den gör.

(define make-set
  (lambda (lst)
    (cond ((null? lst) '())
          ((member? (car lst) (make-set (cdr lst)))
           (make-set (cdr lst)))
          ((list? (car lst)) (cons (make-set (car lst)) (make-set (cdr lst))))
          (else (cons (car lst) (make-set (cdr lst)))))))

;Härifrån till och med uppgift 8, får enbart egna selektorer, predikat och konstruktorer användas, med undantag att någon av procedurerna över behöver skrivas om.


;uppgift 2

;skriv en procedur member? som kollar om ett element finns i ett set:

;member? är ett predikat som givet ett element och en mängd, undersöker om det finns någon förekomst av elementet i setet.

(define member?
  (lambda (el set)
    (cond ((empty-set? set) #f)
          ((same-elem? el (first-of set)) #t)
          (else (member? el (rest-of set))))))

;skriv en procedur subset? som kollar om alla element som finns i en mängd finns i en andra mängd:

;subset är ett predikat som givet två mängder, undersöker om alla element som finns i första mängden, även finns i andra.

(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
          ((not (set? set1)) #f)
          ((member? (first-of set1) set2)
           (subset? (rest-of set1) set2))
          (else #f))))

;skriv en procedur same-set? som kollar om två mängder innehåller exakt samma element, oavsett ordning:

;same-set är ett predikat som givet två mängder, undersöker om båda mängderna är identiska.

(define same-set?
  (lambda (set1 set2)
    (if (and (subset? set1 set2) (subset? set2 set1))
        #t
        #f)))

;(define same-set?
;  (lambda (set1 set2)
;    (cond ((not (and (set? set1) (set? set2))) #f)
;          ((not (= (set-length set1) (set-length set2))) #f)
;          ((and (subset? set1 set2) (subset? set2 set1)) #t)
;          (else #f))))

;skriv en procedur same-elem? som kollar om två element är samma

;same-elem? är ett predikat som givet två argument (mängder eller element), undersöker om dessa två argument är identiska.

(define same-elem?
  (lambda (e1 e2)
    (cond ((and (symbol? e1) (symbol? e2))
           (eq? e1 e2))
          ((and (number? e1) (number? e2))
           (= e1 e2))
          ((and (set? e1) (set? e2))
           (same-set? e1 e2))
          (else #f))))

;uppgift 3
;definiera om make-set så att den tar bort dubbletter

;exakt samma make-set som den smartare versionen i uppgift 1:

(define make-set
  (lambda (lst)
    (cond ((null? lst) '())
          ((member? (car lst) (make-set (cdr lst)))
           (make-set (cdr lst)))
          ((list? (car lst)) (cons (make-set (car lst)) (make-set (cdr lst))))
          (else (cons (car lst) (make-set (cdr lst)))))))

;tester för uppgift 2 & 3

(displayln "member check:")

(member? 3 (make-set'(1 2 3 4)))                          ;#t
(member? 5 (make-set'(1 2 3 4)))                          ;#f
(member? 3 (make-set'(1 2(3 4))))                         ;#f
(member? (make-set '((x) 2)) (make-set '(4 2 (x))))       ;#f
(member? (make-set '(1 (b))) (make-set '(3 2 ((b) 1))))   ;#t

(newline)

(displayln "subset check:")

(subset? (make-set '(1 2 3 4)) (make-set'(1 2 3 4 5)))     ;#t
(subset? (make-set'(1 2 3 4 5)) (make-set'(1 2 3 4)))      ;#f
(subset? (make-set'(1 2 (4 3))) (make-set'(1 2 (3 4) 5)))  ;#t

(newline)

(displayln "same-set check:")

(same-set? (make-set'(2 3 4)) (make-set'(5 2 3 4 )))        ;#f
(same-set? (make-set'(1 2 (3 4))) (make-set'(1 (3 4) 2)))   ;#t
(same-set? (make-set '(1 2 a)) (make-set '(2 a 1)))         ;#t
(same-set? (make-empty) (make-empty))                       ;#t
(same-set? (make-set '((2 3) a b)) (make-set '(2 3 (a b)))) ;#f

(newline)
  
(displayln "same-elem check:")

(same-elem? 'a 'b)                                              ;#f
(same-elem? 'a 'a)                                              ;#t
(same-elem? 1 2)                                                ;#f
(same-elem? 1 1)                                                ;#t
(same-elem? (make-set '(1 2 (3 4))) (make-set '(1 2)))          ;#f
(same-elem? (make-set '(1 2 (3 4))) (make-set '((4 3) 1 2)))    ;#t

(newline)

(displayln "test med uppdaterad make-set")

(make-set '(a (a b b (c b) 3) 5 5.0 (e s) (s e s))) ;(a (a b (c b) 3) 5.0 (e s))

(newline)

;uppgift 4

;skapa en procedur union som skapar unionen av två mängder: (element som finns i någon av mängderna)

;union är en konstruktor som givet två mängder, skapar en union av mängderna. (Alltså en ny mängd innehållandes elementen från de båda mängden)

(define union
  (lambda (set1 set2)
    (make-set (combine-sets set1 set2))))

;skapa en procedur som beräknar snittet av två mängder: (element som finns i båda mängderna)

;intersection är en konstruktor som givet två mängder, skapar en ny mängd innehållandes snittet av mängderna. (en ny mängd innehållandes alla element som finns i båda mängderna)

(define intersection
  (lambda (set1 set2)
    (cond ((or (empty-set? set1) (empty-set? set2)) (make-empty))
          ((member? (first-of set1) set2)
           (insert-el (first-of set1)
                      (intersection (rest-of set1) set2)))
          (else (intersection (rest-of set1) set2))
          )))

;skapa en procedur som skapar differensen mellan två mängder

;difference är en konstruktor som givet två mängder, skapar en ny mängd innehållandes differensen mellan mängderna.
;(en ny mängd innehållandes ala element som finns i första men inte i andra mängden.

(define difference
  (lambda (set1 set2)
    (cond ((empty-set? set1) (make-empty))
          ((member? (first-of set1) set2)
           (difference (rest-of set1) set2))
          (else (insert-el (first-of set1)
                           (difference (rest-of set1) set2))))))


;test av union, difference, intersection

(displayln "union test")

(union (make-set '(1 2 (x (y 3) 4))) (make-set '(b 1 (a (r)))))  ;(2 (x (y 3) 4) b 1 (a (r)))
(union (make-set '(2 3)) (make-set '((a b))))                    ;(2 3 (a b))

(newline)

(displayln "difference test")

(difference (make-set '(1 (2 3) b)) (make-set '(b 1)))                       ;((2 3))

(newline)

(displayln "intersection test")

(intersection (make-set '(1 (2 (1)) 3 (1 2)))(make-set '(((1) 2) 3 4 (2 3)))) ;((2 (1)) 3)

(newline)

;uppgift 5

;Skriv en procedur (cartesian-product s1 s2)som returnerar den kartesiska produkten av två mängder.

;cartesian-product är en konstruktor som givet två mängder, returnerar den kartesiska produkten.
;(returneras som en lista av vektorer, där vektorerna består av alla kombinationer av elementsammansättningar mellan första och andra mängden)

(define cartesian-product
  (lambda (set1 set2)
    (define cartesian-product-inner
      (lambda (set1 set2 save)
        (cond ((empty-set? set2) (make-empty))
              ((empty-set? set1) (cartesian-product-inner save (rest-of set2) save))
              (else (insert-el (vector (first-of set1) (first-of set2)) (cartesian-product-inner (rest-of set1) set2 save))))))
    (cartesian-product-inner set1 set2 set1)))

;alternativt med hjälp av map

(define cartesian
  (lambda (set1 set2)
    (if (empty-set? set2)
        (make-empty)
        (combine-sets (do-to-each (lambda (x) (vector x (first-of set2))) set1)
                   (cartesian set1 (rest-of set2))))))
              

(displayln "cartesian product test")

(cartesian-product (make-set '(1 2 3)) (make-set '(a b)))  ;(#(1 a) #(2 a) #(3 a) #(1 b) #(2 b) #(3 b))

(newline)

#|

;uppgift 6

;skriv om funktioner så svansrekursiva

;procedurer från uppgift 1 som inte är svansrekursiva, som nu definieras om:

;denna används inte längre i koden, därför bortkommenterad.

;(define remove-el
;  (lambda (el lst)
;    (define remove-el-inner
;      (lambda (el lst new-lst)
;        (cond ((null? lst) new-lst)
;              ((same-elem? el (car lst)) (remove-el-inner el (cdr lst) new-lst))
;              (else (remove-el-inner el (cdr lst) (cons (car lst) new-lst)))
;              )))
;    (remove-el-inner el lst '())))

;denna används inte längre i koden, därför bortkommenterad.

;(define remove-duplicate
;  (lambda (lst)
;    (define remove-duplicate-inner
;      (lambda (lst new-lst)
;        (cond ((null? lst)
;               new-lst)
;              ((member? (car lst) (cdr lst))
;               (remove-duplicate-inner (cdr lst) new-lst))
;              (else (remove-duplicate-inner (cdr lst)  (cons (car lst) new-lst))))))
;    (remove-duplicate-inner lst '()))

;ingen från uppgift 2 behöver skrivas om.
;alla procedurer i uppgift två är predikat, och det blir inga beräkningar som ligger och väntar.

;make-set måste skrivas om:

;svansrekursiv variant av make-set från uppgift 3:

(define make-set
  (lambda (ls)
    (define make-set-inner
      (lambda (ls new-set help)
        (cond ((null? ls) new-set)
              ((not (null? help))
               (cond ((member? (car help) (make-set (cdr help)))
                      (make-set-inner ls new-set (cdr help)))
                     ((list? (car help))
                      (make-set-inner ls (cons (make-set (car help)) new-set)(cdr help)))
                     (else (make-set-inner ls (cons (car help) new-set) (cdr help)))))
              ((member? (car ls) (make-set (cdr ls)))
               (make-set-inner (cdr ls) new-set help))
              ((list? (car ls))
               (make-set-inner (cdr ls) new-set (cons (make-set (car ls)) help)))
              (else (make-set-inner (cdr ls) (cons (car ls) new-set) help)))))
   ; (trace make-set-inner)
    (make-set-inner ls '() '())))

;ett element är inte en lista och finns ingen dubblett på samma nivå -> spara elementet
;ett element är inte en lista och finns dubblett -> elementet kastas bort
;ett element är en lista -> gå in i elementet och eliminera dubbletter. när dubbletter tagits bort i inre listan,
;fortsätt då köra make-set på resten av den yttre listan

(displayln "test med nya make-set")

(make-set '(a (a b b (c (b b (a c (b a b) c) c) b) 3) 5 5.0 (e s) (s e s))) ;((s e) 5.0 (3 (b (c (c (b) a) b) c) b a) a)

(newline)

;procedurer från uppgift 4:

;union redan svansrekursiv, då make-set är svansrekursiv:

(define union
  (lambda (set1 set2)
    (make-set (combine-sets set1 set2))))

(displayln "test med nya union")

(union (make-set '(1 2 (x (y 3) 4))) (make-set '(b 1 (a (r)))))  ;(2 (x (y 3) 4) b 1 (a (r)))
(union (make-set '(2 3)) (make-set '((a b))))                    ;(2 3 (a b))

(newline)

;intersection måste skrivas om till svansrekursiv:

(define intersection
  (lambda (set1 set2)
    (define intersection-inner
      (lambda (set1 set2 new-set)
        (cond ((or (empty-set? set1) (empty-set? set2)) new-set)
              ((member? (first-of set1) set2)
               (intersection-inner (rest-of set1) set2 (insert-el (first-of set1) new-set)))
              (else (intersection-inner (rest-of set1) set2 new-set))
              )))
    (intersection-inner set1 set2 '())))

(displayln "test med nya difference")

(difference (make-set '(1 (2 3) b)) (make-set '(b 1)))                       ;((2 3))

(newline)

;difference måste skrivas om till svansrekursiv:

(define difference
  (lambda (set1 set2)
    (cond ((null? set2) set1)
          ((member? (first-of set2) set1)
           (difference (rem (first-of set2) set1) (rest-of set2)))
          (else (difference set1 (rest-of set2))))))

(displayln "test med nya intersection")

(intersection (make-set '(1 (2 (1)) 3 (1 2)))(make-set '(((1) 2) 3 4 (2 3)))) ;((2 (1)) 3)

(newline)

;procedurer från uppgift 5

;cartesian product måste skrivas om till svansrekursiv:

(define cartesian-product
  (lambda (set1 set2)
    (define cartesian-product-inner
      (lambda (set1 set2 save new-vector)
        (cond ((empty-set? set2) new-vector)
              ((empty-set? set1) (cartesian-product-inner save (rest-of set2) save new-vector))
              (else (cartesian-product-inner (rest-of set1) set2 save (insert-el (vector (first-of set1) (first-of set2)) new-vector))))))
    (cartesian-product-inner set1 set2 set1 (make-empty))))

;alternativt mha map

(define cartesian
  (lambda (set1 set2)
    (define cartesian-inner
      (lambda (set1 set2 new-vector)
        (if (empty-set? set2)
            new-vector
            (cartesian-inner set1 (rest-of set2) (combine-sets (do-to-each(lambda (x) (vector x (first-of set2))) set1) new-vector)))))
    (cartesian-inner set1 set2 (make-empty))))
              

(displayln "test med nya cartesian product")

(cartesian-product (make-set '(1 2 3)) (make-set '(a b)))  ;(#(1 a) #(2 a) #(3 a) #(1 b) #(2 b) #(3 b))

(newline)

;då är alla procedurer svansrekursiva.

;uppgift 7

;Gör en ny variant av proceduren make-set, make-set-from-elems som inte tar elementen i en lista, utan tar dessa som argument "direkt".

;make-set-from-elems är en konstruktor som tar emot ett godtyckligt antal argument (minst 1) och skapar en mängd utifrån att varje argument är ett element i den nya mängden.

(define make-set-from-elems
  (lambda (el . els)
    (make-set (insert-el el els))))

(displayln "make-set-from-elems test")

(make-set-from-elems 'a '(a b b (c b) 3) 5 5.0 '(e s) '(s e s)) ;((s e) 5.0 (3 (b c) b a) a)

(newline)

;uppgift 8

;gör nya varianter av union*, difference* och cartesian-product* (cartesian-product frivillig)som tar godtyckligt många argument.

;skapar en hjälpfunktion if-set som kommer att undersöka för varje element i en lista, om den är en lista, och då plocka ut elementen från listan, men bara på första nivån.
;listor på större djup sparas fortfarande som listor. Denna hjälpfunktion skapade jag för att använda i union* och difference*

(define if-set
  (lambda(set)
    (cond ((empty-set? set) (make-empty))
          ((set? (first-of set)) (combine-sets (first-of set) (if-set (rest-of set))))
          (else (insert-el (first-of lst) (if-set (rest-of lst)))))))

;svansrekursiv variant

(define if-set
  (lambda (set)
    (define if-set-inner
      (lambda (set new-set)
        (cond ((empty-set? set) new-set)
              ((set? (first-of set)) (if-set-inner (rest-of set) (combine-sets (first-of set) new-set)))
              (else (if-set-inner (rest-of set) (insert-el (first-of set) new-set))))))
    (if-set-inner set (make-empty))))

;union* är en konstruktor som givet ett godtyckligt antal mängder, skapar unionen mellan alla dessa mängder.

(define union*
  (lambda (set . sets)
    (make-set (union set (if-set sets)))))

;difference* är en konstruktor som givet ett godtyckligt antal mängder,skapar differensen mellan dessa.

(define difference*
   (lambda (set . sets)
    (make-set (difference set (if-set sets)))))

;tester uppgift 8

(displayln "union* test")

(union* (make-set '(1 2 3)) (make-set '()) (make-set '(3 4)) (make-set '(4)))  ;(1 2 3 4)

(newline)

(displayln "difference* test")

(difference* (make-set '(1 2 3)) (make-set '()) (make-set '(3 4)) (make-set '(4))) ;(1 2)

(newline)

|#