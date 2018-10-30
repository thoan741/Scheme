(define my-list '(1 2 3 4))
(define (square x) (* x x))


;; LIST.SCM
;;

;;;;;;;;;;
;; Assignment 1

;; list-create -- create an empty list
(define list-create
  (lambda ()
    '()))

;(list-create)

;; list-empty -- ???
(define list-empty?
  (lambda (ls)
    (if (null? ls)
        #t
        #f)))

;(list-empty? my-list)
;(list-empty? '())



;; list-insert -- ???
(define list-insert
  (lambda (el ls)
    (cons el ls)))

;(list-insert 0 my-list)

; list-delete -- ???
(define list-delete
  (lambda (el ls)
    (cond ((null? ls) '())
          ((eq? (car ls) el) (list-delete el (cdr ls)))
          (else (cons (car ls) (list-delete el (cdr ls)))))))

;;kan skrivas enklare mha remove*:
;(define list-delete
;  (lambda (el ls)
;    (remove* (list el) ls)))

;; list-first -- ???
(define list-first
  (lambda (ls)
    (car ls)))

;(list-first my-list)

;; list-rest -- ???
(define list-rest
  (lambda (ls)
    (cdr ls)))

;(list-rest my-list)

; list-len -- ???
(define list-len
  (lambda (ls)
    (if (null? ls)
        0
        (+ 1 (list-len (cdr ls))))))

;;kan göras enklare mha length:
;(define list-len
;  (lambda (ls)
;    (length ls)))

;(list-len my-list)
;(list-len '())

; list-nth -- ???
(define list-nth
  (lambda (n ls)
    (if (= n 1)
        (car ls)
      (list-nth (- n 1) (cdr ls)))))

;;enklare mha list-ref:
;(define list-nth
;  (lambda (n ls)
;    (list-ref ls (sub1 n))))

;(list-nth 2 my-list)



;;;;;;;;;;
;; Test of list functions (supplied by course team)

;; list-test -- run list test with an (initially) empty list
(define list-test
  (lambda ()
    (list-edit (list-create))))

;; list-edit -- interactively test list manipulation functions
(define list-edit
  (lambda (lst)
    (display "Edit operation (insert/delete/print/nth/first/rest/end): ")
    (let ((oper (read)))
      (cond ((or (eq? oper 'insert)
                 (eq? oper 'i))
             (list-edit (list-insert (read-element "Element to insert: ")
                                     lst)))
            ((or (eq? oper 'delete)
                 (eq? oper 'd))
             (list-edit (list-delete (read-element "Element to delete: ")
                                     lst)))
            ((or (eq? oper 'nth)
                 (eq? oper 'n))
             (elem-print (list-nth (read-element "Element number: ")
                                   lst))
             (newline)
             (list-edit lst))
            ((or (eq? oper 'print)
                 (eq? oper 'p))
             (list-print lst)
             (list-edit lst))
            ((or (eq? oper 'rest)
                 (eq? oper 'r))
             (list-print (list-rest lst))
             (list-edit lst))
            ((or (eq? oper 'first)
                 (eq? oper 'f))
             (elem-print (list-first lst))
             (newline)
             (list-edit lst))
            ((or (eq? oper 'end)
                 (eq? oper 'e)) lst)
            (else (display "Unknown operation")
                  (newline)
                  (list-edit lst))))))

;; read-element -- prompt the user with the given string, then
;; return the answer
(define read-element
  (lambda (operation)
    (display operation)
    (read)))

;; list-print -- print each element in the given list, separated
;; by " "
(define list-print
  (lambda (lst)
    (cond ((list-empty? lst) (newline))
          (else (display (list-first lst))
                (display " ")
                (list-print (list-rest lst))))))

;; elem-print -- print the given element
(define elem-print
  (lambda (elem)
    (display elem)))

;;;;;;;;;;
;; Assignment 2

;(define mylist (list-edit (list-create)))

;definiera en funktion do-to-each som applicerar en procedur på alla element i listan

(define do-to-each
  (lambda (proc ls)
    (if (list-empty? ls)
        (list-create)
        (list-insert (proc (list-first ls))(do-to-each proc (list-rest ls))))))

;;kan göras enklare mha map:
;(define do-to-each
;  (lambda (proc ls)
;    (map proc ls)))

(define add-one
  (lambda (x)
    (+ 1 x)))

;;;;;;;;;;
;; Assignment 3

;skapa procedurer som returnerar procedurer

(define add-n
  (lambda (n)
    (lambda (x) (+ x n))))

(define mul-n
  (lambda (n)
    (lambda (x) (* x n))))

(define div-n
  (lambda (n)
    (lambda (x) (quotient x n))))

(define expt-n
  (lambda (n)
    (lambda (x) (expt n x))))



(define add-two (add-n 2))
;
;(do-to-each add-two my-list)
;
(define mul-two (mul-n 2))
;
;(do-to-each mul-two my-list)
;
(define div-two (div-n 2))
;
;(do-to-each div-two my-list)
;
(define expt-two (expt-n 2))
;
;(do-to-each expt-two my-list)

;uppgift 4

;definiera en funktion even-odd som tar 2 argument som är procedurer. om x är jämnt ska proc-even appliceras på x, och om x är udda ska proc-odd appliceras på x

(define even-odd
  (lambda (proc-even proc-odd)
    (lambda (n)
      (if (even? n)
          (proc-even n)
          (proc-odd n)))))

(define halve-double (even-odd div-two mul-two))

;(do-to-each halve-double my-list)

;(do-to-each (even-odd (div-n 2) (mul-n 2)) my-list)

;uppgift 5

;ska applicera proc-1 på x om predikatet på x stämmer, annars proc-2.

(define choose
  (lambda (predicate proc-1 proc-2)
    (lambda (x)
      (if (predicate x)
          (proc-1 x)
          (proc-2 x)))))

(define halve-double-2 (choose even? div-two mul-two))

;(do-to-each halve-double-2 my-list)

;uppgift 6

;ska applicera proc på första elementet i listan med resterande del av listan tills dess att cdr av listan är tom.

(define accumulate
  (lambda (proc lst)
    (if (list-empty? (list-rest lst))
        (list-first lst)
        (proc (list-first lst) (accumulate proc (list-rest lst))))))

;(accumulate * my-list)
;(accumulate + my-list)
  
(define accumulate-2
  (lambda (proc lst null-value)
    (if (list-empty? lst)
        null-value
        (proc (list-first lst) (accumulate-2 proc (list-rest lst) null-value)))))

;(accumulate-2 * my-list 1)

;(accumulate-2 cons my-list '())

;(accumulate-2 + my-list 0)

;uppgift 7

;en procedur acc-proc som returnerar en procedur, som fungerar som accumulate när den anropas med en lista.

;(define acc-proc
;  (lambda (proc)
;    (lambda (lst)
;      (define inner-loop
;        (lambda (lst)
;          (if (list-empty? (list-rest lst))
;              (list-first lst)
;              (proc (list-first lst) (inner-loop (list-rest lst))))))
;      (inner-loop lst))))

(define acc-proc
  (lambda (proc)
    (lambda (lst)
      (if (list-empty? (list-rest lst))
          (list-first lst)
          (proc (list-first lst) ((acc-proc proc) (list-rest lst)))))))

;fungerar som accumulator2:
;(define acc-proc2
;  (lambda (proc null-value)
;    (lambda (lst)
;      (if (list-empty? lst)
;          null-value
;          (proc (list-first lst) ((acc-proc proc) (list-rest lst)))))))

(define sum (acc-proc +))
(define prod (acc-proc *))

;(sum my-list)
;(prod my-list)

;uppgift 8

;en procedur do-to-each-proc som returnerar en procedur, som fungerar som do-to-each när den anropas med en lista.

;(define do-to-each-proc
;  (lambda (proc)
;    (lambda (lst)
;      (define inner-loop
;        (lambda (lst)
;          (if (list-empty? lst)
;              (list-create)
;              (list-insert (proc (list-first lst)) (inner-loop (list-rest lst))))))
;      (inner-loop lst))))

(define do-to-each-proc
  (lambda (proc)
    (lambda (lst)
      (if (list-empty? lst)
          (list-create)
          (list-insert (proc (list-first lst))
                       ((do-to-each-proc proc)(list-rest lst)))
          ))))

;;kan skrivas enklare mha map:

;(define do-to-each-proc
;  (lambda (proc)
;    (lambda (lst)
;      (map proc lst))))

(define sq-list (do-to-each-proc square))

;(sq-list my-list)

