;lektionshandledning 10

;labb 5 tips

(define element-of-set?
  (lambda (element set)
    (cond ((null? set) #f)
          ((= element (car set)) #t)
          (else (element-of-set element (cdr set))))))

;linjärsökning - titta på ett element i taget. O(n) "linjär tidskomplexitet"

(define adjoin-set
  (lambda (element set)
    (if (element-of-set? element set)
        set
        (cons element set))))

;O(n), ty element-of-set? har tidskomplexitet O(n) och adjoin-set O(1).

(define intersect
  (lambda (set-1 set-2)
    (cond ((or (null? set-1) (null? set-2))
           '())
          ((element-of-set? (car set-1) set2)
           (cons (car set-1) (intersect (cdr set-1) set-2)))
          (else (intersect (cdr set-1) set-2)))))
        
;O(n^2), ty element-of-set? O(n) och intersect O(n)

(define union
  (lambda (set-A set-B)
    (cond ((null? set-A)
           set-B)
          ((null? set-B)
           set-A)
          ((element-of-set? (car set-A) set-B)
           (union (cdr set-A) set-B))
          ;(else (union (cdr set-A) (cons (car set-A) set-B))))))
          (else (cons (car set-A) (union (cdr set-A) set-B))))))

;O(n^2), ty element-of-set? O(n) och union O(n)
