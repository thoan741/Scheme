(require (lib "trace.ss"))
(require racket/list)
(require racket/vector)


;uppgift 9
;member? mha ormap:

(define member?
  (lambda (el set)
    (ormap (lambda (x) (equal? el x)) set)))

;subset? mha andmap

(define subset?
  (lambda (set-1 set-2)
    (andmap (lambda (x) (member? x set-2)) set-1)))

;intersection mha filter

(define intersection
  (lambda (set-1 set-2)
    (filter (lambda (x) (if (list? x)
                            (subset? x set-2)
                            (member? x set-2))) set-1)))

;difference mha filter

(define difference
  (lambda (set-1 set-2)
    (filter (lambda (x) (not (if (list? x)
                                 (subset? x set-2)
                                 (member? x set-2)))) set-1)))

;cartesian product mha map två gånger (inte helt rätt)
(define cartesian-product
  (lambda (set-1 set-2)
    (map (lambda (x)
           (map (lambda (y)
                  (vector x y))
                set-1))
    set-2)))

;make-set mha map & remove-duplicates
(define make-set
  (lambda (lst)
    (remove-duplicates
     (map (lambda (x)
            (if (list? x)
                (remove-duplicates x)
                x))
          lst))))








(cartesian-product '(1 2 3) '(4 5 6))
