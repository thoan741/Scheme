;Laboration 3


;; GRAPH.SCM
;;
;; Graph is a simple graphics package in scheme.
;;
;; revision made by Serafim Dahl 2009-09-18
;;;; to make graph.scm independent of simple-draw
;;;; by including the content of simple-draw in
;;;; this file
;; revision made by Per Sedholm 2008-09-15
;; revision made by Serafim Dahl 2005-08-29
;; revision made by Marco Wesselgren 1997-09-14
;; Many thanks

;; ------------------------------------------------
;; RÃ¶r inte -- BehÃ¶vs fÃ¶r att drawGraph ska fungera
(require (lib "draw.ss" "htdp")
	 (lib "posn.ss" "lang"))

(define draw-a-point
  (lambda (X_coord Y_coord . color)
    (if (null? color)
	(draw-solid-disk (make-posn X_coord Y_coord) 1)
	(draw-solid-disk (make-posn X_coord Y_coord) 1 (car color)))))

(define draw-a-line
  (lambda (X_coord_1 Y_coord_1 X_coord_2 Y_coord_2 . color)
    (if (null? color)
	(draw-solid-line (make-posn X_coord_1 Y_coord_1)
			 (make-posn X_coord_2 Y_coord_2))
	(draw-solid-line (make-posn X_coord_1 Y_coord_1)
			 (make-posn X_coord_2 Y_coord_2)
			 (car color)))))

(define draw-a-circle
  (lambda (X_coord Y_coord radius . color)
    (if (null? color)
	(draw-circle (make-posn X_coord Y_coord) radius)
	(draw-circle (make-posn X_coord Y_coord) radius (car color)))))

(define clear clear-all)

;;; -----------------------------------------------------------------
;;; ---------- allt hÃ¤rovanfÃ¶r mÃ¥ste vara intakt, rÃ¶r INTE ----------

;; Starta ett grafikfÃ¶nster
; (start 400 400)
;; eller (start b h) ;; dÃ¤r b Ã¤r bredden och h Ã¤r hÃ¶jden
;;

;; HÃ¤r bÃ¶rjar det som man kan (ska) Ã¤ndra i

;; makeGraph -- constructor for graphical units.
(define makeGraph
  (lambda (name shape)
    ;(cons shape name)))
    (cons name shape)))

;; name -- selector for the name of a graphical unit
(define name
  (lambda (graph)
    ;(cdr graph)))
    (car graph)))

;; shape -- selector for the shape of a graphical unit
(define shape
  (lambda (graph)
    ;(car graph)))
    (cdr graph)))

;;;;;;;;;;
;; Constructors for the various graphical units.
;;
;; TODO: Delete the quoted text and write your code in its place.

;; makePoint -- constructor for 'point
(define makePoint
  (lambda (x0 y0)
    (makeGraph 'point (list x0 y0))))                 ;skapar en lista (point x0 y0)

(define X_coord          ;; Re-defined later; see below
  (lambda (point)
    (car (cdr point))))                               ;skriver ut andra elementet i listan point.

(define Y_coord          ;; Re-defined later; see below
  (lambda (point)
    (car (cdr (cdr point)))))                         ;skriver ut tredje elementet i listan point.

;; makeCircle -- constructor for 'circle
(define makeCircle
  (lambda (x0 y0 radius)
    (makeGraph 'circle
               (list (makePoint x0 y0) radius))))     ;skapar en lista (circle (point x0 y0) radius).

;; makeLine -- constructor for 'line
(define makeLine
  (lambda (x0 y0 x1 y1)
    (makeGraph 'line                       ;`line
               (list (makePoint x0 y0)
                     (makePoint x1 y1)))))            ;skapar en lista (line (point x0 y0) (point x1 y1)).

;; makeRectangle -- constructor for 'rectangle
(define makeRectangle
  (lambda (x0 y0 x1 y1)
    (makeGraph 'rectangle
               (list (makePoint (min x0 x1) (max y0 y1))
                     (makePoint (max x0 x1) (min y0 y1))))))  ;skapar en lista (rectangle (point minsta-x största-y) (point största-x minsta-y))
               

;; makePict -- constructor for 'pict
(define makePict
  (lambda (picts)
    (makeGraph 'pict picts)))             ;`pict

;;;;;;;;;;
;; Help functions for safer selectors


(define Radius           ;; Re-defined later; see below
  (lambda (circle)
    (if (eq? (name circle) 'circle)
        (cadr (shape circle))
        (errormessage "this does not work with" circle))))

;; errormessage -- report an error.
;;   ``string'' is the error message
;;   ``object'' is the object that caused the error.
(define errormessage
  (lambda (string object)
    (error (string-append string
                          " "
                          (symbol->string (name object))))))
#|
(define X_coord          ;; Re-defined later; see below
  (lambda (point)
    (if (eq? (name point) 'point)
        (car (shape point))
        (errormessage "this does not work with" point))))
|#
(define retrieve
  (lambda (Graph expected_name selection)                   ;3 invariabler, en lista, ett förväntat namn och en selektor
    (if (eq? (name Graph) expected_name)                    ;är listans namn samma som det förväntade namnet?
        (selection (shape Graph))                           ;använda selektorn på listans "kropp"
        (errormessage "this does not work with"  Graph))))  ;felmeddelande om listans namn inte stämmer med förväntade namnet.

;;;;;;;;;;
;; Selectors for the various graphical units.
;;
;; NB: Some of these replaces previous versions. Normally, the code
;;     for the previous versions should be removed. In this case, we
;;     (the assistants) may want to look at them, so it's best to
;;     simply leave them in place.


;; X_coord -- select the X coordinate from the given 'point
(define X_coord
  (lambda (point)
    (retrieve point 'point car)))

;; Y_coord -- select the Y coordinate from the given 'point
(define Y_coord
  (lambda (point)
    (retrieve point 'point cadr)))

;; Lower_left_corner -- select the lower left (or south west) corner
;; of the given 'rectangle
(define Lower_left_corner
  (lambda (rectangle)
    (retrieve rectangle 'rectangle car)))

;; Upper_right_corner -- select the upper right (or north east) corner
;; of the given 'rectangle
(define Upper_right_corner
  (lambda (rectangle)
    (retrieve rectangle 'rectangle cadr)))

;; Lower_right_corner -- select the lower right (or south east) corner
;; of the given 'rectangle

(define Lower_right_corner
  (lambda (rectangle)
    (makePoint
     (X_coord (Upper_right_corner rectangle))
     (Y_coord (Lower_left_corner rectangle)))))
#|
(define Lower_right_corner
  (lambda (rectangle)
    (makePoint (car (shape (cadr (retrieve rectangle 'rectangle append))))
               (cadr (shape (car (retrieve rectangle 'rectangle append)))))))
|#        

;; Upper_left_corner -- select the upper left (or north west) corner
;; of the given 'rectangle
#|
(define Upper_left_corner
  (lambda (rectangle)
    (makePoint (car (shape (car (retrieve rectangle 'rectangle append))))
               (cadr (shape (cadr (retrieve rectangle 'rectangle append)))))))
|#
(define Upper_left_corner
  (lambda (rectangle)
    (makePoint
     (X_coord (Lower_left_corner rectangle))
     (Y_coord (Upper_right_corner rectangle)))))


;; Origo -- select the center of the given 'circle
(define Origo
  (lambda (circle)
    (retrieve circle 'circle car)))         ;argumentet är på formen '(circle (point x y) radius), vi vill hämta ut '(point x y) som är mittpunkt

;; Radius -- select the radius of the given 'circle
(define Radius
  (lambda (circle)
    (retrieve circle 'circle cadr)))

;; Startpoint -- select the first point of the given 'line
(define Startpoint
  (lambda (line)
    (retrieve line 'line car)))

;; Endpoint -- select the second (last) point of the given 'line
(define Endpoint
  (lambda (line)
    (retrieve line 'line cadr)))

;; Graphs -- select the list of graphical units from the given 'pict
(define Graphs
  (lambda (graph)
    (retrieve graph 'pict append)))

;; First_of -- select the first of the graphical units from the given
;; 'pict
(define First_of
  (lambda (graphs)
    (retrieve graphs 'pict car)))

;; Rest_of -- create a new 'pict with the second through last (i.e.
;; all except the first) of the graphical units of the given 'pict
(define Rest_of
  (lambda (graphs)
    (if (eq? (name (list 1 0)) 1)
        (cons (name graphs)(retrieve graphs 'pict cdr))
        (cons (retrieve graphs 'pict cdr)(name graphs)))))

;; no_more? -- predicate to determine whether the given 'pict has any
;; graphical units. Returns #t if so, #f otherwise.
(define no_more?
  (lambda (graphs)
    (retrieve graphs 'pict null?)))

;; --- /Assignment 7/ ---
;; Re-define makeGraph.
; (define makeGraph
;   (lambda (name shape)
;     (cons shape name)))
;
;; TODO: Additional code for assignment 7 should also be added here.

;; drawGraph can be used to draw the graphical units on the computer
;; screen.

(define (drawGraph object)
  (cond ((null? object) '())           ;nil
        ((equal? 'point (name object))
         (draw-a-point (X_coord object)
                       (Y_coord object)))

        ((equal? 'circle (name object))
         (draw-a-circle (X_coord (Origo object))
                        (Y_coord (Origo object))
                        (Radius object)))

        ((equal? 'line (name object))
         (draw-a-line (X_coord (Startpoint object))
                      (Y_coord (Startpoint object))
                      (X_coord (Endpoint object))
                      (Y_coord (Endpoint object))))

        ((equal? 'rectangle (name object))
         (draw-a-line (X_coord (Lower_left_corner object))
                      (Y_coord (Lower_left_corner object))
                      (X_coord (Upper_left_corner object))
                      (Y_coord (Upper_left_corner object)))
         (draw-a-line (X_coord (Upper_left_corner object))
                      (Y_coord (Upper_left_corner object))
                      (X_coord (Upper_right_corner object))
                      (Y_coord (Upper_right_corner object)))
         (draw-a-line (X_coord (Upper_right_corner object))
                      (Y_coord (Upper_right_corner object))
                      (X_coord (Lower_right_corner object))
                      (Y_coord (Lower_right_corner object)))
         (draw-a-line (X_coord (Lower_right_corner object))
                      (Y_coord (Lower_right_corner object))
                      (X_coord (Lower_left_corner object))
                      (Y_coord (Lower_left_corner object))))
        ((and (equal? 'pict (name object))
              (not (no_more? object)))
             (begin
               (drawGraph (First_of object))
               (drawGraph (Rest_of object))))))

#|
        ((equal? 'pict (name object))
         (if (not (no_more? object))
             (begin
               (drawGraph (First_of object))
               (drawGraph (Rest_of object)))))))
|#

;;;;;;;;;;
;; Test figure
(begin
  (start 400 400)
  (clear)
  (define r 100)
  (define x0 200)
  (define y0 200)
;  (define pi (* 4 (atan 1)))
  (define x/2 (* (/ (sqrt 3) 2) r))
  (define fig
    (makePict (list
               (makePoint x0 y0)
               (makeRectangle (- x0 r) (- y0 r) (+ x0 r) (+ y0 r))
               (makeCircle x0 y0 r)
               (makePict (list (makeLine x0         (- y0 r)
                                         (- x0 x/2) (+ y0 (/ r 2)))
                               (makeLine x0         (- y0 r)
                                         (+ x0 x/2) (+ y0 (/ r 2)))
                               (makeLine (- x0 x/2) (+ y0 (/ r 2))
                                         (+ x0 x/2) (+ y0 (/ r 2))))))))
  (drawGraph fig)
  )
