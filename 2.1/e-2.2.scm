; Exercise 2.2. 
;
; Consider the problem of representing line segments in a plane. Each segment is
; represented as a pair of points: a starting point and an ending point. Define a constructor
; make-segment and selectors start-segment and end-segment that define the representation of
; segments in terms of points. Furthermore, a point can be represented as a pair of numbers: the x
; coordinate and the y coordinate. Accordingly, specify a constructor make-point and selectors
; x-point and y-point that define this representation. Finally, using your selectors and constructors,
; define a procedure midpoint-segment that takes a line segment as argument and returns its midpoint
; (the point whose coordinates are the average of the coordinates of the endpoints). To try your procedures,
; you'll need a way to print points:

; needing average procedure
(load "../common.scm")

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

; In the terms of wishful thinking we start by designing higher levels
; of abstraction first, so we do not deal with details of implementation
; yet and just implement them when needed.

(define (make-segment sp ep)
  (cons sp ep))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

; Now we need points in fact

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define start (make-point 0 0))
(define end (make-point 2 2))

; (print-point start)
; (print-point end)

; now we calculate midpoint which itself is a point with calculated
; coordinates. It receives segment as param

(define (mid-point s)
  (make-point
    (average (x-point (car s)) (x-point (cdr s)))
    (average (y-point (car s)) (y-point (cdr s)))))

; (print-point (mid-point (make-segment start end)))
