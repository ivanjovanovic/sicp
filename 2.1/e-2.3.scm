; Exercise 8.3. Implement a representation for rectangles in a plane. (Hint: You may want to make use of
; exercise 8.8.) In terms of your constructors and selectors, create procedures that compute the perimeter
; and the area of a given rectangle. Now implement a different representation for rectangles. Can you
; design your system with suitable abstraction barriers, so that the same perimeter and area procedures will
; work using either representation?

(load "e-2.2.scm")

; in general we can make rectangle with 4 points, which are in fact two
; segments, if we want to reuse the abstractions from previous exercise.
; I will avoid restrictions needed to be made in order to ensure that
; angles are all normal.

; it receives segment top and segment bottom.
(define (make-rect st sb)
  (cons st sb))

(define (top-left r)
  (car (car r)))

(define (top-right r)
  (cdr (car r)))

(define (bottom-left r)
  (car (cdr r)))

(define (bottom-right r)
  (cdr (cdr r)))

(define (rect-area r)
  (*
    (- (x-point (top-right r)) (x-point (top-left r)))
    (- (y-point (top-right r)) (y-point (bottom-right r)))))

(define (rect-perim r)
  (* 2
     (+ 
       (- (x-point (top-right r)) (x-point (top-left r)))
       (- (y-point (top-right r)) (y-point (bottom-right r))))))

(define my-rect 
  (make-rect 
    (make-segment 
      (make-point 0 8) 
      (make-point 8 8))
    (make-segment
      (make-point 0 0)
      (make-point 8 0))))

; (display (rect-area my-rect))
; (newline)

; (display (rect-perim my-rect))
; (newline)

; here we see that in both cases we have to calculate lengths of sides
; of rectangle and that we need these selectors in fact to make proper
; level of abstraction on top of rectangle concept

(define (rect-width r)
  (- (x-point (top-right r)) (x-point (top-left r))))

(define (rect-height r)
  (- (y-point (top-right r)) (y-point (bottom-right r))))

; and now we redefine the procedures
(define (rect-area r)
  (*
    (rect-width r)
    (rect-height r)))

(define (rect-perim r)
  (* 2
     (+ 
       (rect-width r)
       (rect-height r))))

; (display (rect-area my-rect))
; (newline)

; (display (rect-perim my-rect))
; (newline)
;
; This now produces the same result and is expressed in much simpler
; way. Now if we want to change how we represent height and width, and how we
; count points we can do that without problem since we don't have to
; take care of points any more which are in the lower level of
; abstraction.
;
; Isolation layers look something like this now
;
;                      
;            Area / Perimeter
;     ------------------------------------
;     Side widths/points selectors and constructor
;     ------------------------------------
;     segment constrctors and selectors
;     -----------------------------------
;     Points constructors and selectors
;     ----------------------------------
;                   Pairs
;
; 
; One idea to note here is that we should not jump accross abstractions
; when building our software. In upper layer of abstraction, we should
; commnicate/use only the abstraction that is on the one layer beyond.
;
;
;    

