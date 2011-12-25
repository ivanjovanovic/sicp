; Exercise 2.49.  Use segments->painter to define the following
; primitive painters:

; a.  The painter that draws the outline of the designated frame.

; b.  The painter that draws an ``X'' by connecting opposite corners of the frame.

; c.  The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.

; d.  The wave painter.
; --------------------------------------------------------

; I have to define list of segments that define the painters

; 1. outine of the frame

(define outline-painter
  (define outline-segment-list 
    (list
      (make-segment (make-vect 0 0) (make-vect 1 0))
      (make-segment (make-vect 1 0) (make-vect 1 1))
      (make-segment (make-vect 1 1) (make-vect 0 1))
      (make-segment (make-vect 0 1) (make-vect 0 0))))
  (segments->painteri outline-segment-list))

(define x-painter
  (define x-segment-list
    (list 
      (make-segment (make-vect 0 0) (make-vect 1 1))
      (make-segment (make-vect 0 1) (make-vect 1 0))))
  (segments->painter x-segment-list))

(define diamond-painter
  (define diamond-segment-list
    (list 
      (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
      (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
      (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
      (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))
  (segments->painter diamond-segment-list))

; I don't do 4th since there is nothing to learn from doing it and I
; would need to define the points for every line ...


