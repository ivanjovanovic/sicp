; Exercise 2.50.
;
; Define the transformation flip-horiz, which flips
; painters horizontally, and transformations that rotate painters
; counterclockwise by 180 degrees and 270 degrees.
; --------------------------------------------------

; First to flip horizontally
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


; Counterclockwise 180
(define (rotate-180-cc painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 1.0)))

; Counterclockwise 270
(define (rotate-270-cc painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


