; Exercise 2.51.
;
; Define the below operation for painters. Below takes
; two painters as arguments. The resulting painter, given a frame, draws
; with the first painter in the bottom of the frame and with the second
; painter in the top. Define below in two different ways -- first by
; writing a procedure that is analogous to the beside procedure given
; above, and again in terms of beside and suitable rotation operations
; (from exercise 2.50).
; --------------------------------------------------

; first the same way beside does it by splitting into two 
; frames and returning composing function that will render both
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               split-point
                               (make-vect 1.0 .0.0)))
          (paint-top
            (transform-painter painter2
                               split-point
                               (make-vect 0.0 1.0)
                               (make-vect 0.5 1.0)))))
    (lambda (frame)
      (paint-bottom frame)
      (paint-top frame))))

; the other way is to reuse beside and just rotate it first 180 and then
; additional 270, that way we'll get 450 degrees of rotation which is 90
; degrees real, and that will make it one below eachother. Before that
; we have to rotate each individual painter so they are positioned
; properly at the end. We could have defined as well transformation of
; 90 degrees ...
(define (below painter1 painter2)
  (rotate-270-cc 
    (rotate-180-cc 
      (beside (rotate-270-cc painter1) (rotate-270-cc painter2)))))
