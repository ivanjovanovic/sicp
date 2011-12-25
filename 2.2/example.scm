; Picture language example is interesting case of using data
; abstraction, closure property and higher order functions.
;
; It is based on manipulating the image in order to make more complex
; patterns by transforming image and organizing it in relation to other
; transformations in a more complex structures.
;
; If we take one graphical element caled painter as only primitive of
; the language we want to create then we can create more complex
; structure by definining following procedures.
;
; We can define some means of combinations of our data

; this will create one picture beside the other where second is inverted
; vertically.
(define wave2 (beside wave (flip-vert wave)))

; now by combining these two we make even more complex structure
(define wave4 (below wave2 wave2))

; or even in more abstract way
(define (flipped-pairs wave)
  (let ((painter2 (beside wave (flip-vert wave))))
    (below painter2 painter2)))

; and then we can more easy define sqared form
(define wave4 (flipped-pairs wave))

; also some recursive operations.
(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

; We can go even more abstract in this by defining general
; transformation which accepts what operation to apply to top-left,
; top-right, bottom-left and bottom-right elements.

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

; In order to transform certain painter in the particular frame we
; define mapping proceudre that given a vector will  return new vector
; in the new coordinate system
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


; in order to build painters from segments, we define procedure
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

; painter is the procedure that will, given the frame render list of
; segments adjusted to the it.
;
; In the exercises we'll have to create specialized painters for
; different tasks that already have segment list predefind for their
; purpose

; Since painter is just rendering to a given frame, if we want to
; transform how certain output looks we just have to modify frame before
; we give it to the painter. We can make an general procedure for
; transforming painters

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))


; Then using this we can define soem transformations

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

; As well we can define a combination of painters to make more complex
; structures. Here we produce new painter function that will split the
; frame on half, make two new painters to write into these frames and
; then execute two painters where each will write ito its own frame

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


