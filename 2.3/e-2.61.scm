; Exercise 2.61.
;
; Give an implementation of adjoin-set using the ordered
; representation. By analogy with element-of-set? show how to take
; advantage of the ordering to produce a procedure that requires on the
; average about half as many steps as with the unordered representation.
; ------------------------------------------------------------

; Well, adjoin set is using the element-of-set? proceudre which already
; has in average half as many steps. Without any change we get the
; benefit.
;
(define (adjoin-set x set)
  (if (not (element-of-set? x set))
    (cons x set)
    set))
