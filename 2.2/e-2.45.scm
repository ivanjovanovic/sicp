; Exercise 2.45.  Right-split and up-split can be expressed as instances
; of a general splitting operation. Define a procedure split with the
; property that evaluating

(define right-split (split beside below))
(define up-split (split below beside))

; produces procedures right-split and up-split with the same behaviors
; as the ones already defined.
; -------------------------------------

; So, goal is to define split operation that can accept operations as
; paramters

(define (split split-position identity-position)
  (lambda (painter)
    (identity-position painter (split-position painter painter))))

; if we want to make it recursive
(define (split split-position identity-position)
  (lambda (painter n)
    (let ((smaller (split painter (- n 1))))
      (identity-position painter (split-position smaller smaller)))))
