; Exercise 3.77.  The integral procedure used above was analogous to the
; ``implicit'' definition of the infinite stream of integers in section
; 3.5.2. Alternatively, we can give a definition of integral that is
; more like integers-starting-from (also in section 3.5.2):

(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? integrand)
                   the-empty-stream
                   (integral (stream-cdr integrand)
                             (+ (* dt (stream-car integrand))
                                initial-value)
                             dt))))

; When used in systems with loops, this procedure has the same problem
; as does our original version of integral. Modify the procedure so that
; it expects the integrand as a delayed argument and hence can be used
; in the solve procedure shown above.
; ------------------------------------------------------------

; So if we expect delayed argument in the procedure then we have to
; force it to get its value and to delay it again when passing in
; recursion

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? (force delayed-integrand)) ; here forcing the evaluation
                   the-empty-stream
                   (integral (delay (stream-cdr (force delayed-integrand))) ; here delaying the tail again
                             (+ (* dt (stream-car integrand))
                                initial-value)
                             dt))))
