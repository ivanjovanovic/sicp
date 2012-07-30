; Exercise 5.34.  Compile the iterative factorial procedure

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

; Annotate the resulting code, showing the essential difference between the
; code for iterative and recursive versions of factorial that makes one process
; build up stack space and the other run in constant stack space.
; ------------------------------------------------------------

(load "5.5.scm")

(define object-code
  (compile
    '(define (factorial n)
        (define (iter product counter)
          (if (> counter n)
            product
            (iter (* counter product)
                  (+ counter 1))))
        (iter 1 1))
    'val
    'next))

(format-object-code (instruction-list object-code))

; So, here we have to analyze the part where the recursively defined procedure has to call itself.
;
; In the case of iterative process, it is just enough to call itself again with the current state
; of registers without storing anything on stack. We do not need data on stack since we do not need to
; do further computations on that level of execution after the recursively called procedure returns.
;
; In the case of recursive process, we have to store the partial argument list for the expression
; (* (fact (- n 1)) n) because we have to wait for internal factorial to finish execution and only then
; we can finish computation of this one. Beside argument list, we can see that it preserves as well
; the continue and proc register as part of the state in that particular moment.
;
; Moral of this is that we should avoid pausing one computation and run another and wait for its result
; because every time we do that we have to preserve the whole environment needed to compute the calling level
; after callee has finished.
