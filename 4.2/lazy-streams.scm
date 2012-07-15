; Based on the lazy evaluation of procedure arguments, we can implement
; list abstraction so they are naturally lazy. We just have to implement
; pair as a procedure that will represent them and selectors just as
; applications of this procedure.

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

; When defined this way in a lazy language, lists are even lazier than streams. Streams
; have car always as immediate value, and here we have even car value as thunk that has
; to be forced.
