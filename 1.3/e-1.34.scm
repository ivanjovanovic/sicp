; Exercise 1.34.
;
; Suppose we define the procedure

(load "../common.scm")

(define (f g)
  (g 2))

; Then we have

(display (f square)) ; 4
(newline)

(display (f (lambda (z) (* z (+ z 1))))) ; 6
(newline)

; What happens if we (perversely) ask the interpreter to
; evaluate the combination (f f)? Explain.
;
; --------------------------------------------------

(f f)

; (f f) - will evaluate to
; (f 2) - will evaluate to
; (2 2) - will throw error because it expects first term to be procedure


