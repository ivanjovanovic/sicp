(load "../../helpers.scm")
(load "../arithmetics.scm")
(load "../symbolic_algebra.scm")

; All the test situations should be defined here.
; This is for the sake of seeing if soemthing is awfully broken
; Not for the sake of automatizing testing.
(output (add 4 5))
(output (sub 4 5))
(output (mul 4 5))
(output (div 4 5))
(output (equ? 4 5))
(output (equ? 5 5))
(output (=zero? 5))
(output (=zero? 0))

(output (add 5 (make-rational 5 2)))
(output (add (make-rational 5 2) (make-rational 3 1)))

(output (raise (raise 5)))

(output (equ? (make-complex-from-real-imag 3 4)
              (make-complex-from-real-imag 3 4)))

(output (equ? (make-rational 3 4)
              (make-rational 3 4)))

(output (equ? 5 5))

(output (equ? 5 (make-rational 5 1)))

(output (equ? 5 (make-complex-from-real-imag 5 0)))
(output (equ? 5 (make-complex-from-real-imag 5 1)))
(output (equ? (make-complex-from-real-imag 5 1)
              (make-complex-from-real-imag 5 1)))

(output (push-down (make-rational 5 2)))
(output (push-down (make-complex-from-real-imag 5 2)))

(output (drop (make-rational 4 2))) ; 2
(output (drop (make-rational 4 3))) ; (rational 4 . 3)
(output (drop (make-complex-from-real-imag 5 0))) ; (rational 4 . 3)
(output (drop (make-complex-from-real-imag 5 1))) ; (complex rectangular 5 1)

(output (drop (add 3 5)))
(output (drop (add 3 (make-rational 5 1))))
(push-down (make-rational 4 1))
(output (pushable? (make-rational 4 2)))
(output (add (make-rational 4 1) (make-rational 4 1)))
(output (push-down (make-rational 4 1)))

(output (=zero? (make-polynomial 'x (list (list 1 0) (list 0 0)) ))) ; #t
(output (=zero? (make-polynomial 'x (list (list 1 0) (list 0 1)) ))) ; #f
