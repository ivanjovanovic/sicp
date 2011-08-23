; Library of common procedures used in exercises which do not particularly participate
; in solution design but are used only as helpers and can be shared among.

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

