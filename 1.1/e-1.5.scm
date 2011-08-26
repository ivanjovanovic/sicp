; Exercise 1.5 Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using 
; applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; then he tests the interpreter with
; (test 0 (p))

; Normal order evaluation:
;   In this case operand (p) will not be evaluated until it is needed by
;   some primitive operation and thus this will return 0 as result.
;
; Applicative order evaluation:
;   In this case operand y will be by default evaluated and then it will
;   end up in recursion since (p) points to itself.
