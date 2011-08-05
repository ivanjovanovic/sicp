; Exercise 1.5 Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using 
; applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; then he tests the interpreter with
; (test 0 (p))

; Point is that in normal-order evaluation (p) will try to expand itself even if it doesn't need to
; be evaluated by the interpreter at all for these conditions.
; problem is that (p) expands to itself and then ends up in recursion.
;
; On the other hand, applicative-order evaluation will evaluate argument y which is (p) only
; if it is needed for evaluatin of test procedure with given params. Therefore with this 
; evaluation strategy we will have no problems getting the result from given test.
