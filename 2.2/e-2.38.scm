; Exercise 2.38.
;
; The accumulate procedure is also known as fold-right, because it
; combines the first element of the sequence with the result of combining
; all the elements to the right. There is also a fold-left, which is similar
; to fold-right, except that it combines elements working in the opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; What are the values of

; (fold-right / 1 (list 1 2 3))
; (fold-left / 1 (list 1 2 3))
; (fold-right list nil (list 1 2 3))
; (fold-left list nil (list 1 2 3))

; Give a property that op should satisfy to guarantee that fold-right
; and fold-left will produce the same values for any sequence.
; ----------------------------------------

(load "../helpers.scm")
(load "../common.scm")

; First lets define accumulate under fold-right name
(define fold-right accumulate)

; First can be presented as 
; (/ 1 (/ 2 (/ 3 1)))
; (/ 1 (/ 2 3))
; (/ 1 0.66666666)
; 1.5
; (output (fold-right / 1 (list 1 2 3)))

; Second is 
;
; (/ (/ (/ 1 1) 2) 3)
; (/ (/ 1 2) 3)
; (/ 0.5 3)
; 0.166666666666
; (output (fold-left / 1 (list 1 2 3)))

; just replace operators with list
; (list 1 (list 2 (list 3 nil)))
; (1 (2 (3 ()))) 
; (output (fold-right list nil (list 1 2 3)))

; replacing again operator
; (list (list (list nil 1) 2) 3)
; (((() 1) 2) 3)
; (output (fold-left list nil (list 1 2 3)))

; Operation should have associative property
; (op (op a b) c) === (op a (op b c))
; For example + or *
; (output (fold-right + 0 (list 1 2 3)))
; (output (fold-left + 0 (list 1 2 3)))

; (output (fold-right * 1 (list 1 2 3)))
; (output (fold-left * 1 (list 1 2 3)))
