; Suppose that the withdrawals made by concurrent users are implemented
; as two separate processes which share  a common variable balance

(define (withdraw amount)
  (if (>= (balance amount))
    (begin (set! balance (- balance amount))
           balance)
    ("you do not have enough money")))

; if we see the line in which mutation is done
; (set! balance (- balance amount)
;
; it consists of three steps, and two processes that share the balance
; variable can interleave execution in every moment making results of both
; processes incorrect
;

