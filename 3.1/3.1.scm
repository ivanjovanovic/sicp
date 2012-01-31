(load "../helpers.scm")

; For the first time we introduce possibility that one procedure returns
; different result when called

(define balance 100)

(define (withdraw amount)
  (if (> balance amount)
    (begin
      (set! balance (- balance amount))
      balance) ; returns current state of balance
    "Insufficient funds"))

(output (withdraw 20)) ; 80
(output (withdraw 20)) ; 60
(output (withdraw 200)) ; "Insufficient funds"

; here we use (set! <name> <new-value>) to set new value of a variable

; Here balance is kind of exposed to the public and other procedures
; could influence it without really wanting to do that. We can hide it
; internally to the procedure


; Here for the first time we build computational object with internal
; state. We return procedure that takes amount to be withdrawn.
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; We can do it as well a bit differently, by making so called withdrawal
; processors. The difference is that here we provide balance externally
; so we can create lot of processing objects.

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

; W1 and W2 are completelly independent objects.
