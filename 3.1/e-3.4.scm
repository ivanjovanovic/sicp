; Exercise 3.4.  Modify the make-account procedure of exercise 3.3 by
; adding another local state variable so that, if an account is accessed
; more than seven consecutive times with an incorrect password, it
; invokes the procedure call-the-cops.
; ------------------------------------------------------------

(load "../helpers.scm")

(define (make-account balance account-password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ; action dispatcher, protected by password
  (let ((attempts 0))
    (lambda (password m)
      (if (eq? password account-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        ; just return complaint, but has to accept argument
        (lambda (n) 
          (set! attempts (+ attempts 1))
          (if (>= attempts 7)
            "Lizard detected. Calling cops"
            "Incorrect password"))))))

(define acc (make-account 100 'wizards!lizards))
(output ((acc 'wizards!lizards 'withdraw) 40))
(output ((acc 'lizards!wizards 'withdraw) 40))
(output ((acc 'lizards!wizards 'withdraw) 40))
(output ((acc 'lizards!wizards 'withdraw) 40))
(output ((acc 'lizards!wizards 'withdraw) 40))
(output ((acc 'lizards!wizards 'withdraw) 40))
(output ((acc 'lizards!wizards 'withdraw) 40))
(output ((acc 'lizards!wizards 'withdraw) 40))
