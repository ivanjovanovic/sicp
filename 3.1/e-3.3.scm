; Exercise 3.3.  Modify the make-account procedure so that it creates
; password-protected accounts. That is, make-account should take a
; symbol as an additional argument, as in

; (define acc (make-account 100 'secret-password))

; The resulting account object should process a request only if it is
; accompanied by the password with which the account was created, and
; should otherwise return a complaint:

; ((acc 'secret-password 'withdraw) 40) ; 60

; ((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
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
  (lambda (password m)
    (if (eq? password account-password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT" m)))
      ; just return complaint, but has to accept argument
      (lambda (n) "Incorrect password"))))


(define acc (make-account 100 'wizards!lizards))
(output ((acc 'wizards!lizards 'withdraw) 40))
(output ((acc 'lizards!wizards 'withdraw) 40))
