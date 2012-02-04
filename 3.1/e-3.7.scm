; Exercise 3.7.
;
; Consider the bank account objects created by make-account,
; with the password modification described in exercise 3.3. Suppose that our
; banking system requires the ability to make joint accounts. Define a
; procedure make-joint that accomplishes this. Make-joint should take three
; arguments. The first is a password-protected account. The second argument
; must match the password with which the account was defined in order for the
; make-joint operation to proceed. The third argument is a new password.
; Make-joint is to create an additional access to the original account using
; the new password. For example, if peter-acc is a bank account with password
; open-sesame, then

; (define paul-acc
;   (make-joint peter-acc 'open-sesame 'rosebud))

; will allow one to make transactions on peter-acc using the name paul-acc and
; the password rosebud. You may wish to modify your solution to exercise 3.3 to
; accommodate this new feature.
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

(define (make-joint account new-pass old-pass)
  (lambda (password m)
    (if (eq? password new-pass)
      (account old-pass m) ; just delegate call to the account with old-pass stored in environment
      "Incorrect password")))

(define peter-acc (make-account 100 'wizard!lizard))
(output ((peter-acc 'wizard!lizard 'deposit) 20)) ; 140

; Now creating join account and doing the deposit on top of it
(define paul-acc (make-joint peter-acc 'open-sesame 'wizard!lizard))
(output ((paul-acc 'open-sesame 'deposit) 20)) ; => 140

