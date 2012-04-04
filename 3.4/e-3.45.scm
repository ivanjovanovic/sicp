; Exercise 3.45.  Louis Reasoner thinks our bank-account system is
; unnecessarily complex and error-prone now that deposits and
; withdrawals aren't automatically serialized. He suggests that
; make-account-and-serializer should have exported the serializer (for
; use by such procedures as serialized-exchange) in addition to (rather
; than instead of) using it to serialize accounts and deposits as
; make-account did. He proposes to redefine accounts as follows:

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

; Then deposits are handled as with the original make-account:

(define (deposit account amount)
 ((account 'deposit) amount))

; Explain what is wrong with Louis's reasoning. In particular, consider
; what happens when serialized-exchange is called.
; ------------------------------------------------------------

; Each serializer has one locking primitive (e.g. mutex) which is global
; for all procedures produced by that serializer. Here we will execute
; serializer1 which will lock mutex of account1 and then serializer2
; which will lock mutex of account2. Since accout1 and account2
; withdrawal and deposit procedures are depending on same lock they will
; never run since it is already locked. It will stand still then.
