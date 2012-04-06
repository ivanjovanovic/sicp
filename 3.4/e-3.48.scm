; Exercise 3.48.
;
; Explain in detail why the deadlock-avoidance method
; described above, (i.e., the accounts are numbered, and each process
; attempts to acquire the smaller-numbered account first) avoids
; deadlock in the exchange problem. Rewrite serialized-exchange to
; incorporate this idea. (You will also need to modify make-account so
; that each account is created with a number, which can be accessed by
; sending an appropriate message.)
; ------------------------------------------------------------

; If we order the access to the resources, then is not possible that one
; process locks resources which are needed in the further steps of
; processing for the other process, they will wait in the queue to access
; next resource needed to proceded with computation. That way, every
; process can fulfill the whole computation.

(define (make-account-and-serializer balance number)
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
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'number) number)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

; ordering serializers based on the account number
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (account1 'number) (account2 'number))
      ((serializer2 (serializer1 exchange))
        account1 account2)
      ((serializer1 (serializer2 exchange))
        account1 account2))))
