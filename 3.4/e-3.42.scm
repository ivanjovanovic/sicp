; Exercise 3.42.  Ben Bitdiddle suggests that it's a waste of time to create a
; new serialized procedure in response to every withdraw and deposit message.
; He says that make-account could be changed so that the calls to protected are
; done outside the dispatch procedure. That is, an account would return the
; same serialized procedure (which was created at the same time as the account)
; each time it is asked for a withdrawal procedure.

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance) balance)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      dispatch)))

; Is this a safe change to make? In particular, is there any difference in what
; concurrency is allowed by these two versions of make-account ?
; ------------------------------------------------------------

; Difference is that in the previous case we had for every call
; ((account 'withdraw) 50) new protected procedure to execute
;
; In the case of this example we have always the same procedure execution.
; Both of these are local to this account object and they are safe to be
; this way since they will anyway share same synchronization primitive I guess.
