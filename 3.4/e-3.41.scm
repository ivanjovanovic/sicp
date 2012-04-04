; Exercise 3.41.  Ben Bitdiddle worries that it would be better to implement
; the bank account as follows (where the commented line has been changed):

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
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             ((protected (lambda () balance)))) ; serialized
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

; because allowing unserialized access to the bank balance can result in
; anomalous behavior. Do you agree? Is there any scenario that demonstrates
; Ben's concern?
; ------------------------------------------------------------

; What we have here is that retrieving of the balance is serialized.
;
; In this particular case it is not possible that, even if reading balance is non-atomic,
; to have it interleaved with the write from other parallel process since withdraw and deposit
; are both protected.

;
;
; Overall reading of the variables should not be considered atomic and there should be special care
; about that in concurrent program execution.
;
; If retrieving balance is not atomic and needs multiple steps in the layer in which
; parallelism is implemented then we need it. For example, if we go deep into the way machines work
; and consider balance to be made of some more complex structure than just a small number (number of the
; size of byte or properly aligned 16 and 32 bit numebrs).Maybe retrieving it
; takes several operations, and if it is interrupted in the middle of retrieval and memory content changed, we might get
; wrong value at the end because memory locations are changed meanwhile.
;
; Interesting reads:
; http://stackoverflow.com/questions/5002046/atomicity-in-c-myth-or-reality
; http://sergworks.wordpress.com/2010/08/28/understanding-threads-atomic-operations-and-memory-ordering/
