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

; In Sceme, we can implement serializers in order to controll concurrent execution.
;
; ; Lets say we have
; (parallel-execute <p1> <p2> <p3> ... <pk>)
;
; to execute in parallel p1, p2, ...

; In order to serialize execution we can build serializer

(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))


; knowing the approach of protecting procedures with serializers we can
; apply it to our account computational object procedures
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
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

; Serialization of the access to the resource becomes bigger challenge
; when we have to protect multiple resources.

; Consider the case where we have procedure to exchange balances of two
; accounts

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

; Here, lot of things can go wrong.
; One of balances can be updated in the middle of procedure and then we
; get invalid values in he accounts.

; One way to solve this is to synchronize the whole exchange procedure.

; If we define new account computational object which exposes serializer
; through the dispather procedure

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
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

; We can use it like this

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

; And serialize the whole exchange procedure as ordered execution
; of two serialized procedures

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

; Implementation of serializers is not very complicated. It is simple
; function wrapper with one synchronization primitive. We use mutex for
; that. mutex stands for mutual exclusion flag.


