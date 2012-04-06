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

; Making a serializer.
; Serializer is like the factory to make more concrete type of the
; procedure which is passed.
; We pass certain procedure, and get back more restricted type which is
; serialized procedure.
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'aquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p))))

; NOTE:
; Here we can see one of the most standard uses of the higher-order
; functions. We pass one procedure to another one which produce new
; procedure which is combined with certain additional operations around
; the passed one.

; Making a mutex object.
; It is an object with internal state and two mutators It can be
; acquired if not already, and released if already acquired.

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'aquire)
             (if (test-and-set! cell)
               (the-mutex 'aquire))) ; try again until you aquire it
            ((eq? m 'release) (clear! cell)))
      the-mutex)))
(define (clear! cell)
  (set-car! cell false))

; following function is the brain of the mutex.
; A bit more magical than I would expect since it has kind of reversed
; logic to return false if it aquired it.
(define (test-and-set! cell)
  (if (car cell)
    true
    (begin (set-car! cell true)
           false)))

; One thing to notice here is that (test-and-set!) procedure is by
; default not atomic and can be as well source of concurrency bugs :)
