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
