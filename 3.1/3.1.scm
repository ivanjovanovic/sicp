(load "../helpers.scm")

; For the first time we introduce possibility that one procedure returns
; different result when called

(define balance 100)

(define (withdraw amount)
  (if (> balance amount)
    (begin
      (set! balance (- balance amount))
      balance) ; returns current state of balance
    "Insufficient funds"))

(output (withdraw 20)) ; 80
(output (withdraw 20)) ; 60
(output (withdraw 200)) ; "Insufficient funds"

; here we use (set! <name> <new-value>) to set new value of a variable

; Here balance is kind of exposed to the public and other procedures
; could influence it without really wanting to do that. We can hide it
; internally to the procedure


; Here for the first time we build computational object with internal
; state. We return procedure that takes amount to be withdrawn.
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; We can do it as well a bit differently, by making so called withdrawal
; processors. The difference is that here we provide balance externally
; so we can create lot of processing objects.

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

; W1 and W2 are completelly independent objects.

; We can make even more complex object that can have interna local state with multiple actions

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

; this can be used like this

(define acc (make-account 100))

; This way of calling an action on the object is called message passing style
(output ((acc 'withdraw) 50)) ; 50
(output ((acc 'withdraw) 70)) ; Insufficient 

; Benefits of introduction of assignment can be easily seen in one usage of the
; random number generator. That is (rand) procedure that intrinsicly has local state
; and without having it internally it would be needed to pass it always when we need it
; and maintain the state in parts of code that do not have to know anything of it.

; this is kind of explanation how internals work and why we need local state
; (define rand
;   (let ((x random-init))
;     (lambda ()
;       (set! x (random-update x))
;       x)))

; this works
(define rand
  (lambda () (random 10000)))

; Using this object in one monte carlo simulation gives more information about how this is 
; very independent from how rand works

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(output (estimate-pi 1000))

; if we would now use random that doesn't encapsulate value of x in a local state
; variable our implementation would look something like this

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)   
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))
