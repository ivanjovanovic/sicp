; Exercise 3.10.  In the make-withdraw procedure, the local variable
; balance is created as a parameter of make-withdraw. We could also
; create the local state variable explicitly, using let, as follows:

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; Recall from section 1.3.2 that let is simply syntactic sugar for a
; procedure call:

(let ((<var> <exp>)) <body>)

; is interpreted as an alternate syntax for

((lambda (<var>) <body>) <exp>)

; Use the environment model to analyze this alternate version of
; make-withdraw, drawing figures like the ones above to illustrate the
; interactions

(define W1 (make-withdraw 100))

(W1 50)

(define W2 (make-withdraw 100))

; Show that the two versions of make-withdraw create objects with the
; same behavior. How do the environment structures differ for the two
; versions?
; ------------------------------------------------------------

; Point here is that (let) is only the sytactic sugar for the (lambda)
; which is the only way to create procedure. Let, in this case defines
; new procedure and immediatelly executes it so we'll have different
; structure of the environments with oe more intermediate for the
; overall execution, but result will be the same.
;
;
