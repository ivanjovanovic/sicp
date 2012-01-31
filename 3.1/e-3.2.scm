; Exercise 3.2.  In software-testing applications, it is useful to be
; able to count the number of times a given procedure is called during
; the course of a computation. Write a procedure make-monitored that
; takes as input a procedure, f, that itself takes one input. The result
; returned by make-monitored is a third procedure, say mf, that keeps
; track of the number of times it has been called by maintaining an
; internal counter. If the input to mf is the special symbol
; how-many-calls?, then mf returns the value of the counter. If the
; input is the special symbol reset-count, then mf resets the counter to
; zero. For any other input, mf returns the result of calling f on that
; input and increments the counter. For instance, we could make a
; monitored version of the sqrt procedure:


; (define s (make-monitored sqrt))

; (s 100) => 10

; (s 'how-many-calls?) => 1
; ------------------------------------------------------------
(load "../helpers.scm")

; just wrapping the procedure with the counter inside
; constructor
(define (make-monitored f)
  (let ((count 0))
    (lambda (arg)
      (set! count (+ count 1))
      (f arg))))

(define s (make-monitored sqrt))
(output (s 100))

; adding selectors on local state
(define (make-monitored f)
  (let ((count 0))
    ; message dispatcher procedure
    (lambda (m)
      (cond ((equal? m 'how-many-calls?) count)
            ((eq? m 'reset-count) (set! count 0))
            (else 
              (begin
                (set! count (+ count 1))
                (f m)))))))

(define s (make-monitored sqrt))
(output (s 100))
(output (s 100))
(output (s 'how-many-calls?)) ; 2
(output (s 100))
(output (s 100))
(output (s 'how-many-calls?)) ; 4
(s 'reset-count) ; resets to 0
(output (s 100))
(output (s 100))
(output (s 100))
(output (s 100))
(output (s 'how-many-calls?)) ; 4
