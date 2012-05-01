; Exercise 3.81.
;
; Exercise 3.6 discussed generalizing the random-number
; generator to allow one to reset the random-number sequence so as to
; produce repeatable sequences of ``random'' numbers. Produce a stream
; formulation of this same generator that operates on an input stream of
; requests to generate a new random number or to reset the sequence to a
; specified value and that produces the desired stream of random
; numbers. Don't use assignment in your solution.
; ------------------------------------------------------------

; in exercise 3.6 it is defined as a object with the internal state.

; lets say random update is this one
(define random-update
  (let ((m 19)
        (a 3)
        (c 5))
    (lambda (x)
      (modulo (+ (* a x) c) m))))

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (begin
               (set! x (random-update x))
               x))
            ((eq? m 'reset)
             (lambda (value)
               (set! x value)))))))

; stream of random numbers is defined
(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

; what we want to achieve is to have stream of random numbers which has
; argument the other stream in which is placed whether to generate new
; one or to start from the new value

; it can be used like this
(define random-stream (random-numbers actions))

(define (random-numbers actions)
  )
