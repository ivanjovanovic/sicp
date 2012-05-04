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
(load "3.5.scm")
(load "../3.1/e-3.6.scm")

; first initializer
(define random-init
  (inexact->exact (current-milliseconds)))

; lets say random update is this one
(define random-update
  (let ((m 19)
        (a 3)
        (c 5))
    (lambda (x)
      (modulo (+ (* a x) c) m))))


; stream of random numbers is defined
(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

; what we want to achieve is to have stream of random numbers which has
; argument the other stream in which is placed whether to generate new
; one or to start from the new value

; directed-pseudo-random-stream
(define (dp-random-stream directions)
  (cons-stream
    random-init
    (stream-map
      (lambda (number direction)
        (if (eq? direction 'generate)
          (random-update number)
          direction))
      (dp-random-stream directions) ; self-reference to the stream
      directions)))

; test stream directions
(define directions-stream
  (cons-stream 'generate
               (cons-stream 'generate
                            (cons-stream 0
                                         (cons-stream 'generate
                                                      (cons-stream 'generate
                                                                   (cons-stream 'generate
                                                                                (cons-stream 'generate directions-stream))))))))
(define test (dp-random-stream directions-stream))
(display-stream-head test 20); with randomly inited first value -> 959 13 6 0 5 1 8 10 16 15 0 5 1 8 10 16 15 0 5 1

