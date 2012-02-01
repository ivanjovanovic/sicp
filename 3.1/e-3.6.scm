; Exercise 3.6.
;
; It is useful to be able to reset a random-number generator to
; produce a sequence starting from a given value. Design a new rand procedure
; that is called with an argument that is either the symbol generate or the
; symbol reset and behaves as follows: (rand 'generate) produces a new random
; number; ((rand 'reset) <new-value>) resets the internal state variable to the
; designated <new-value>. Thus, by resetting the state, one can generate
; repeatable sequences. These are very handy to have when testing and debugging
; programs that use random numbers.
; ------------------------------------------------------------

; This is not that difficult ot do when we know the technique of message passing.
; Rand is not object that contains local state and two actions 'generate and 'reset

(load "../helpers.scm")

; practically we have to define our random number generator.

; first initializer
(define random-init
  (inexact->exact (current-milliseconds)))

; and how we update random number every time randomization is called.
; @see http://en.wikipedia.org/wiki/Random_number_generation
;
; I will use http://en.wikipedia.org/wiki/Linear_congruential_generator with some predefined constants
; just for sake of the simplicity
;
; m = 19 (primary number)
; a = 3
; c = 5

; Probably incorrect but for simplicity just to explain what it means to build random sequence generator
; this should be sufficient. For more information read a bit more on the topic of random/pseudo
; random number generation
;
; maximum number this generator can produce is m (which we have to define significantly high for big seqs)
(define random-update
  (let ((m 19)
        (a 3)
        (c 5))
    (lambda (x)
      (modulo (+ (* a x) c) m))))

; and randomization procedure to expose
; message handler action
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

; this is initialized by random-init
(output (rand 'generate))
(output (rand 'generate))
(output (rand 'generate))
(output (rand 'generate))
(output (rand 'generate))
(output (rand 'generate))

; here we set the seed on our own
(output "reseting")
((rand 'reset) 10)
(output (rand 'generate))
(output (rand 'generate))
(output (rand 'generate))
(output (rand 'generate))
(output (rand 'generate))
(output (rand 'generate))

; here we reset the sequence
(output "reseting")
((rand 'reset) 10)
(output (rand 'generate))
(output (rand 'generate))
(output (rand 'generate))
(output (rand 'generate))
(output (rand 'generate))
(output (rand 'generate))
