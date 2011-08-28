; Exercise 1.25.
;
; Alyssa P. Hacker complains that we went to a lot of extra work in writing
; expmod. After all, she says, since we already know how to compute
; exponentials, we could have simply written
;
; (define (expmod base exp m)
;   (remainder (fast-expt base exp) m))
;
; Is she correct? Would this procedure serve as well for our fast prime tester?
; Explain.
; --------------------------------
;
; To recall to the expmod which is already defined.
;

(load "common.scm")
(load "1.2.scm")


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(display (expmod 123 33 12))
(newline)


; definition from the examples reusing fast-expt from 1.2.scm
(define (expmod2 base exp m)
  (remainder (fast-expt base exp) m))

(display (expmod2 123 33 12))
(newline)

; Both algorithms work correctly, but there is a difference in the way they are
; conducted with implications on amount of memory and calcuations needed to
; execute.
;
; The one that is based on fast-expt first needs to calculate full result of
; exponentiation and then find modulo of it. This is not always optimal since
; result can get very big containing thousands of digits in a number and thus
; requiring more effort for basic operations on these numbers.
;
; Other algorithm is continuously doing squared exponentiation and calculation
; of modulo and thus keeps involved numbers to low higher threshold involving
; less memory for storing them and less number of steps needed to do operations
; with hughe numbers which do not fit into one 16/32/64 bit regirsters. This
; approach is called "Right-to-Left binary method" because it is based on sqared
; exponentiation known as "binary exponentioation".
;
; @see http://en.wikipedia.org/wiki/Modular_exponentiation
