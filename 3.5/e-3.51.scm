(load "../helpers.scm")
(load "3.5.scm")

; Exercise 3.51.  In order to take a closer look at delayed
; evaluation, we will use the following procedure, which simply
; returns its argument after printing it:

(define (show x)
  (output x)
  x)

; What does the interpreter print in response to evaluating each
; expression in the following sequence?

; (define x (stream-map show (stream-enumerate-interval 0 10)))
; (stream-ref x 5)
; (stream-ref x 7)
;------------------------------------------------------------

; First about the definition of the exercise. Here we have a procedure
; (show x) which, when executed, will display the value of x and then return the value.
; It is useful in this case because it will tell us when certain element of the stream is
; evaluated.
;
; This approach of mixing the presentation code with the data structure is extremely bad
; example of mixing concerns and should not be regular approach in development. For this
; demonstrational purpose it serves its local purpose and nothing more.

; this one will evaluate only the first element of the stream
(output "(define ...")
(define x (stream-map show (stream-enumerate-interval 0 10)))

; This one will evaluate up to fifth element
(output "(stream-ref x 5)")
(stream-ref x 5)

; This one will evaluate up to seventh, but since first 5 are already
; evaluated, then we will not evaluate them again, so we evaluate only two more.
(output "(stream-ref x 7)")
(stream-ref x 7)
