; Exercise 3.76.
;
; Eva Lu Ator has a criticism of Louis's approach in
; exercise 3.75. The program he wrote is not modular, because it
; intermixes the operation of smoothing with the zero-crossing
; extraction. For example, the extractor should not have to be changed
; if Alyssa finds a better way to condition her input signal. Help Louis
; by writing a procedure smooth that takes a stream as input and
; produces a stream in which each element is the average of two
; successive input stream elements. Then use smooth as a component to
; implement the zero-crossing detector in a more modular style.
; ------------------------------------------------------------

; The program in 3.75 was implemented more in the structured programming
; style where in one iterative process we put lot of calculations that
; depend on the current value of certain variable.
;
; We can chain stream with stream operations that we already have
(load "../helpers.scm")
(load "3.5.scm")

(define sensor-stream
  (stream-map (lambda (x) (- (random 10) 5))
              ones))

(define (average x y)
  (/ (+ x y) 2))

(define (average-stream input-stream)
  (stream-map average input-stream (stream-cdr input-stream)))

(define (make-zero-crossings input-stream)
  (stream-map sign-change-detector input-stream (stream-cdr input-stream)))

(define zero-crossings
  (make-zero-crossings (average-stream sensor-stream)))
