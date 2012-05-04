; Exercise 3.82.
;
; Redo exercise 3.5 on Monte Carlo integration in terms of
; streams. The stream version of estimate-integral will not have an argument
; telling how many trials to perform. Instead, it will produce a stream of
; estimates based on successively more trials.
; ------------------------------------------------------------

(load "../helpers.scm")
(load "3.5.scm")

; here I can define the stream version of random in range with the help of the
; previously defined procedure
(define (random-in-range-stream low high)
  (let ((range (- high low)))
    (define (random-in-range low high)
      (+ low (/ (random (inexact->exact (* range 100))) 100))) ; with a bit improved resolution
  (cons-stream
    (random-in-range low high)
    (random-in-range-stream low high))))

; some predicates for the circles
(define (unit-predicate-3-3 x y) (<= (+ (* (- x 3) (- x 3)) (* (- y 3) (- y 3))) 1))
(define (central-unit-circle x y) (<= (+ (* x x) (* y y)) 1))

; generic monte-carlo procedure defined in the text book.
; This one is pretty cool since it can work with streams
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))


(define (estimate-integral predicate x1 x2 y1 y2)
  (define rectangle-area (* (- x2 x1) (- y2 y1)))
  (define experiment-stream
    (stream-map predicate
                (random-in-range-stream x1 x2)
                (random-in-range-stream y1 y2)))
  (stream-map (lambda (success-percentage) (* success-percentage rectangle-area)) (monte-carlo experiment-stream 0 0)))

; (display-stream-head (estimate-integral unit-predicate-3-3 0 10 0 10) 10000)

;this converges towards value of pi, not very fast though due to inprecisions
(display-stream-head (estimate-integral central-unit-circle -1.0 1.0 -1.0 1.0) 10000)
