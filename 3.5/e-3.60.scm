; Exercise 3.60.
;
; With power series represented as streams of
; coefficients as in exercise 3.59, adding series is implemented by
; add-streams. Complete the definition of the following procedure for
; multiplying series:

; (define (mul-series s1 s2)
;   (cons-stream <??> (add-streams <??> <??>)))

; You can test your procedure by verifying that sin^2(x) + cos^2(x) = 1,
; using the series from exercise 3.59.
; ------------------------------------------------------------

(load "../helpers.scm")
(load "3.5.scm")
(load "e-3.59.scm")

; Since series is defined by the list of coefficients, mutiplying them
; is multiplying two multi coefficient sums.
; (a0 a1 a2 a3 ...) and (b0 b1 b2 b3 ...)
;
; If we represent it in the real sum form
; (a0 + a1*x + a2*x^2 + a3*x^3 + ...) * (b0 + b1*x + b2*x^2 + b3*x^3 + ...)
; that gives
; a0*b0 + a0*(b1*x + b2*x^2 + b3*x^3 + ...) + b0*(a1*x + a2*x^2 + a3*x^3 + ...) + ...
;
; more intuitive, if we define series as
; s1 = (a0 + Arest) and s2 = (b0 + Brest) then
; s1 * s2 = a0*b0 + a0*Brest + b0*Arest + Arest*Brest ...
;
; This is written as
(define (mul-series s1 s2)
  (cons-stream
    (* (stream-car s1) (stream-car s2))
    (add-streams
      (add-streams (scale-stream (stream-cdr s1)
                                 (stream-car s2))
                   (scale-stream (stream-cdr s2)
                                 (stream-car s1)))
      (cons-stream 0 (mul-series (stream-cdr s1)
                                 (stream-cdr s2))))))

(define circle (add-streams (mul-series sin-series sin-series)
                            (mul-series cos-series cos-series)))

; (display-stream-head circle 10) ; 1 0 0 0 ...
