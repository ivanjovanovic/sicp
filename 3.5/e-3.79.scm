; Exercise 3.79.
;
; Generalize the solve-2nd procedure of exercise 3.78 so
; that it can be used to solve general second-order differential
; equations d2 y/dt2 = f(dy/dt, y).
; ------------------------------------------------------------

; to generalize solution for the second order differential equation
; we have to make the part that produces ddy to be some function and
; not the concrete sum we had in previous exercise. In the exercise is
; given function of two params, dy/dt and y so we use generic map
; function to map the function accross two streams

(define (solve-2nd dt y0 dy0 f)
  (define y   (integral (delay dy) y0 dt))
  (define dy  (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y)) ; uses generic map zipper
  y)
