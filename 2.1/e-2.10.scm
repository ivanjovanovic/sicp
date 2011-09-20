; Exercise 2.10.
;
; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and 
; comments that it is not clear what it means to divide by an interval that spans zero.
; Modify Alyssa's code to check for this condition and to signal an error if it occurs.
; ----------------------------------------------

; original method of division is

(load "2.1.scm")
(load "e-2.7.scm")

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (lower-bound y))
                   (/ 1.0 (upper-bound y)))))

; We can introduce check if interval spans zero by checking if
; product of lower and upper bound is negative, which would mean that they
; are from the other sides of the 0 value

(define (div-interval x y)
  (if (<  (* (lower-bound y)
            (upper-bound y))
          0)
    (error "y is spanning zero")
    (mul-interval
      x
      (make-interval (/ 1.0 (lower-bound y))
                     (/ 1.0 (upper-bound y))))))

; can check this with the test

(div-interval (make-interval 1 2) (make-interval -1 -2)) ; should be OK
(div-interval (make-interval 1 2) (make-interval -1 2)) ; should throw an error, and it throws it!!! yay
