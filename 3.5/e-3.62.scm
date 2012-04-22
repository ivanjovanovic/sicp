; Exercise 3.62.
;
; Use the results of exercises 3.60 and 3.61 to define a
; procedure div-series that divides two power series. Div-series should
; work for any two series, provided that the denominator series begins
; with a nonzero constant term. (If the denominator has a zero constant
; term, then div-series should signal an error.) Show how to use
; div-series together with the result of exercise 3.59 to generate the
; power series for tangent.
; ------------------------------------------------------------

(load "../helpers.scm")
(load "3.5.scm")
(load "e-3.59.scm") ; sin-series, cos-series
(load "e-3.60.scm") ; mul-series
(load "e-3.61.scm") ; invert-unit-series

(define (div-series num-series den-series)
  (let ((den (stream-car den-series)))
    (if (= den 0)
        (error "Division by zero is not allowed")
        (scale-stream
         (mul-series
           num-series
           (invert-unit-series (scale-stream den-series (/ 1 den))))
         (/ 1 den)))))

(define tan-series (div-series sin-series cos-series))

(display-stream-head tan-series 10)
; The result of reading first 10 coefficients of tangent
;
; 0 1 0.0 0.333333333333333 0.0 0.133333333333333 0.0 0.053968253968254 0.0 0.0218694885361552
;
; which is correct for expanded Taylor series for tangent trig function
; http://en.wikipedia.org/wiki/Tangent_(trigonometric_function)



