; Throughout this chapter we have implemented small api for manipulating
; Taylor series. Here are the procedures comprising it.
(load "streams.scm")

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

(define (div-series num-series den-series)
  (let ((den (stream-car den-series)))
    (if (= den 0)
        (error "Division by zero is not allowed")
        (scale-stream
         (mul-series
           num-series
           (invert-unit-series (scale-stream den-series (/ 1 den))))
         (/ 1 den)))))

(define (negate-series series)
  (stream-map (lambda (x) (- x))
              series))

(define (integrate-series series)
  (mul-streams coefficients series))

(define (invert-unit-series stream)
  (cons-stream 1
               (negate-series (mul-series (stream-cdr stream)
                                          (invert-unit-series stream)))))

; some interesting series
(define cos-series (cons-stream 1 (negate-series (integrate-series sin-series))))
(define sin-series (cons-stream 0 (integrate-series cos-series)))
(define tan-series (div-series sin-series cos-series))
