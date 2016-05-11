; excercise 1.3
; Define procedure that takes three numbers as arguments and returns
; the sum of the squares of the two larger numbers
(define (sum-larger-square a b c)
  (cond ((and (< a b) (< a c)) (+ (* b b) (* c c)))
        ((and (< b a) (< b c)) (+ (* a a) (* c c)))
        (else (+ (* a a) (* b b)))))
