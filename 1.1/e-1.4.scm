; Exercise 1.4 Operators can be compound expressions. Based on the value of parameter b operator is determined.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
