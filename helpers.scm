; helper DSL to make implementation and representation of exercises more
; enjoyable

(define (output . to-print)
  (map (lambda (x) (display x) (newline)) to-print))

