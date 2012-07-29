; helper procedure to output formatted object code
(define (format-object-code object-code)
  (if (not (null? object-code))
    (let ((instruction (car object-code)))
      (if (symbol? instruction)
        (begin
          (output instruction) ; label
          (format-object-code (cdr object-code)))
        (begin
          (display "  ") (output instruction) ; indented
          (format-object-code (cdr object-code)))))
    'done))

(define (instruction-list object-code) (caddr object-code))
