; Modeling with mutable data
; 
; Until now we worked only with non-mutable cons, car and cdr
; procedures.
; But, we can see this a bit differently. Cons can be defined as two
; mutations that set car and cdr

(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))

; where (get-new-pair) is the constructor of the empty pair

; getting the last pair of the list
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


