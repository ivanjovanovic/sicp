; Exercise 2.75.  Implement the constructor make-from-mag-ang in
; message-passing style. This procedure should be analogous to the
; make-from-real-imag procedure given.
;
; (define (make-from-real-imag x y)
;   (define (dispatch op)
;    (cond ((eq? op 'real-part) x)
;          ((eq? op 'imag-part) y)
;          ((eq? op 'magnitude)
;           (sqrt (+ (square x) (square y))))
;          ((eq? op 'angle) (atan y x))
;          (else
;           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
;  dispatch)
; ------------------------------------------------------------

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
            (eror "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)


