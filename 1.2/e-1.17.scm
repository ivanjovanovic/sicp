; Exercise 1.17.
;
; The exponentiation algorithms in this section are based on
; performing exponentiation by means of repeated multiplication.
; In a similar way, one can perform integer multiplication by means of repeated addition. 
; The following multiplication procedure (in which it is assumed that our language can only add, not multiply) 
; is analogous to the expt procedure:

; (define (* a b)
;   (if (= b 0)
;       0
;       (+ a (* a (- b 1)))))

; This algorithm takes a number of steps that is linear in b.
; Now suppose we include, together with addition, operations double,
; which doubles an integer, and halve, which divides an (even) integer by 2.
; Using these, design a multiplication procedure analogous to
; fast-expt that uses a logarithmic number of steps.

; --------------------------------
;  First we define needed auxillary procedures

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))


; Recursive procedure which halves the problem size in every new call have
; logarithmic order of growth

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

(display (fast-mult 220000 38000003))
(newline)


