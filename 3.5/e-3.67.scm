; Exercise 3.67.
;
; Modify the pairs procedure so that (pairs integers
; integers) will produce the stream of all pairs of integers (i,j)
; (without the condition i < j). Hint: You will need to mix in an
; additional stream.
; ------------------------------------------------------------

(load "../helpers.scm")
(load "3.5.scm")

; current implementation of the mentioned procedure
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define int-pairs (pairs integers integers))




; Our implementation of all pairs can map over the stream of pairs to
; make just the inversion of the elements if they are different or to
; emit null element if same

(define (swap-pair pair) ; represented as list of two elements, not cons
  (if (= (car pair) (cadr pair))
    '() ; if they are same don't emit one more like that
    (list (cadr pair) (car pair))))

(define all-pairs
  (interleave
    int-pairs
    (stream-filter (lambda (x) (not (null? x))) (stream-map swap-pair int-pairs))))

; small test to see if this works
; (display-stream-head all-pairs 20) ; it works
