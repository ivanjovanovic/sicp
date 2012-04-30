; Exercise 3.69.
;
; Write a procedure triples that takes three infinite
; streams, S, T, and U, and produces the stream of triples (Si,Tj,Uk)
; such that i <= j <= k. Use triples to generate the stream of all
; Pythagorean triples of positive integers, i.e., the triples (i,j,k)
; such that i <= j and i^2 + j^2 = k^2.
; ------------------------------------------------------------

(load "../helpers.scm")
(load "3.5.scm")

; We do this by putting one outer iteration around all pairs.
; So for
(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x)) (pairs t (stream-cdr u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

; (display-stream-head (triples integers integers integers) 100)

(define int-triplets (triples integers integers integers))
(define (pythagorean-filter triplet)
  (let ((i (car triplet))
        (j (cadr triplet))
        (k (caddr triplet)))
    (= (+ (square i) (square j)) (square k))))

(define pythagorian-triples (stream-filter pythagorean-filter int-triplets))

(display-stream-head pythagorian-triples 10)
; (3 4 5) (6 8 10) (5 12 13) ... it just takes some time because they
; are not that often in the sequence
