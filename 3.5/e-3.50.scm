; Exercise 3.50.  Complete the following definition, which generalizes
; stream-map to allow procedures that take multiple arguments, analogous to map
; in section 2.2.3, footnote 12.

; (define (stream-map proc . argstreams)
;  (if (<??> (car argstreams))
;      the-empty-stream
;      (<??>
;       (apply proc (map <??> argstreams))
;       (apply stream-map
;              (cons proc (map <??> argstreams))))))
; ------------------------------------------------------------

(load "../helpers.scm")
(load "../common.scm")
(load "3.5.scm")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define mapped (stream-map (lambda (x y) (+ x y)) (stream-enumerate-interval 1 10) (stream-enumerate-interval 2 20)))
(output (stream-car (stream-cdr mapped)))
