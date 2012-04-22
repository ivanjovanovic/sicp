(load "../helpers.scm")

; Throughout this chapter we have developed api for manipulating
; streams. Here is list of procedures that we used.

; basic definition of the stream as abstract data type
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define the-empty-stream '())
(define (stream-null? stream) (null? stream))


; Stream operations analog to lists
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

; single stream map
(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream
      (proc (stream-car s))
      (stream-map proc (stream-cdr s)))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (stream-for-each s proc)
  (if (stream-null? s)
    'done
    (begin
      (proc (stream-car s))
      (stream-for-each (stream-cdr s) proc))))


(define (add-streams s1 s2)
  (stream-map + s1 s2))

; for this we have to define division of the streams
(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (negate-series series)
  (stream-map (lambda (x) (- x))
              series))

; zipper
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

; stream presentation helpers
(define (display-stream s)
  (stream-for-each s output))

(define (display-stream-head stream n)
  (if (= n 1) (output (stream-car stream))
    (begin
      (output (stream-car stream))
      (display-stream-head (stream-cdr stream) (- n 1)))))


; some characteristic and useful streams
(define ones (cons-stream 1 ones)) ; ones
(define integers (cons-stream 1 (add-streams ones integers))) ; integers
(define double (cons-stream 1 (scale-stream double 2))) ; powers of 2
