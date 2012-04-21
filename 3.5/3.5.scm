; using simple lists as standard interface between operations can be very inefficient.
; One example can explain this.

(load "../1.2/1.2.scm")
(load "../helpers.scm")
(load "../common.scm")

; iterative way to sum the primes
(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ accum count)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

; (output (sum-primes 10 20))

; Now if we write the same in the stream processing style
(define (sum-primes a b)
  (accumulate +
             0
             (filter prime?
                     (enumerate-interval a b))))


; (output (sum-primes 10 20))

; This approach looks much better abstracted and modularized,
; but has one problem. (enumerate-interval) will first produce
; all the elements of the interval and only ten filtering is going
; to produce next list of fitlered values and only then it will
; be passed to accumulate for folding. This means lot of memory usage
; for long lists.

; (output (sum-primes 1 1000000)) ; This will take some time and space

; Other part of inefficiency is, what if we want only one element out
; of all of them. Then we don't need to construct the whole enumerated list.
; (car (cdr (filter prime? (enumerate-interval 1000 1000000))))

; In orer to solve these problems, we can introduce streams.
; They are similar as lists, and they share with them a lot of properties, and
; as well adding some new ones.
;
; They are described by the following abstraction.
; (stream-car (cons-stream x y)) = x
; (stream-cdr (const-stream x y)) = y
; the-empty-stream object
; and the constructor (const-stream x y)

; we have to define cons-stream as macro.
; This is because if we do (cons-stream a b) as a procedure
; call, then expression b will be evaluated before application of
; the procedure (cons-stream)
; This way, as a macro, it is first syntactically transformed to
; delayed version and then executed.
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

; Stream operations analog to lists
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream
      (proc (stream-car s))
      (stream-map proc (stream-cdr s)))))

(define (stream-for-each s proc)
  (if (stream-null? s)
    'done
    (begin
      (proc (stream-car s))
      (stream-for-each (stream-cdr s) proc))))

(define (display-stream s)
  (stream-for-each s output)) ; output defined in helpers.scm


; now we have procedures to manipualate streams, but we still don't have stream
; implementation.
;
; We will implement streams with the help of (delay) function where.
; (cons-stream a b) is equal to
; (cons a (delay b))
;
; This means, that (cdr) of the stream will not be a concrete value but it will be
; the promise to calculate it when we need it, and not before that
; we are still missing the parts (delay) and (force) to implement
; streams fully

; (define (force delayed-object)
;   (delayed-object))

; there is one optimization we can do in order not to evaluate same delayed stream
; more than once. We make small closure with the internal status variable and the
; result of computation

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
        (begin
          (set! result (proc))
          (set! already-run? true)
          result)
        result))))

; (define (delay <exp>)
;   (memo-proc (lambda () <exp>)))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
; to finalize the definition

(define the-empty-stream '())
(define (stream-null? stream) (null? stream))

; delay and force need to be special constructs implemented
; in the lower level of language because they don't bahave
; as ordinary procedures, so we take them as defined

; Given these primitives we can write our inefficient example

; (stream-car
;   (stream-cdr
;     (stream-filter prime?
;                    (stream-enumerate-interval 10000 1000000))))

; we need some more things to implement

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream low
                 (stream-enumerate-interval (+ low 1) high))))

; so this looks like this

; (cons 10000
;       (delay (stream-enumerate-interval 10001 1000000)))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
