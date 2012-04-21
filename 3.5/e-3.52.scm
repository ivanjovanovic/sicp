(load "../helpers.scm")
(load "../common.scm")
(load "3.5.scm")

; Exercise 3.52.  Consider the sequence of expressions

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
; (display-stream z)

; What is the value of sum after each of the above expressions is evaluated?
; What is the printed response to evaluating the stream-ref and display-stream
; expressions? Would these responses differ if we had implemented (delay <exp>)
; simply as (lambda () <exp>) without using the optimization provided by
; memo-proc ? Explain.
; ------------------------------------------------------------
(define sum 0)

; here we define sequence that is potentially expandable to
; 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(output sum) ; => 1 (first element of the sequence was evaluated)

; here we define sequence expandable to
; 6 10 28 36 66 78 120 136 190 210
(define y (stream-filter even? seq))
; here we have filter that will look for the first element
; and then evaluate sums for 1 2 3 and produce 6
;
(output sum) ; => 6 (up to first element of sequence is evaluated)

; here we define sequence of the form
; 10 15 45 55 105 120 190 210
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(output sum) ; => 10 (up to first element of sequence evaluated)
(stream-ref y 7)
(output sum) ; =>  136 (up to 8th element evaluated because of zero base indexing in stream-ref)


(display-stream z) ; => 10 15 45 55 105 120 190 210



; These results would differ significantly if there was no memo implementation
; since procedure (accum) would be executed every time we need to
; get next element of the sequence and sum would turn out different.
