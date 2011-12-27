; Exercise 2.54.
;
; Two lists are said to be equal? if they contain equal elements
; arranged in the same order. For example,

; (equal? '(this is a list) '(this is a list))

; is true, but

; (equal? '(this is a list) '(this (is a) list))

; is false. To be more precise, we can define equal? recursively in
; terms of the basic eq? equality of symbols by saying that a and b are
; equal? if they are both symbols and the symbols are eq?, or if they
; are both lists such that (car a) is equal? to (car b) and (cdr a) is
; equal? to (cdr b). Using this idea, implement equal? as a procedure.
; ------------------------------------------------------------

(load "../common.scm")
(load "../helpers.scm")
(load "2.3.scm")

(define (equal? first second)
  (cond ((and (null? first) (null? second)) true)
        ((and (not (and (symbol? (car first))
                       (symbol? (car second))
                       (eq? (car first) (car second))))
             (not (and (list? (car first))
                       (list? (car second))
                       (equal? (car first) (car second)))))
         false)
        (else (equal? (cdr first) (cdr second)))))

(output (equal? '(this is (test (test)) list) '(this is (test (test)) list)))
