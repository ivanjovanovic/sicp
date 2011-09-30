; Exercise 2.22.
; Louis Reasoner tries to rewrite the first square-list procedure of exercise
; 2.21 so that it evolves an iterative process:

; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things) 
;               (cons (square (car things))
;                     answer))))
;   (iter items nil))

; Unfortunately, defining square-list this way produces the answer list in the reverse order of the one desired. Why?

; Louis then tries to fix his bug by interchanging the arguments to cons:

; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things)
;               (cons answer
;                     (square (car things))))))
;   (iter items nil))
; 
; This doesn't work either. Explain.
; ----------------------------------

(load "../helpers.scm")
(load "../common.scm")

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

(output (square-list (list 1 2 3 4))) ; -> (16 9 4 1)

; 1) Here we see that in calling the iterative procedure call we use
;    (cons (square (car things)) answer)
;    which adds on top of the result new value. Since we are `cdr-ing`
;    down the list new value is always bigger than the previous

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(output (square-list (list 1 2 3 4))) ; -> (16 9 4 1)

; 2) Second approach doesn't work as well since cons by definition
;    is to build lists when given the structure. 
;    (cons 1 (cons 2 (cons 3))) e.t.c.
;    Otherwise it will build only pairs which have `.` notation. Example (cons 1 2) -> (1 . 2)
;    Here we have the second situation where we do not build the list
;    but pairs
;    (cons nil (square 1))
;    (cons (cons nil (square 1)) (square 2))
;    (cons (cons (cons nil (square 1)) (square 2)) (square 3))
;    (cons (cons (cons (cons nil (square 1)) (square 2)) (square 3)) (square 4))
;    which evaluates to ((((() . 1) . 4) . 9) . 16)
;
;    This representation is because we create so called `non-list`
;    pairs, where one of the `cons` arguments is a pair but not a list


