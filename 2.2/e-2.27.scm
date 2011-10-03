; Exercise 2.27.
;
; Modify your reverse procedure of exercise 2.18
; to produce a deep-reverse procedure that takes a
; list as argument and returns as its value the list with
; its elements reversed and with all sublists deep-reversed as well. For example,

(define x (list (list 1 2) (list 3 4)))

; x -> ((1 2) (3 4))

; (reverse x) -> ((3 4) (1 2))

; (deep-reverse x) -> ((4 3) (2 1))

(load "../common.scm")
(load "../helpers.scm")
(load "../e-2.18.scm")

(output (reverse x))

; we can recursevly apply reverse based on check if argument is pair

(define (deep-reverse items)
  (cond ((null? items) nil)
        ((not (pair? items)) items)
        (else (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))))

; and result is as expected
(output (deep-reverse x))

; lets check a bit more complex task

(define y (list (list (list 1 2 3) (list 4 5 6)) (list 7 8 9)))
(output (deep-reverse y)) ; -> quite correctly ((9 8 7) ((6 5 4) (3 2 1)))



