; Exercise 2.25.
; Give combinations of cars and cdrs that will pick
; 7 from each of the following lists:

; (1 3 (5 7) 9)

; ((7))

; (1 (2 (3 (4 (5 (6 7))))))
; ----------------------------

(load "../helpers.scm")

(define first (list 1 3 (list 5 7) 9))
(define second (list (list 7)))
(define third (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

; combinations
(output (car (cdr (car (cdr (cdr first))))))
(output (car (car second)))

; I wrongly assumed that I need just to cdr-down the third definition.
; (cdr third) -> ((2 (3 (4 (5 (6 7)))))) -> we have to do one car after
; every cdr
(output (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr third)))))))))))))

