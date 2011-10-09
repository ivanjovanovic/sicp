; Exercise 2.35.
;
; Redefine count-leaves from section 2.2.2 as an accumulation:

; (define (count-leaves t)
;   (accumulate <??> <??> (map <??> <??>)))
; --------------------------------------------------------

(load "../common.scm")
(load "../helpers.scm")
(load "2.2.scm")

(define (count-leaves tree)
  (accumulate
    + 0 
    (map 
      (lambda (x) 1) ; for every element just return binary existence
      (enumerate-leaves tree)))) ; enumerate all the leaves

; this gives 9 as expected
(output (count-leaves (list 1 2 (list 3 4 5) 4 5 (list 1 (list 3)))))
