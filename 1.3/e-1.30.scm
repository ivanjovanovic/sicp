; Exercise 1.30.  
;
; The sum procedure above generates a linear recursion. 
; The procedure can be rewritten so that the sum is performed iteratively. 
; Show how to do this by filling in the missing expressions in the following definition:
;
; (define (sum term a next b)
;   (define (iter a result)
;     (if <??>
;         <??>
;         (iter <??> <??>)))
;   (iter <??> <??>))
; ---------------------------------

; One thing to note is that all arguments passed to sum procedure are available
; in internal iter procedure as well.

(load "../common.scm")

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; define test function on top of this implementation of sum

(define (sum-cubes a b)
  (define (next x) (+ x 1))
  (sum cube a next b))

; (display (sum-cubes 1 10)) ; expected 3025
; (newline)
