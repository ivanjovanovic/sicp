; Exercise 2.40.  
;
; Define a procedure unique-pairs that, given an integer n, generates the 
; sequence of pairs (i,j) with 1< j< i< n. Use unique-pairs to simplify 
; the definition of prime-sum-pairs given above.
; ----------------------------------------------

(load "../common.scm")
(load "../helpers.scm")
(load "2.2.scm")

; defining generator of unique pairs

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; (output (unique-pairs 10))

; by using this procedure prime sum of pairs looks pretty
; straightforward

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(output (prime-sum-pairs 10))

