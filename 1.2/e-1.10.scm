; Exercise 1.10.  The following procedure computes a mathematical function called Ackermann's function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; What are the values of the following expressions?

; (A 1 10)
; (A 2 4)
; (A 3 3)

; Consider the following procedures, where A is the procedure defined above:

; (define (f n) (A 0 n))
; (define (g n) (A 1 n))
; (define (h n) (A 2 n))
; (define (k n) (* 5 n n))

; Give concise mathematical definitions for the functions computed by the 
; procedures f, g, and h for positive integer values of n. For example, (k n) computes 5n^2.
;
;--------------------------------------
;
; If we do substitution by the fiven rules we can see how procedures
; evolve and where they end.
;
; 1. (A 1 10)
;
; (A 1 10)
; (A 0 (A 1 9))
; (* 2 (A 1 9))
; (* 2 (A 0 (A 1 8)))
; (* 2 (* 2 (A 0 (A 1 7))))
; (* 2 (* 2 (* 2 (A 0 (A 1 6)))))
; (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 5))))))
; (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 4)))))))
; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 3))))))))
; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 2)))))))))
; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 1))))))))))
; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 2)))))))))
; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 4))))))))
; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 8)))))))
; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 16))))))
; (* 2 (* 2 (* 2 (* 2 (* 2 32)))))
; (* 2 (* 2 (* 2 (* 2 64))))
; (* 2 (* 2 (* 2 128)))
; (* 2 (* 2 256)
; (* 2 512)
; 1024
;
; (display (A 1 10)) => 1024
; (newline)

; Substitution
;
; (A 2 4)
; (A 1 (A 2 3))
; (A 0 (A 1 (A 2 2)))
; (* 2 (A 1 (A 1 (A 2 1)))
; .....
; it expands recursivelly pretty crazy. Overall the point is that
; recursion can be very colrful in the shapes when we apply substitution
; model for some pretty simple definitions.
; 
; (display (A 2 4)) => 65536
; (newline)

; (display (A 3 3)) => 65536
; (newline)
;
; --------------------------------
;
; (define (f n) (A 0 n))
; Directly from the definition we see that this computes 2*n
;
; (define (g n) (A 1 n))
; From the expansion we already did we can see it is 2^n what this represents
;
; (define (h n) (A 2 n))
; This reprsents the function which calculates iterated exponentiation (tetration).
; This is for very small numbers of n already really big value and it is defined as
; in terms of Knuth's up arrow notation: http://en.wikipedia.org/wiki/Knuth's_up-arrow_notation
; as 2 ↑↑ n which is 2 ↑ 2 ↑ 2 ↑ ... ↑ 2 .

