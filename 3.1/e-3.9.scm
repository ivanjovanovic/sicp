; Exercise 3.9.
;
; In section 1.2.1 we used the substitution model to
; analyze two procedures for computing factorials, a recursive version

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; and an iterative version

(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; Show the environment structures created by evaluating (factorial 6)
; using each version of the factorial procedure.14
;
; --------
;
; Solution to this is drawing of the environmental model for both of the
; executions. There is nothing special to see here except the fact that
; application of same function will produce new frames every time it is
; applied to the new arguments.
