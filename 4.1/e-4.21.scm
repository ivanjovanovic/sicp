; Exercise 4.21.  Amazingly, Louis's intuition in exercise 4.20 is correct. It
; is indeed possible to specify recursive procedures without using letrec (or
; even define), although the method for accomplishing this is much more subtle
; than Louis imagined. The following expression computes 10 factorial by
; applying a recursive factorial procedure:27

;((lambda (n)
   ;((lambda (fact)
      ;(fact fact n))
    ;(lambda (ft k)
      ;(if (= k 1)
          ;1
          ;(* k (ft ft (- k 1)))))))
 ;10)

; a. Check (by evaluating the expression) that this really does compute
; factorials. Devise an analogous expression for computing Fibonacci numbers.

; b. Consider the following procedure, which includes mutually recursive
; internal definitions:

;(define (f x)
  ;(define (even? n)
    ;(if (= n 0)
        ;true
        ;(odd? (- n 1))))
  ;(define (odd? n)
    ;(if (= n 0)
        ;false
        ;(even? (- n 1))))
  ;(even? x))

; Fill in the missing expressions to complete an alternative definition of f,
; which uses neither internal definitions nor letrec:

;(define (f x)
  ;((lambda (even? odd?)
     ;(even? even? odd? x))
   ;(lambda (ev? od? n)
     ;(if (= n 0) true (od? <??> <??> <??>)))
   ;(lambda (ev? od? n)
     ;(if (= n 0) false (ev? <??> <??> <??>)))))

; ------------------------------------------------------------

; a)
;
