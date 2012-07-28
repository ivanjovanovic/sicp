; Exercise 5.29.
;
; Monitor the stack operations in the tree-recursive Fibonacci
; computation:

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

; a. Give a formula in terms of n for the maximum depth of the stack required
; to compute Fib(n) for n > 2. Hint: In section 1.2.2 we argued that the space
; used by this process grows linearly with n.

; b. Give a formula for the total number of pushes used to compute Fib(n) for n
; > 2. You should find that the number of pushes (which correlates well with
; the time used) grows exponentially with n. Hint: Let S(n) be the number of
; pushes used in computing Fib(n). You should be able to argue that there is a
; formula that expresses S(n) in terms of S(n - 1), S(n - 2), and some fixed
; ``overhead'' constant k that is independent of n. Give the formula, and say
; what k is. Then show that S(n) can be expressed as a Fib(n + 1) + b and give
; the values of a and b.
