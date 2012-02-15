(load "../helpers.scm")
(load "e-3.26.scm")
; Exercise 3.27.
;
; Memoization (also called tabulation) is a technique that
; enables a procedure to record, in a local table, values that have previously
; been computed. This technique can make a vast difference in the performance
; of a program. A memoized procedure maintains a table in which values of
; previous calls are stored using as keys the arguments that produced the
; values. When the memoized procedure is asked to compute a value, it first
; checks the table to see if the value is already there and, if so, just
; returns that value. Otherwise, it computes the new value in the ordinary way
; and stores this in the table. As an example of memoization, recall from
; section 1.2.2 the exponential process for computing Fibonacci numbers:

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; given that memoization is defined as saving to a table
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

; The memoized version of the same procedure is

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

; Draw an environment diagram to analyze the computation of (memo-fib 3).
; Explain why memo-fib computes the nth Fibonacci number in a number of steps
; proportional to n. Would the scheme still work if we had simply defined
; memo-fib to be (memoize fib)?
;
; ------------------------------------------------------------


; This one is a bit complex but still straight forward if we know how evaluation and 
; application in environment model work. Lets get a bit back to it.
;
; (define (square x) (* x x)) is just sytactic sugar for
; (define square (lambda (x) (* x x)))
;
; Definitions are evaluates in a way that body of definition (lambda (x) (* x x)) is evaluated
; and name square is attached to procedure produced by lambda in the enclosing environment. In this
; case global one.
;
; Our case is a bit more complex since we have definition
;
; (define memo-fib (memoize (lambda (n) ...))
; which means that memo-fib is defined as the result of evaluation of (memoize) procedure which receives
; actual fibonnaci number generator procedure.
; Here is important to know how this is evaluated. By the aplication rules
;
; 1. First evaluate all the subexpressions of the combination
; 2. Apply the value of operator subexpression to the values of operand expressions
;
; In our case we first evaluate memoize to a procedure, and fibonacci generator lambda and then we apply memoize.
; Important is to notice that (lambda (n) ...) as argument to memoize is producing new procedure which points to
; the global environment.

; After we understand the layout it is easy just to go step by step and see what is going on
; with the flow.
;
; O(n)?
; -------------
; This works with O(n) order of growth because when results are memoized, for every fibonacci number
; we need only constant number of steps to calculate it. We don't calculate already calculated values again
; So if we have constant number of steps in every recursion, then it will grow lineraly with the size
; of the problem.
;
; (memoize fib)?
; --------------------
; Simple as this will not work since fib doesn't call memo-fib in recursion and does not go through
; the check if value exist in the table
