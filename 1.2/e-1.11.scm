; Exercise 1.11.  A function f is defined by the rule that 
; f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. 
;
; Write a procedure that computes f by means of a recursive process. 
; Write a procedure that computes f by means of an iterative process.

; ------------------------------------------------------------
; Recursive implementation can be mapped pretty directly from math
; definition.

(define (f n)
  (cond ((< n 3) n)
        (else (+
                 (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

(display (f 15))
(newline)

; timewise this recursive implementation grows with the size of problem
; in exponential fashion. Spacewise it is linear?
;
; time = O(3^n)
; space = O(n)


; Iterative way can be got from formulation based on previous values
;
; for a b c initialy valued as 0 1 2, next step can be defined as
; a = b
; b = c
; c = a + 2b + 3c

(define (f n)
  (cond ((< n 3) n)
        (else (f-iter 0 1 2 (- n 2)))))

(define (f-iter a b c count)
  (if (= count 0)
      c
      (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))

(display (f 15))
(newline)

; Iterative implementation is much more efficient and faster.
; Timewise it is linear and space wise constant.
; It represents standar linear iterative process.
; 
; time = O(n)
; space = O(c)
;
; There is interesting discussion among other comments on Hacker News
; regarding this excerise: http://news.ycombinator.com/item?id=2843715
; Point is that linear transformation of (a, b, c) -> (b, c, 3a + 2b + c) can
; be represented by 3x3 matrix. Iterating n times is as raising matrix
; to the nth power which can be done in O(log(n)) which is better than
; O(n).
;
; Something similar is done in excercise 1.19
