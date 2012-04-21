; Exercise 3.57.
;
; How many additions are performed when we compute the
; nth Fibonacci number using the definition of fibs based on the
; add-streams procedure? Show that the number of additions would be
; exponentially greater if we had implemented (delay <exp>) simply as
; (lambda () <exp>), without using the optimization provided by the
; memo-proc procedure described in section 3.5.1
; ------------------------------------------------------------

; When we use memoization of the sequence element values, number of additions is n - 1 since for every
; Nth fibonacci we have to calculate previous n-1, and we do every
; calculation only once.
;
; As it was discussed during examinations of orders of growth regarding
; the fibonacci sequence, it takes order of growth O(Fib(N)) to
; calculate it recursivelly without memoization.
; Somehow differently presented, but the order of growth here is the
; same since we will have to do all the additions for every Fib sequence
; element, thus it will grow exponentially.
