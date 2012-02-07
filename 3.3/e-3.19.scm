; Exercise 3.19.
;
; Redo exercise 3.18 using an algorithm that takes only a
; constant amount of space. (This requires a very clever idea.)
; ------------------------------------------------------------

; First to analyze a bit the solution in 3.18.
; We are storing every visited node in the history object. Therefore,
; dependent on the length of the list this will grow over time. 
; Its space growth is characterized by O(n) asymptotic approximation.

(define (cycled? l))
