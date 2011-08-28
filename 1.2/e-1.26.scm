; Exercise 1.26.  
;
; Louis Reasoner is having great difficulty doing exercise 1.24. His fast-prime? test seems to run more slowly 
; than his prime? test. Louis calls his friend Eva Lu Ator over to help. 
; When they examine Louis's code, they find that he has rewritten the 
; expmod procedure to use an explicit multiplication, rather than calling square:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; ``I don't see what difference that could make,'' says Louis. 
; ``I do.'' says Eva. ``By writing the procedure like that, you have transformed the (log n) process into a (n) process.'' Explain.
; -----------------------------
;
; If we know that applicative substitution evaluates parameters every time then
; it is evaluating 2 times expmod for every expmod call. Therefore, despite the
; fact we are halving the problem we are doubling the number of calls. So at the
; end we are cancelling logarithm with exponend and then getting to the O(n)
; order of growth.
;
; The one that do squaring instead of multiplication has only one call per
; recursion and with halving the size of the problem every time it is converging
; logarithmicaly.
