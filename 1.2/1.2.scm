; One of the trivial problems which can describe the difference between
; recursive and iterative processes is calculation of factorial
; 
; n! = n*(n-1)*(n-2)*...*3*2*1
;
; This can be recursivelly defined and executed like this
;
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; (display (factorial 6)) ; 720
; (newline)

; By doing substitution we see the shape of the process
;
; (factorial 6)
; (* 6 (factorial 5))
; (* 6 (* 5 (factorial 4)))
; (* 6 (* 5 (* 4 (factorial 3))))
; (* 6 (* 5 (* 4 (* 3 (factorial 2)))))
; (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
; (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
; (* 6 (* 5 (* 4 (* 3 2))))
; (* 6 (* 5 (* 4 6)))
; (* 6 (* 5 24))
; (* 6 120)
; 720
;
; We can see that representation of this process shows growth in two
; dimensions 
;
; Number of steps required to finish
; Number of elements that have to be saved for later execution
;
; For this process we see that if we add one more element we ahve to add
; two more steps so order of growth is O(2n) ~ O(n)
;
; Here we see as well that we need to save n number of elements for
; input of size n, so we say that it grows in space with O(n).
;
; For process with growth of 
; time  = O(n)
; space = O(n)
;
; we say it is `linear recursive`

; We can have as well different approach to calculating factorial

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter 
        (* product counter)
        (+ counter 1)
        max-count)))

(define (factorial n)
  (fact-iter 1 1 n))

; (display (factorial 6)) ; 720
; (newline)


; Shape of this process is completelly different if we do substitution
;
; (factorial 6)
; (fact-iter 1  1  6)
; (fact-iter 1  2  6)
; (fact-iter 2  3  6)
; (fact-iter 6  4  6)
; (fact-iter 24 5  6)
; (fact-iter 120 6 6)
; (fact-iter 720 7 6)
; 720

; here we see that process is linear in time = O(n) and space is
; constant since we do not have to remember any of the values for the
; next computation
;
; This kind of process is called `linear iteration`
