; Exercise 3.19.
;
; Redo exercise 3.18 using an algorithm that takes only a
; constant amount of space. (This requires a very clever idea.)
; ------------------------------------------------------------

; First to analyze a bit the solution in 3.18.
; We are storing every visited node in the history object. Therefore,
; dependent on the length of the list this will grow over time. 
; Its space growth is characterized by O(n) asymptotic approximation.

; Robert Floyd (http://en.wikipedia.org/wiki/Robert_W._Floyd) has found in
; the late 1960s a very clever way to find the cycle in any part of the list.
; We just have to make two pointers on the top of the list and then go
; through the list in a way that we increase first one to the next position and
; second one for two positions. If there is a cycle in the list these two
; pointers will eventually meet, otherwise they will both end in null end and
; exit the algorithm loop.

; For more formal information about the approach
; @see http://en.wikipedia.org/wiki/Cycle_detection

(load "../helpers.scm")

(define (cycled? l)
  (if (null? (cdr l)) 
    '#f
    (let ((slow l)
          (fast (cdr l)))
      (define (recur)
        (cond ((or (null? (cdr slow))
                   (or (null? (cdr fast)) (null? (cddr fast))))
               '#f)
              ((eq? slow fast) '#t)
              (else
                (begin
                  (set! slow (cdr slow))
                  (set! fast (cddr fast))
                  (recur)))))
      (recur))))

; no cycle in simple linked list
(output (cycled? (list 1 2 3))) ; #f

; build cycled list out of pairs
(define first (cons 1 2))
(define second (cons 1 2))
(define third (cons 1 2))
(define forth (cons 1 2))
(define fifth (cons 1 2))

(set-cdr! first second)
(set-cdr! second third)
(set-cdr! third forth)
(set-cdr! forth fifth)
(set-cdr! fifth second)

(output (cycled? first)) ; #t
