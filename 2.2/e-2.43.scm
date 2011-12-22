; Exercise 2.43.  Louis Reasoner is having a terrible time doing exercise 2.42.
; His queens procedure seems to work, but it runs extremely slowly. (Louis
; never does manage to wait long enough for it to solve even the 6× 6 case.)
; When Louis asks Eva Lu Ator for help, she points out that he has interchanged
; the order of the nested mappings in the flatmap, writing it as

(flatmap
(lambda (new-row)
  (map (lambda (rest-of-queens)
         (adjoin-position new-row k rest-of-queens))
       (queen-cols (- k 1))))
(enumerate-interval 1 board-size))

; Explain why this interchange makes the program run slowly. Estimate how long
; it will take Louis's program to solve the eight-queens puzzle, assuming that
; the program in exercise 2.42 solves the puzzle in time T.
; ------------------------------------------------------------
;

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

; If we analyse how this works in a call graph for size 3 it is obvious
; that in every recursion, 3 new recursive calls are spawned, for level of 3 it means
; 27 calls.
;
; First approach takes O(N) as complexity in the time space since it executes N + 1 calls
; for the input N. The deepest recursive call for N = 0 is just returning empty list, so I'd ignore it.
; So O(N) for any N a bit bigger.
;
; Approach in this exercise does O(N^N) recursive calls. Which is exponentially more resources in 
; the domain of time.
;
; If first took T time for N calls, then X is time for N^N calls, therefore it will take 
; N^N/N times more which converges to N^N for bigger numbers.
