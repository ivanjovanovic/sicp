; Exercise 2.42.

; The ``eight-queens puzzle'' asks how to place eight queens on a
; chessboard so that no queen is in check from any other 
; (i.e., no two queens are in the same row, column, or diagonal).
; One possible solution is shown in figure 2.8. One way to solve the
; puzzle is to work across the board, placing a queen in each column. Once
; we have placed k - 1 queens, we must place the kth queen in a position
; where it does not check any of the queens already on the board. We can
; formulate this approach recursively: Assume that we have already
; generated the sequence of all possible ways to place k - 1 queens in the
; first k - 1 columns of the board. For each of these ways, generate an
; extended set of positions by placing a queen in each row of the kth
; column. Now filter these, keeping only the positions for which the queen
; in the kth column is safe with respect to the other queens. This
; produces the sequence of all ways to place k queens in the first k
; columns. By continuing this process, we will produce not only one
; solution, but all solutions to the puzzle.

; We implement this solution as a procedure queens, which returns a
; sequence of all solutions to the problem of placing n queens on an n×
; n chessboard. Queens has an internal procedure queen-cols that returns
; the sequence of all ways to place queens in the first k columns of the
; board.

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; In this procedure rest-of-queens is a way to place k - 1 queens in the
; first k - 1 columns, and new-row is a proposed row in which to place
; the queen for the kth column. Complete the program by implementing the
; representation for sets of board positions, including the procedure
; adjoin-position, which adjoins a new row-column position to a set of
; positions, and empty-board, which represents an empty set of
; positions. You must also write the procedure safe?, which determines
; for a set of positions, whether the queen in the kth column is safe
; with respect to the others. (Note that we need only check whether the
; new queen is safe -- the other queens are already guaranteed safe with
; respect to each other.)
; -------------------------------------------

; First to define empty board, that is easy. Since we are going to store
; the positions as list of pairs, then empty board means empty list

(load "../common.scm")
(load "../helpers.scm")
(load "2.2.scm")

(define empty-board nil)

; The way we generate all the possible positions queen can be in the new
; iteration is defined by the rest of queens (valid solutions the
; smaller problem set) and new row injected into the problem. So we have
; to generate combinations.

(define (adjoin-position new-row k rest-of-queens)
  (if (null? rest-of-queens)
    (list (list (list new-row k)))
    (map (lambda (one-position-set)
           ; we'll put k-th position element in front
           (append (list (list new-row k))
                   one-position-set))
         rest-of-queens)))

; Here we define filter that checks if k-th queen is placed on the safe
; place within the position set, taking that all previous elements of
; the set comprise a valid set.
(define (safe? k positions) ; (= 1 1))
  (if (null? (cdr (car positions)))
    true
    (not (colides? k (caar positions) (cdr (car positions))))))

(define (colides? k position rest-of-positions)
  (or (same-row? position rest-of-positions)
      (same-diagonal? k position rest-of-positions)))

(define (same-row? position rest-of-positions)
  (not (null? (filter (lambda (rest-position)
                   (= (car position) (car rest-position)))
                 rest-of-positions))))

(define (same-diagonal? k position rest-of-positions)
  (or (same-upper-diagonal? k position rest-of-positions)
      (same-lower-diagonal? k position rest-of-positions)))

(define (same-upper-diagonal? k position rest-of-positions)
  (not 
    (null? 
      (flatmap (lambda (diagonal-position)
             (filter (lambda (rest-position)
                       (and (= (car diagonal-position) (car rest-position))
                            (= (cadr diagonal-position) (cadr rest-position))))
                     rest-of-positions))
           (enumerate-upper-diagonal k position)))))

(define (same-lower-diagonal? k position rest-of-positions)
  (output rest-of-positions)
  (not 
    (null? 
      (flatmap (lambda (diagonal-position)
             (filter (lambda (rest-position)
                       (and (= (car diagonal-position) (car rest-position))
                            (= (cadr diagonal-position) (cadr rest-position))))
                     rest-of-positions))
           (enumerate-lower-diagonal k position)))))

(define (enumerate-upper-diagonal k position)
  (map (lambda (row)
         (list row (+ (- (cadr position) (car position)) row)))
       (enumerate-interval 1 (- (car position) 1))))

(define (enumerate-lower-diagonal k position)
  (output k position)
  (map (lambda (row)
         (list row (+ (- (cadr position) row) (car position))))
       (enumerate-interval (+ (car position) 1) k)))

; (output (adjoin-position 2 3 (list)))
; (output (queens 4))
; (output (same-lower-diagonal? 3 (list 3 3) (list (list 4 2))))
; (output (enumerate-upper-diagonal 4 (list 3 4)))
(output (enumerate-lower-diagonal 3 (list 3 3)))
