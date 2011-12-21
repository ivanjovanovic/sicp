; Exercise 2.42.

; The ``eight-queens puzzle'' asks how to place eight queens on a
; chessboard so that no queen is in check from any other 
; (i.e., no two queens are in the collides row, column, or diagonal).
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

; (define (queens board-size)
;   (define (queen-cols k)
;     (if (= k 0)
;         (list empty-board)
;         (filter
;          (lambda (positions) (safe? k positions))
;          (flatmap
;           (lambda (rest-of-queens)
;             (map (lambda (new-row)
;                    (adjoin-position new-row k rest-of-queens))
;                  (enumerate-interval 1 board-size)))
;           (queen-cols (- k 1))))))
;   (queen-cols board-size))

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

(load "../common.scm")
(load "../helpers.scm")
(load "2.2.scm")

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

; First to define empty board, that is easy. Since we are going to store
; the positions as list of pairs, then empty board means empty list
(define empty-board nil)

; The way we generate all the possible positions queen can be in the new
; iteration is defined by the rest of queens (valid solutions the
; smaller problem set) and new row injected into the problem. So we have
; to generate combinations.

; I kind of don't like how I manipulate the lists here. I had to make
; list in the list in the list so they properly append to form list of
; positions. I'd like to do this in a simpler and more robust manner.

(define (adjoin-position new-row k rest-of-queens)
  (if (null? rest-of-queens)
    (list (list (list new-row k)))
    (map (lambda (one-position-set)
           ; we'll put k-th position element in front for easier
           ; selection later
           (append (list (list new-row k))
                   one-position-set))
         rest-of-queens)))

; Here we define filter that checks if k-th queen is placed on the safe
; place within the position set, taking that all previous elements of
; the set comprise a valid set.
(define (safe? k positions)
  (if (null? (cdr (car positions)))
    true
    (not (colides? (caar positions) (cdr (car positions))))))

; here are defined set of methods to check for th ecollision of the
; newly placed queen with the already placed on board
(define (colides? position rest-of-positions)
  (or (collides-row? position rest-of-positions)
      (collides-diagonal? position rest-of-positions)))

; is it in the same row with other
(define (collides-row? position rest-of-positions)
  (not (null? (filter (lambda (rest-position)
                   (= (car position) (car rest-position)))
                 rest-of-positions))))

; does it collide in any diagonal
(define (collides-diagonal? position rest-of-positions)
  (or (collides-upper-diagonal? position rest-of-positions)
      (collides-lower-diagonal? position rest-of-positions)))

; check collision in upper diagonal
(define (collides-upper-diagonal? position rest-of-positions)
  (not (null? 
         (filter (lambda (rest-position)
                   (= (cadr rest-position)
                      (+ (- (cadr position) (car position))
                         (car rest-position))))
                 rest-of-positions))))

; check collision in lower diagonal
(define (collides-lower-diagonal? position rest-of-positions)
  (not (null? 
         (filter (lambda (rest-position)
                   (= (cadr rest-position)
                      (+ (- (cadr position) (car rest-position))
                         (car position))))
                 rest-of-positions))))

; compact version of collision detection which inlines all the cases in
; one mehtod.
(define (compact-colides? position rest-of-positions)
  (not (null? (filter (lambda (rest-position)
                        (or (= (car position) (car rest-position))
                            (= (cadr rest-position)
                               (+ (- (cadr position) (car position))
                                  (car rest-position)))
                            (= (cadr rest-position)
                               (+ (- (cadr position) (car rest-position))
                                  (car position)))))
                      rest-of-positions))))

; redefining safe to use compact version of collision check
(define (safe? k positions)
  (if (null? (cdr (car positions)))
    true
    (not (compact-colides? (caar positions) (cdr (car positions))))))

; (output (adjoin-position 2 3 (list)))
(output (queens 6))
; (output (collides-lower-diagonal? 3 (list 3 3) (list (list 4 2))))
; (output (enumerate-upper-diagonal 4 (list 3 4)))
; (output (enumerate-lower-diagonal 3 (list 3 3)))
