; Exercise 2.37.
;
; Suppose we represent vectors v = (vi) as sequences of numbers, and matrices m = (mij) as sequences
; of vectors (the rows of the matrix). For example, the matrix:
;
; | 1 2 3 4 |
; | 4 5 6 6 |
; | 6 7 8 9 |


; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)).
; With this representation, we can use sequence operations to concisely express the basic matrix
; and vector operations. These operations (which are described in any book on matrix algebra) are the following:
;
; (dot-product v w) ; returns the sum SUMi(vi*wi)
; (matrix-*-vector m v) ; returns vector t where ti = SUMj(mij*vj)
; (matrix-*-matrix m n) ; returns matrix p where pij = SUMk(mik*nkj)
; (transpose m) ; returns matrix n where nij = mji

; We can define the dot product as

; (define (dot-product v w)
;   (accumulate + 0 (map * v w)))

; Fill in the missing expressions in the following procedures for computing the
; other matrix operations. (The procedure accumulate-n is defined in exercise 2.36.)

; (define (matrix-*-vector m v)
;   (map <??> m))
; (define (transpose mat)
;   (accumulate-n <??> <??> mat))
; (define (matrix-*-matrix m n)
;   (let ((cols (transpose n)))
;     (map <??> m)))
;
; ------------------------------------------------------------------------------

(load "../helpers.scm")
(load "../common.scm") ; standard sequence procedures are there

(define test-matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

; so, dot product is defined by using more generic map definition
; which is provided by Scheme
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(output (dot-product (list 1 2 3) (list 5 6 7)))

; 1. matrix * vector
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(output (matrix-*-vector test-matrix (list 1 2 3 4)))

; 2. Transposition
(define (transpose m)
  (accumulate-n cons nil m))

(output (transpose test-matrix))

; 3. matrix time matrix
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (map (lambda (y) (dot-product x y)) cols)) m)))

(output (matrix-*-matrix test-matrix (transpose test-matrix)))


; This is very interesting. Usually, I would think of this kind of
; problem as iterative cycling through the elements of data and then doing
; operations to combine them and get results.
; With standard sequence operations it is very easy to
; combine things and express calculations in more high level langauge
; without even having in mind that there is some looping over the data.
;
; I used following link to play with matrices
; http://www.bluebit.gr/matrix-calculator/matrix_multiplication.aspx
;
; I used Khan academy videos to refresh a bit my mind on matrix
; multiplications
; http://www.khanacademy.org/video/matrix-multiplication--part-1
