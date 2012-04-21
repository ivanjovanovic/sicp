; Library of common procedures used in exercises which do not particularly participate
; in solution design but are used only as helpers and can be shared among.

(define true #t)
(define false #f)

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (div a b)
  (floor (/ a b)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (identity x) x)

(define close-enough?
  (lambda (a b delta) (< (abs (- a b)) delta)))

; average of 2 values
(define (average x y)
  (/ (+ x y) 2.0))

; avergae of 3.0
(define (average-of-3 x y z)
  (/ (+ x y z) 3.0))

(define (inc x)
  (+ x 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; define nil as empty list.
(define nil '())

;;;; standard sequence procedures

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-leaves tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-leaves (car tree))
                      (enumerate-leaves (cdr tree))))))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs)) ; get cars of every list inside
            (accumulate-n op init (map cdr seqs))))) ; proceed with cadrs in next recursion

; Folding to left and right are standard operations
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; folding right is the standard accumulation
(define fold-right accumulate)

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
