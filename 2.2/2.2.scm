; we can easily create sequence by using cons to create linked pairs
; (display (cons 1 (cons 2 (cons 3 (cons 4)))))

; Scheme provides (list) primitive for easier list construction
; (display (list 1 2 3 4))

; since the list is defined by series of nested conses, car and cdr can
; be used to get elements of the lists

; this will display only first element 1
; (display (car (list 1 2 3 4)))
; this will show (2 3 4)
; (display (cdr (list 1 2 3 4)))

; we can create new list by combining some more elements
; (display (cons 22 (list 1 2 3 4)))

; () represents the end of the list
; (display (cdr (cdr (cdr (cdr (list 1 2))))))


; Use of pairs is suitable for walking through the list with the
; technique called `cdr-ing down` the list.
; Like in this example in which we just return n-th element of the list

(load "../helpers.scm")
(load "../common.scm")
(load "../1.2/e-1.19.scm") ; here we have Fib(n) in log(n) time complexity

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

; this should correctly return 16
; (display (list-ref squares 3))

; usual case is that it is drilled down the whole list, for example for
; reading the length of the list. Implemented in recursive fashion
(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

; (display (length (list 1 2 3 4)))

; length can be as well computed as linear iterative process

(define (length items)
  (define (iter-length items count)
    (if (null? items)
      count
      (iter-length (cdr items) (+ count 1))))
  (iter-length items 0))

; this correctly again gives 4
; (display (length (list 1 2 3 4)))

; Another useful technique is to `cons up` new, result list whil
; `cdr-ing` down the list. Like in the example of append procedure
(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

; this corerctly shows (1 2 3 4 5 6 7 8)
; (display (append (list 1 2 3 4) (list 5 6 7 8)))


; ### Mapping over list

; one example of applying a procedure over all elements of the list is to scale
; all the elements. We will `cons-up` the result while `cdr-ing` down the elements

(define (scale-elements items factor)
  (if (null? items)
    nil
    (cons (* (car items) factor) (scale-elements (cdr items) factor))))

; (output (scale-elements (list 1 2 3 4) 10))

; this can be abstracted as general mapping function

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

; some examples of mapping functions
; (output (map (lambda (x) (+ x 1)) (list 1 2 3 4 5)))
; (output (map abs (list -10 -12.5 12 99 0)))


; Hierarhical data structures
;

; defining small hierarchy by using pair of lists
(define small-tree (cons (list 1 2) (list 3 4)))

; getting length
; (output (length small-tree))

; recursively we can compute number of leaves in the tree
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

; (output (count-leaves small-tree)) ; 4 leaves overall

; 2.2.3 Sequences as conventional interfaces
;
; Consider example of squaring only odd leaves of the tree

(define (sum-odd-squares tree)
  (cond ((null? tree) 0) ; if we got to the end of the tree then 0
        ((not (pair? tree)) ; if we cam to the leaf of the tree than do something
         (if (odd? tree) ; if leaf is odd square it otherwise skip (by returning 0)
           (square tree) 0))
        (else (+ (sum-odd-squares (car tree)) ; preceed with new level
                 (sum-odd-squares (cdr tree))))))

; (output (sum-odd-squares (list 1 2 3 4 5)))

; and the second example of producing the list of even fibonacci
; sequences

(define (even-fib n)
  (define (next k)
    (if (> k n)
      nil
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (next 0))

; (output (even-fib 10))


; These two procedures are structuraly very different, but they share
; some commonality in the way the processes behind can be thought of.
;
; For the first one we do:
;  - enumerate the leave trees
;  - filter only the odd ones
;  - square them
;  - make summ of all of them
;
; For second:
;  - enumerate all integers from 0 to n
;  - compute Fibonacci number of every one
;  - filter out even ones
;  - accumulate all of them in one list

; This can be easily seen as a flow of data through the system blocks
; where data is processed in every block and passed to next block for
; processing.
; 1. enumeration (producing a signal)
; 2. filtering only interesting part of the signal
; 3. mapping a procedure to the filtered signal
; 4. accumulation for end result

; If we represent these signals as lists, then we can apply known list
; operations to manipulate them

; (output (map square (list 1 2 3 4 5)))

; Generalized filtering of the sequence can be done like this

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; (output (filter odd? (list 1 2 3 4 5)))

; Generic accumulation can be implemented this way

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) 
        (accumulate op initial (cdr sequence)))))

; (output (accumulate + 0 (list 1 2 3 4 5)))

; all we need to implement now mentioned examples in signal-flow fashion
; is the signal enumerator itself. One example is

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

; (output (enumerate-interval 1 10))

; To enumerate leaves of the tree

(define (enumerate-leaves tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-leaves (car tree))
                      (enumerate-leaves (cdr tree))))))

; (output (enumerate-leaves (list 1 2 (list 3 4 5) (list 5 6 7) 4 5 6)))

; so now we can implement examples

(define (sum-odd-squares tree)
  (accumulate 
    + 0 (map square (filter odd? (enumerate-leaves tree)))))

; (output (sum-odd-squares (list 1 2 3 4 5))) ; gives 35 as expected

; or defining the enumeration of only even fibs

(define (even-fib n)
  (accumulate
    cons nil (filter even? (map fib (enumerate-interval 0 n)))))

; (output (even-fib 10)) ; -> gives (0 2 8 34)

; playing with this example, we can be creative on various sides since
; we have created very expressive way to talk about our ideas. For
; example, I want squares of fibs until 10

(define (fib-squares n)
  (accumulate
    cons nil (map (lambda (x) (square (fib x)))
                  (enumerate-interval 0 n))))

; (output (fib-squares 10))
