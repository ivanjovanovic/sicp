; Exercise 3.17.
;
; Devise a correct version of the count-pairs procedure of
; exercise 3.16 that returns the number of distinct pairs in any structure.
; (Hint: Traverse the structure, maintaining an auxiliary data structure that
; is used to keep track of which pairs have already been counted.)
; ------------------------------------------------------------

(load "../helpers.scm")
(load "e-3.12.scm")

; We update count-pairs with some kind of global history object
; to save the pointers to pairs it has visited
(define (count-pairs x)
  (if (or (not (pair? x)) 
          ((history 'visited?) x))
      0
      (begin
        ((history 'add) x)
        (+ (count-pairs (car x))
           (count-pairs (cdr x))
           1))))

; First to make a list to keep track of pairs that have been traversed
(define history
  ; setting initial history it to null so append have to what to append actually ;)
  (let ((visited-list (cons '() '()))) 

    (define (visited? x)
      (define (iter visited-list x)
        (cond ((null? visited-list) '#f)
              ((eq? (car visited-list) x) '#t)
              (else (iter (cdr visited-list) x))))
      (iter visited-list x))
    
    (define (add x)
      (append! visited-list (cons x '()))
      visited-list)

    (lambda (m)
      (cond ((eq? m 'add) add)
            ((eq? m 'visited?) visited?)
            ((eq? m 'reset) (set! visited-list (cons '() '())))
            (else (error "Unknow operation on history"))))))

; (define l (list 1 2 3)) ; three pairs, linked in a list
; (output (count-pairs l))
(define first (cons 1 2))
(define second (cons 3 4))
(define l (cons first second))
(output ((history 'add) first))
(output ((history 'visited?) second))

(history 'reset)

(define l (list 1 2 3)) ; three pairs, linked in a list
(output (count-pairs l))

(define first (cons 1 2))
(define second (cons 1 2))
(define third (cons first second))
(set-car! second first)
(output (count-pairs third)) ; 4

(history 'reset)
  
(define first (cons 1 2))
(define second (cons first first))
(define third (cons second second))
(output (count-pairs third))

(history 'reset)

; Now it returns 3 even with the cycle in the data structure
(define first (cons 1 2))
(define second (cons 1 2))
(define third (cons 1 2))
(set-cdr! first second)
(set-cdr! second third)
(set-cdr! third first)
(output (count-pairs first))

(history 'reset)

(define l (list 1 2 3))
(output (count-pairs l))

(history 'reset)

(define l (list 1 2 3 4 5 6))
(append! l l)
(output (count-pairs l))
