; Exercise 2.29.
;
; A binary mobile consists of two branches, a left branch and a right branch.
; Each branch is a rod of a certain length, from which hangs either a weight or
; another binary mobile. We can represent a binary mobile using compound data by
; constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

; A branch is constructed from a length (which must be a number) together
; with a structure, which may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

; a.  Write the corresponding selectors left-branch and right-branch,
;     which return the branches of a mobile, and branch-length and
;     branch-structure, which return the components of a branch.
; b.  Using your selectors, define a procedure total-weight that returns
;     the total weight of a mobile.
; c.  A mobile is said to be balanced if the torque applied by its top-left
;     branch is equal to that applied by its top-right branch (that is, if the
;     length of the left rod multiplied by the weight hanging from that rod is
;     equal to the corresponding product for the right side) and if each of the
;     submobiles hanging off its branches is balanced. Design a predicate that
;     tests whether a binary mobile is balanced.
; d.  Suppose we change the representation of mobiles so that the constructors are

; (define (make-mobile left right)
;   (cons left right))
; (define (make-branch length structure)
;   (cons length structure))

; How much do you need to change your programs to convert to the new representation?
; ----------------------------------------------------------------------------------

(load "../helpers.scm")
; first lets define selectors without thinking for compatibility with
; the future changes

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (cond ((not (pair? mobile)) 0) ; are we done with traversing tree
        ((not (pair? (branch-structure mobile))) (branch-structure mobile)) ; is last element weight
        (else (+ (total-weight (left-branch mobile))
                 (total-weight (right-branch mobile))))))

(define simple-mobile
  (make-mobile
    (make-branch 10 20)
    (make-branch 10 20)))

(define double-mobile
  (make-mobile
    (make-branch 5 simple-mobile)
    (make-branch 15 simple-mobile)))

(define quad-mobile
  (make-mobile
    (make-branch 5 double-mobile)
    (make-branch 15 double-mobile)))


(output (total-weight simple-mobile))
(output (total-weight double-mobile))
(output (total-weight quad-mobile))

; if we redefine the way data is represented and redefine all the
; objects with new structure

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))


(define simple-mobile
  (make-mobile
    (make-branch 10 20)
    (make-branch 10 20)))

(define double-mobile
  (make-mobile
    (make-branch 5 simple-mobile)
    (make-branch 15 simple-mobile)))

(define quad-mobile
  (make-mobile
    (make-branch 5 double-mobile)
    (make-branch 15 double-mobile)))

; we see that these now provide false values.
(output (total-weight simple-mobile)) ; -> 10
(output (total-weight double-mobile)) ; -> 15
(output (total-weight quad-mobile)) ; -> 15

; therefore, we'll have to define new selectors for tails of the data
; since now we use pairs instead of lists

(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

; and see that now we get again correct results
(output (total-weight simple-mobile))
(output (total-weight double-mobile))
(output (total-weight quad-mobile))

; There is one thing which can be seen here. If we represent our
; abstraction layers as
;
;
;  -----------------------------------------
;   total weight (data usage)
;  -----------------------------------------
;   constructor / selectors abstraction
;  -----------------------------------------
;   data structure/representation
;  ---------------------------------------
;
;  we can see that by changing data structure, we had to change only two
;  selectors in constructor/selectors layer. The way data is used was
;  not affected by this change, which tells that there was a good
;  separation of concerns between data representation and data usage
;  layers through constructor/selector layers.
