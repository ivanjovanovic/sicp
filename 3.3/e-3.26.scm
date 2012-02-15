; Exercise 3.26.
;
; To search a table as implemented above, one needs to scan
; through the list of records. This is basically the unordered list
; representation of section 2.3.3. For large tables, it may be more efficient
; to structure the table in a different manner. Describe a table implementation
; where the (key, value) records are organized using a binary tree, assuming
; that keys can be ordered in some way (e.g., numerically or alphabetically).
; (Compare exercise 2.66 of chapter 2.)
; ------------------------------------------------------------

(load "../helpers.scm")

; In previous exercise we have implemented unlimited nesting of 
; linked lists. In this one, every simple list is implemeted as a tree 
; and to make it hierarchically generic we implement our table so it
; is constructed by nesting these binary trees.
;
; first we need way to build ordered list represented as the tree
; from 2.3-binary-trees.scm we take already defined elements

; tree abstraction, constructor and selectors
(define (make-tree entry left right) (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

; then we need a way to add element to the tree and to maintain it as
; ordered. Our element here is a (key, value) pair where we order by 
; numerical value of the key.
(define (adjoin-tree element tree)
  (cond ((null? tree) (make-tree element '() '()))
        ((= (key element) (key (entry tree))) tree)
        ((< (key element) (key (entry tree)))
         (make-tree (entry tree)
                    (adjoin-tree element (left-branch tree))
                    (right-branch tree)))
        ((> (key element) (key (entry tree)))
         (make-tree (entry tree)
                    (left-branch tree)
                    (adjoin-tree element (right-branch tree))))))

; to find element in the tree we use

(define (lookup-tree lookup-key tree)
  (cond ((null? tree) '#f)
        ((= lookup-key (key (entry tree))) (entry tree))
        ((< lookup-key (key (entry tree))) (lookup-tree lookup-key (left-branch tree)))
        ((> lookup-key (key (entry tree))) (lookup-tree lookup-key (right-branch tree)))))


; in previous procedure we used simple selector for getting key out of element
(define (key element) (car element))

; testing tree creation
(define t (adjoin-tree (cons 100 'a) '()))
(define t (adjoin-tree (cons 200 'b) t))
(define t (adjoin-tree (cons 50 'b) t))
(define t (adjoin-tree (cons 1000 'b) t))
(define t (adjoin-tree (cons 10 'b) t))
; ((100 . a) ((50 . b) ((10 . b) () ()) ()) ((200 . b) () ((1000 . b) () ())))
; (output t)
; (output (lookup-tree 100 t)) ; (100 . a)
; (output (lookup-tree 1000 t)) ; (1000 . b)
; (output (lookup-tree 1 t)) ; #f

; now that we can make simple tree, we can make our nested table by nesting
; them instead nesting lists. I'll make it as a message dispatcher function with the
; nested state

(define (make-table)
  (let ((local-table (list '*tabler)))
    
    (define (lookup keys)
      (define (lookup-recursive keys table)
        (let ((subtable (lookup-tree (car keys) (cdr table))))
          (if subtable
            (if (null? (cdr keys))
              (cdr subtable)
              (lookup-recursive (cdr keys) subtable))
            '#f)))
      (lookup-recursive keys local-table))

    (define (insert! keys value)
      (define (make-elements keys)
        (if (null? (cdr keys))
          (cons (car keys) value)
          (cons (car keys) (make-tree (make-elements (cdr keys)) '() '()))))
      (define (insert-recursive! keys table)
        (let ((subtable (lookup-tree (car keys) (cdr table))))
          (if subtable
            (if (null? (cdr keys))
              (set-cdr! subtable value)
              (insert-recursive! (cdr keys) subtable))
            (set-cdr! table (adjoin-tree (make-elements keys) (cdr table))))))
      (insert-recursive! keys local-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

; do some testing
(define t (make-table))
(define put (t 'insert-proc!))
(define get (t 'lookup-proc))

(put '(100) 'a)
; (output (get '(100)))
; (output (get '(1000)))
(put '(1000) 'b)
; (output (get '(1000)))

; Since this is not a perfect implementation it doesnt cover the case
; when you already have defined (put '(100) 'a) and want to define
; over it (put '(100 200) 'c), it will throw an error. It needs
; one more check in the code
(define t (make-table))
(define put (t 'insert-proc!))
(define get (t 'lookup-proc))

(put '(100 200) 'c)
(put '(100 300) 'd)
; (output (get '(100 200)))
; (output (get '(100 300)))

; Comparison with exercise 2.66
;
; In 2.66 we have only one binary tree in concern and the lookup
; procedure to find element inside it.
; In this exercise, we reuse the abstraction of the binary tree for representing
; ordered set, but go one level of abstraction higher to make these trees nestable in a generic way.
; So, on one level of the tree we have comparison by keys and value of every element of the tree can contain
; either value or subtree which is again ordered by the key and can contain other subtrees.
