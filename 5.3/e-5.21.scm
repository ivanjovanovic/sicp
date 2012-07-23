; Exercise 5.21.  Implement register machines for the following
; procedures. Assume that the list-structure memory operations are
; available as machine primitives.

; a. Recursive count-leaves:

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

; b. Recursive count-leaves with explicit counter:

(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

; First to recall the list of machine internal instructions.
; assign, branch, goto, save, restore, perform
; ------------------------------------------------------------

; defining the machine

(load "../5.2/5.2.scm")
(load "../helpers.scm")

(define leaf-counter-machine
  (make-machine
    '(continue val tree tmp)
    (list (list 'not not) (list 'car car) (list 'cdr cdr)
          (list 'pair? pair?) (list 'null? null?)
          (list '+ +))
    '(init
       (assign continue (label counting-done))
      ; main loop tests for the state of the tree
      ; and initiates the first recursion
      counter-loop
       (test (op null?) (reg tree))
       (branch (label empty-tree))
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp))
       (branch (label tree-leaf))
       ; invocation of the first recursion
       (save continue)
       (save tree)
       (assign continue (label after-counter-1))
       (assign tree (op car) (reg tree))
       (goto (label counter-loop))
      ; first recursion just initiates the second recursion
      ; while preserving the state of the value register
      after-counter-1
       (restore tree)
       (restore continue)
       (assign tree (op cdr) (reg tree))
       (save continue)
       (save val)
       (assign continue (label after-counter-2))
       (goto (label counter-loop))
      ; after second recursion we update the value of the val register
      after-counter-2
       (assign tmp (reg val))
       (restore val)
       (assign val (op +) (reg val) (reg tmp))
       (restore continue)
       (goto (reg continue))
      tree-leaf
       (assign val (const 1))
       (goto (reg continue))
      empty-tree
       (assign val (const 0))
       (goto (reg continue))
      counting-done)))

(set-register-contents! leaf-counter-machine
                        'tree
                        (list
                          (list
                            (list 1 2)
                            (list 3 4))
                          (list 5 6 7 6 8
                                (list 1 2))))
(start leaf-counter-machine)
(output (get-register-contents leaf-counter-machine 'val)) ; => 11

; b) Similar concept, idea is that outside recursive call to count-leaves is
; tail-recusrsive and doesn't have to be implemented as the subroutine that
; is maintaining passing parameters through the stack.
