; Exercise 5.7.
; Use the simulator to test the machines you designed in
; exercise 5.4
; ------------------------------------------------------------
(load "5.2.scm")
(load "../helpers.scm")

(define expt-machine
  (make-machine
    '(counter base product)
    (list (list '- -) (list '* *) (list '= =))
    '(init
       (assign counter (const 3)) ; assuming read is a primitive
       (assign base (const 5))
       (assign product (const 1))
      expt-iter
       (test (op =) (reg counter) (const 0))
       (branch (label expt-done))
       (assign counter (op -) (reg counter) (const 1))
       (assign product (op *) (reg base) (reg product))
       (goto (label expt-iter))
      expt-done)))

(start expt-machine)
(output (get-register-contents expt-machine 'product))
