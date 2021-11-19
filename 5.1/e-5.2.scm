; Exercise 5.2.  Use the register-machine language to describe the
; iterative factorial machine of exercise 5.1.
; ------------------------------------------------------------

(controller
  (assign N (read)) ; assuming read is given as a primitive
  (assign p (const 1))
  (assign c (const 1))
iter
  (test (op >) (reg c) (reg N))
  (branch (label iter-done))
  (assign p (op *) (reg p) (reg c))
  (assign c (op +) (reg c) (const 1))
  (goto (label iter))
iter-done)
