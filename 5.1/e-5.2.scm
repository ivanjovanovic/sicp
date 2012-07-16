; Exercise 5.2.  Use the register-machine language to describe the
; iterative factorial machine of exercise 5.1.
; ------------------------------------------------------------

(controller
  (assign N (read)) ; assuming read is given as a primitive
  (assign p 1)
  (assign c 1)
iter
  (test (op >) (reg c) (const N))
  (branch (label iter-done))
  (assign p (op *) (reg p) (reg c))
  (assign c (op +) (reg c) (const 1))
  (goto (label iter))
iter-done)
