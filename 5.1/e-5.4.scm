; Exercise 5.4.  Specify register machines that implement each of the
; following procedures. For each machine, write a controller instruction
; sequence and draw a diagram showing the data paths.

; a. Recursive exponentiation:

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; b. Iterative exponentiation:

(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))
; ------------------------------------------------------------

; I will just define the machines.

; a) based n already given implementation in 5.1.scm we can similarly
; implement exponential.

(controller
  (assign continue (label expt-done))
 expt-loop
  (test (op =) (reg n) (const 0))
  (branch (label base-case))
  ; save current state and continue at after-expt
  (save continue)
  (save n)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-fact))
  (goto (label expt-loop))
 after-expt
  (restore n)
  (restore continue)
  (assign val (op *) (reg b) (reg val))
  (goto (reg continue))
 base-case
  (assign val (const 1))
  (goto (reg continue))
 expt-done)

; b) This is similar to the iterative machines we have already defined
; before.

(controller
 init
  (assign counter (read)) ; assuming read is a primitive
  (assign base (read))
  (assign product (const 1))
 expt-iter
  (test (op =) (reg counter) (const 0))
  (branch (label expt-done))
  (assign counter (op -) (reg counter) (const 1))
  (assign product (op *) (reg base) (reg product))
  (goto expt-iter)
 expt-done)
