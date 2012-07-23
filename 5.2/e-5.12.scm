; Exercise 5.12.  The simulator can be used to help determine the data
; paths required for implementing a machine with a given controller.
; Extend the assembler to store the following information in the machine
; model:

; a list of all instructions, with duplicates removed, sorted by
; instruction type (assign, goto, and so on);

; a list (without duplicates) of the registers used to hold entry points
; (these are the registers referenced by goto instructions)

; a list (without duplicates) of the registers that are saved or restored;

; for each register, a list (without duplicates) of the sources from
; which it is assigned (for example, the sources for register val in the
; factorial machine of figure 5.11 are (const 1) and ((op *) (reg n) (reg val))).


; Extend the message-passing interface to the machine to provide access
; to this new information. To test your analyzer, define the Fibonacci
; machine from figure 5.12 and examine the lists you constructed.
; ------------------------------------------------------------

; we can easily extend the assembler to analyze the instruction sequence
; after the instruction list was gathered from the extract-labels
; procedure, but before it is updated with the execution procedures.

(load "5.2.scm")
(load "../2.2/2.2.scm") ; for the filter

(require-extension srfi-13)

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (set-instruction-analysis-info! insts labels machine)
                    (update-insts! insts labels machine)
                    insts)))

; we'll have to put analysis information into the machine

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-analysis-info '())) ; holds the information about the analysis of the sequence
    (let ((the-ops
            (list (list 'initialize-stack
                         (lambda () (stack 'initialize)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))

      ; allocation of the new register object with the given name
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)

      ; get the value of the register
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register: " name))))

      ; run the machine
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              ((instruction-execution-proc (car insts)))
              (execute)))))

      ; external interface
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'set-instruction-analysis-info)
               (lambda (info) (set! instruction-analysis-info info)))
              ((eq? message 'get-instruction-analysis-info) instruction-analysis-info)
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


; here we do analysys on the instruction sequence
; and then we update the machine directly with the analysys information
(define (set-instruction-analysis-info! insts labels machine)
  (let* ((insts (map (lambda (inst) (car inst)) insts))
         (used-instructions (collect-used-instructionsins insts))
         (entry-point-registers (collect-entry-point-registers insts))
         (stack-involved-registers (collect-stack-involved-registers insts)))
    ((machine 'set-instruction-analysis-info)
              (make-instruction-info
                used-instructions
                entry-point-registers
                stack-involved-registers))))

(define (make-instruction-info used-instructions entry-point-registers stack-involved-registers)
  (list used-instructions entry-point-registers stack-involved-registers))

(define (get-used-instructions instruction-info)
  (car instruction-info))
(define (get-entry-point-registers instruction-info)
  (cadr instruction-info))
(define (get-stack-involved-registers instruction-info)
  (caddr instruction-info))

; this is more imperative style, other would be
; to map repetitions to nil elements and then filter them out.
; Other is to use for-each of instructions do this or that.
(define (collect-used-instructionsins insts)
  (let ((instructions '()))
    (map
      (lambda (inst)
        (output inst)
        (if (not (member (symbol->string (car inst)) instructions))
          (set! instructions (cons (symbol->string (car inst)) instructions))))
      insts)
    (sort instructions string<)))

(define (goto-instruction? inst)
  (eq? (car inst) 'goto))

; collect registers
(define (collect-entry-point-registers insts)
  (let ((registers '()))
    (map
      (lambda (inst)
        (if (eq? (car (goto-dest inst)) 'reg)
          (set! registers (cons (cdr (goto-dest inst)) registers))))
      (filter (lambda (inst) (goto-instruction? inst)) insts))
    registers))


(define (stack-related-instruction? inst)
  (or (eq? (car inst) 'save) (eq? (car inst) 'restore)))

; collect stack involved registers
(define (collect-stack-involved-registers insts)
  (let ((registers '()))
    (map
      (lambda (inst)
          (set! registers (cons (cadr inst) registers)))
      (filter (lambda (inst) (stack-related-instruction? inst)) insts))
    registers))


; I will use already defined definition of the machine in the exercise
; 5.7

(define expt-machine
  (make-machine
    '(counter base product)
    (list (list '- -) (list '* *) (list '= =))
    '(init
       (assign counter (const 3)) ; assuming read is a primitive
       (assign base (const 5))
       (assign product (const 1))
      expt-iter
       (save counter)
       (restore counter)
       (save base)
       (restore base)
       (test (op =) (reg counter) (const 0))
       (branch (label expt-done))
       (assign counter (op -) (reg counter) (const 1))
       (assign product (op *) (reg base) (reg product))
       (goto (label expt-iter))
      expt-done)))

(start expt-machine)
(output (expt-machine 'get-instruction-analysis-info))
