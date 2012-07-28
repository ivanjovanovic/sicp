(load "common.scm")

;;----------------------------------------------------
;;
;; Top-level interface to the register-machine simulator.
;;
;; > (make-machine <register-names> <operations> <controller>)
;;  constructs and returns a model of the machine with the given 
;;  registers, operations, and controller.
;; 
;; > (set-register-contents! <machine-model> <register-name> <value>)
;;  stores a value in a simulated register in the given machine.
;; 
;; > (get-register-contents <machine-model> <register-name>)
;;  returns the contents of a simulated register in the given machine.
;; 
;; > (start <machine-model>)
;;  simulates the execution of the given machine, starting from 
;;  the beginning of the controller sequence and stopping when 
;;  it reaches the end of the sequence. 
;;
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each 
      (lambda (register-name)
        ((machine 'allocate-register) register-name))
      register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
      (assemble controller-text machine))
    machine))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (start machine)
  (machine 'start))

;;----------------------------------------------------
;; Registers
;; 
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set) 
              (lambda (value) (set! contents value)))
            (else
              (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;;----------------------------------------------------
;; Stack
;; 
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
        (error "Empty stack -- POP")
        (let ((top (car s)))
          (set! s (cdr s))
          top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;;----------------------------------------------------
;; Machine model
;; 
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table 
                (cons (list name (make-register name))
                      register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              ((instruction-execution-proc (car insts)))
              (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) 
                (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;;----------------------------------------------------
;; Assembler
;; 

; Note: using the alternative implementation of extract-labels,
; proposed in footnote 4 of section 5.5.2, because it's easier
; to understand
;
(define (assemble controller-text machine)
  (let ((result (extract-labels controller-text)))
    (let ((instructions (car result))
          (labels (cdr result)))
      (update-instructions! instructions labels machine)
      instructions)))

(define (extract-labels text)
  (if (null? text)
    (cons '() '())
    (let ((result (extract-labels (cdr text))))
      (let ((instructions (car result))
            (labels (cdr result)))
        (let ((next-instruction (car text)))
          (if (symbol? next-instruction) ; a label ?
            (if (label-exists labels next-instruction)
              (error "Label name is duplicated: " next-instruction)
              (cons instructions
                    (cons (make-label-entry next-instruction instructions) labels)))
            (cons (cons (make-instruction next-instruction) instructions)
                  labels)))))))

(define (update-instructions! instructions labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each 
      (lambda (instruction)
        (set-instruction-execution-proc!
          instruction
          (make-execution-procedure
            (instruction-text instruction)
            labels machine pc flag stack ops)))
      instructions)))

(define (make-instruction text)
  (cons text '()))
(define (instruction-text instruction)
  (car instruction))
(define (instruction-execution-proc instruction)
  (cdr instruction))
(define (set-instruction-execution-proc! instruction proc)
  (set-cdr! instruction proc))

(define (make-label-entry label-name instructions)
  (cons label-name instructions))
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
      (cdr val)
      (error "Undefined label -- ASSEMBLE" label-name))))
(define (label-exists labels label-name)
  (assoc label-name labels))

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
          (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
          (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
          (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
          (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
          (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
          (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
          (make-perform inst machine labels ops pc))
        (else 
          (error "Unknown instruction type -- ASSEMBLE" inst))))

;;----------------------------------------------------
;; Instructions of the assembler
;; 
(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-assign inst machine labels operations pc)
  (let ((target
          (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
            (if (operation-exp? value-exp)
              (make-operation-exp
                value-exp machine labels operations)
              (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda () ; the execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
      (let ((condition-proc
              (make-operation-exp
                condition machine labels operations)))
        (lambda ()
          (set-contents! flag (condition-proc))
          (advance-pc pc)))
      (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

;;----------------------------------------------------
;; Execution procedures for subexpressions
;; 

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
          (let ((c (constant-exp-value exp)))
            (lambda () c)))
        ((label-exp? exp)
          (let ((insts 
                  (lookup-label labels (label-exp-label exp))))
            (lambda () insts)))
        ((register-exp? exp)
          (let ((r (get-register 
                    machine
                    (register-exp-reg exp))))
            (lambda () (get-contents r))))
        (else
          (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (label-exp? e)
                  (error "Using operation on label: " e)
                  (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))







