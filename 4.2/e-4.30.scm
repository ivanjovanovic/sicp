; Exercise 4.30.
;
; Cy D. Fect, a reformed C programmer, is worried that
; some side effects may never take place, because the lazy evaluator
; doesn't force the expressions in a sequence. Since the value of an
; expression in a sequence other than the last one is not used (the
; expression is there only for its effect, such as assigning to a
; variable or printing), there can be no subsequent use of this value
; (e.g., as an argument to a primitive procedure) that will cause it to
; be forced. Cy thus thinks that when evaluating sequences, we must
; force all expressions in the sequence except the final one. He
; proposes to modify eval-sequence from section 4.1.1 to use
; actual-value rather than eval:

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; a. Ben Bitdiddle thinks Cy is wrong. He shows Cy the for-each
; procedure described in exercise 2.23, which gives an important example
; of a sequence with side effects:

(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

; He claims that the evaluator in the text (with the original
; eval-sequence) handles this correctly:

;;; L-Eval input:
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
; 57
; 321
; 88
;;; L-Eval value:
; done

; Explain why Ben is right about the behavior of for-each.

; b. Cy agrees that Ben is right about the for-each example, but says
; that that's not the kind of program he was thinking about when he
; proposed his change to eval-sequence. He defines the following two
; procedures in the lazy evaluator:

(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

; What are the values of (p1 1) and (p2 1) with the original
; eval-sequence? What would the values be with Cy's proposed change to
; eval-sequence?

; c. Cy also points out that changing eval-sequence as he proposes does
; not affect the behavior of the example in part a. Explain why this is
; true.

; d. How do you think sequences ought to be treated in the lazy
; evaluator? Do you like Cy's approach, the approach in the text, or
; some other approach?
; ------------------------------------------------------------

; a) Ben is right because for-each in this case applies primitive procedures. Which are
;  applied on actual values
;
; b) values are (p1 1) -> (1 2) and for (p2 1) -> 1
;
; c) Yes, part a anyway works on actual values so nothing changes.
;
; d) I would like to have simple evaluation model that behaves consistently accross use cases.
;    Adding a specific case for the sequences introduces one more thing to think while
;    implementing programs and having to deal with when reasoning about the program.
