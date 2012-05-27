; Exercise 4.26.
;
; Ben Bitdiddle and Alyssa P. Hacker disagree over the
; importance of lazy evaluation for implementing things such as unless. Ben
; points out that it's possible to implement unless in applicative order as a
; special form. Alyssa counters that, if one did that, unless would be merely
; syntax, not a procedure that could be used in conjunction with higher-order
; procedures. Fill in the details on both sides of the argument. Show how to
; implement unless as a derived expression (like cond or let), and give an
; example of a situation where it might be useful to have unless available as a
; procedure, rather than as a special form.
; ------------------------------------------------------------

; lets say we have procedure decider which can be specialized for optimistic or
; pessimistic approach to decisions. We can specialize it by passing the decision
; making function. The simples ones are if and unless.

;(define (decider deciding-strategy)
  ;(lambda ()
    ;(deciding-strategy <condition>
      ;<consequent>
      ;<alternative>)))

;(define optimist
  ;(decider if))

;(define pesimist
  ;(decider unless))

; if unless would be implemented as the special syntax form, then it would
; not be procedure object that is first-order value in the evaluation process and thus
; could not be passed to other functions.


; we can implement unless as derived from if?

(load "../helpers.scm")
(load "../4.1/4.1.scm")

; unoptimized evaluation approach
(define (unless? exp)
  (tagged-list? exp 'unless))

(define (eval-unless exp env)
  (eval (unless->if exp) env))

(define (unless->if-1 exp) (expand-unless-1 (cdr exp)))
(define (unless->if-2 exp) (expand-unless-2 (cdr exp)))

(define (unless-condition operands) (car operands))
(define (unless-consequent operands) (cadr operands))
(define (unless-alternative operands) (caddr operands))

; one way to do it is to revert the predicate with logical not
(define (expand-unless-1 operands)
  (make-if (list 'not (unless-condition operands))
           (unless-consequent operands)
           (unless-alternative operands)))

; other way is to revert the order of consequent and alternative
(define (expand-unless-2 operands)
  (make-if (unless-condition operands)
           (unless-alternative operands)
           (unless-consequent operands)))

(output (unless->if-1 '(unless (= a b) c d)))
(output (unless->if-2 '(unless (= a b) c d)))


; when defined as syntax rule

(define-syntax unless
  (syntax-rules ()
    ((unless predicate consequence alternative)
     (if predicate alternative consequence))))

(output (unless (= 2 3) 4 5))
