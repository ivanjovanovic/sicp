(load "../helpers.scm")
; basic constrainst that we could define are mathematical
; (multiplier a b c) ; holds a b and c in relation a*b = c
; (adder a b c) ; holds the relation a + b = c
; (constant a b) ; holds the relation a = b
;
; One concrete example of the constraint network is given
; for the relation between Celisus and Fahreheit quantities
; 9*C = 5*(F - 32) here.
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_fig_3.28

; we can implement the constraint network in the following way

; (define C (make-connector))
; (define F (make-connector))
; (celsius-fahrenheit-converter C F)

; like in the digital circuit networks, we build our network by 'wireing' things up.
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

; abstractions of a constraint system are
; multiplier
; adder
; constant
;
; Connection between constraints is defined through abstraction
; connector
;
; We have to other notions which are just abstractions of some
; entity that is doing requests, they are defined as
; retractor
; informant

; First implementing the constraints

; adder
(define (adder a1 a2 sum)

  ; implementing the constraint for the change of any value
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))

  ; deleting all values
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  ; message dispatcher
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- ADDER" request))))

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

; wrapper procedures to send proper messages to the constraints
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

; multiplier is very similar to the adder. It just maintains the
; relation between in and out connections regarding the multiplication
; operation
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

; constant is the easy one
(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

; probe
(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define false '#f)
(define true '#t)

; at the end we have to represent connectors with constructor, selectors and modifiers
(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))

    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin (set! informant false)
               (for-each-except retractor
                                inform-about-no-value
                                constraints))
        'ignored))

    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)

    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" request))))
    ; returning the dispatch method me
    me))

; helper method
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

; convenience
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

; examples to see how this works

; (define C (make-connector))
; (define F (make-connector))
; (celsius-fahrenheit-converter C F)

(define a1 (make-connector))
(define a2 (make-connector))
(define sum (make-connector))

(adder a1 a2 sum)
(set-value! a1 5 'ivan)
(set-value! a2 7 'ivan)
; (output (get-value sum))
