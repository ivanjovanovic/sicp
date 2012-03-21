; Digital circuits consist of basic elements like inverter, and-gate, or-gate
; ... which are connected by wires to form more complex elements like half-adder ad similar

; We can define the half-adder circuit @see Figure 3.25 by constructing the elements

(load "e-3.28.scm") ; or-gate definition

(define (make-wire) 1)

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

; we construct basic digital circuits and we provide wires that connect to them

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)

; half adder can be constructed by wiring basic elements

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; and full adder as well by combining half adders

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


; our simulator provides us language for defining circuits where every element of the language
; correspodents to the elementary box in the iagram of the circuit.

; We need some "forces" that will make changes in our system. We define possibility to read from the
; wire, to set new value on the wire and to do some action on change of the state on the wire.

; (get-signal <wire>)
; (set-signal! <wire> <new value>)
; (add-action! <wire> <procedure of no arguments>)

; we'll define delay function to simulate delay of eleectronic device
; (after-delay <procedure>)

; now we can define basic elements

; inverter that takes input signal and sets
; inverted signal to the output
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

; and gate is a bit more complex because it has two inputs and one output
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  ; adding actions that will be executed on signal change on the wires a1 and a2
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

