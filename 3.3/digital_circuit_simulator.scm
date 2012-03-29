; Digital circuits consist of basic elements like inverter, and-gate, or-gate
; ... which are connected by wires to form more complex elements like half-adder ad similar

; We can define the half-adder circuit @see Figure 3.25 by constructing the elements


(define (make-wire) 1)

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

; we construct basic digital circuits and we provide wires that connect to them

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a b)
  (cond ((or (= a 1) (= b 1)) 1)
        (else 0)))

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
  (add-action! input invert-input)
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

; Representing wires

; Wire is an object that has local state represented by two values.
; signal-value and action-procedures (list of actions to be taken on the change of the signal value)
; We can model is with the message passing style
;
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

; Inside this object we see a procedure that calls all the action-procedures from the list
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

; we can define some syntactic sugar for accessing the methods of the wire object easier

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; In order to finish the simulation we need some kind of driving force for the whole system.
; Agenda defines how time passes in the system.
;
; Agenda is defined as a data structure with abstraction defined by these procedures

(make-agenda) ; returns a new empty agenda.
(empty-agenda? <agenda>) ; is true if the specified agenda is empty.
(first-agenda-item <agenda>) ; returns the first item on the agenda.
(remove-first-agenda-item! <agenda>) ; modifies the agenda by removing the first item.
(add-to-agenda! <time> <action> <agenda>) ; modifies the agenda by adding the given action procedure to be run at the specified time.
(current-time <agenda>) ; returns the current simulation time.

; Agenda just holds list of next actions to be executed at the crtain point of time

; procedure after-delay adds things to agenda to be done
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

; Simulation happens by execution of procedure (propagate) which executes elements from the agenda.


; Example of simulation by probing the wires
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

; We begin by initializing the agenda and specifying delays for the primitive function boxes:

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; Now we define four wires, placing probes on two of them:

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)

; at the end we add the agenda data structure procedures

; we implement agenda as the queue of time segments which are abstracted by following constructor
; and selectors

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

; The agenda itself is a list (one-dimensional table) of time segments sorted in order of increasing times.
; It is defined by the following datastructure abstraction
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments-agenda)))

; adding to agenda with maintaining the premises of order we defined for this data structure
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time queue)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
                     action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
          (set-cdr!
            segments
            (cons (make-new-time-segment time action)
                  (cdr segments)))
          (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments!
        agenda
        (cons (make-new-time-segment time action)
              segments))
      (add-to-segments! segments))))

; removing first item from the agenda

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

; finding first agenda item

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))
