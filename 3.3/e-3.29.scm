; Exercise 3.29.
;
; Another way to construct an or-gate is as a compound digital
; logic device, built from and-gates and inverters. Define a procedure or-gate
; that accomplishes this. What is the delay time of the or-gate in terms of
; and-gate-delay and inverter-delay?
; ---------------------------------------

; Subtitution circuit for OR gate, made of AND and NOT gates can be see here:
; http://www.kpsec.freeuk.com/gates.htm#substituting
; I'm treating NAND as sequene of AND gate and inverter.
; NAND could be defined as separate element for the easier implementation but is not
; the most important thing here
;
; we can define it like this just by reusing already given definitions

(load "digital_circuit_simulator.scm")

(define (or-gate a b output)
  (let ((c (make-wire))
        (d (make-wire))
        (e (make-wire))
        (f (make-wire))
        (g (make-wire)))
    (and-gate a a c)
    (and-gate b b d)
    (inverter c e)
    (inverter d f)
    (and-gate e f g)
    (inverter g output)))

; The way I consider it (NAND as sequence of AND and NOT) the delay
; is sum of 2 AND circuits and 2 NOT circuits because every change of the
; signal has to pass through these 4 elements
