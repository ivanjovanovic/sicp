; Exercise 3.32.
;
; The procedures to be run during each time
; segment of the agenda are kept in a queue. Thus, the procedures
; for each segment are called in the order in which they were
; added to the agenda (first in, first out). Explain why this
; order must be used. In particular, trace the behavior of an
; and-gate whose inputs change from 0,1 to 1,0 in the same
; segment and say how the behavior would differ if we stored a
; segment's procedures in an ordinary list, adding and removing
; procedures only at the front (last in, first out).
; ------------------------------------------------------------

; If we look at the implementation more closely, and gate ads one action per
; wire.
;
; wire a1 gets an action and wire a2 gets an action. When there is a change in
; the input signal, these two actions are producing certain events to happen
; after some delay.  If we use LILO approach of the queue, these actions are
; set in a proper order of execution and the end result is OK, otherwise we get
; the wrong result.
;
; Preciselly explained in comment: http://eli.thegreenplace.net/2007/10/08/sicp-section-334/#comment-124196
;
; set-signal! will add some segments to the-agenda, and the signal of z will
; change only after (propagate) is done. So that after s1, something like
; (set-signal! z 1) will be added to the-agenda, and s2 will add (set-signal! z
; 0). Here is the difference: if the LIFO queue is used, (propagate) will run
; (set-signal! z 0) first instead of (set-signal! z 1).
