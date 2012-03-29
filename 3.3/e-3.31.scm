; Exercise 3.31.
;
; The internal procedure accept-action-procedure! defined in
; make-wire specifies that when a new action procedure is added to a wire, the
; procedure is immediately run. Explain why this initialization is necessary.
; In particular, trace through the half-adder example in the paragraphs above
; and say how the system's response would differ if we had defined
; accept-action-procedure! as
;
; (define (accept-action-procedure! proc)
;   (set! action-procedures (cons proc action-procedures)))
;
; ------------------------------------------------------------

; Currently accept-action-procedure! is defined as
(define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

; which is executing the added procedure immediately.
; On the example of half adder we can see what is the difference in the response
; when it is not executed immediately.
;
; The point is that if we don't call it in the point of time we add it to the list
; of actions we will not add it to agenda. In that case we will add
; it to the agenda only when we call (propagate) procedure which will produce
; different results. In fact if we don't do it, no procedures will be in the agenda at all, so
; our simulation will just not run.
