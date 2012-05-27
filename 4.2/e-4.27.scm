; Exercise 4.27.  Suppose we type in the following definitions to the
; lazy evaluator:

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

; Give the missing values in the following sequence of interactions, and
; explain your answers.38

(define w (id (id 10)))
;;; L-Eval input:
count
;;; L-Eval value:
<response>
;;; L-Eval input:
w
;;; L-Eval value:
<response>
;;; L-Eval input:
count
;;; L-Eval value:
<response>
;------------------------------------------------------------

; Responses are sequentially
; count -> 1
; w -> 10
; count -> 2

; The way id procedure is defined it will increment count by one
; and return the passed argument.
;
; During definition of the w, first call to apply id is evaluated as
; application, but internal one is not applied but is wrapped into thunk
; which is passed to outter call to id.
; So, after definition of the w, the value of w is the thunk created
; from (id 10) and outer id is evaluated which increased count for 1.
;
; Once w is asked for the actual value, it evaluates the thunk and
; returns 10, and increases count once more. That is why count goes to
; 2 after w is evaluated.
;
; If we evaluate w several more times, we can see that there is no
; increase of count anymore. That is because we have memoized the
; evaluation of the actual value of the thunk and therefore every next
; time only the value is returned without the evaluation of (id 10)
; which would increase the count every time.
;
; So, here we see the behavior of normal order evaluation, it applies
; procedure but doesn't evaluate its arguments until that is done
; intentionally.
