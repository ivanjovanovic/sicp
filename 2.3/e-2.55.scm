; Exercise 2.55.
;
; Eva Lu Ator types to the interpreter the expression

; (car ''abracadabra)

; To her surprise, the interpreter prints back quote. Explain.
;------------------------------------------------------------

; apostrophe is just symbol representing the procedure (quote), so when we expand this
; we'll get something like

(car (quote (quote abasdfa)))

; which is

(car '(quote asfasd))

; whose output is as stated

(display (car '(quote asdfasdf))) ; > quote
