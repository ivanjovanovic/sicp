; Exercise 4.55.  Give simple queries that retrieve the following
; information from the data base:

; a. all people supervised by Ben Bitdiddle;
; b. the names and jobs of all people in the accounting division;
; c. the names and addresses of all people who live in Slumerville.
; -------------------------------------------------------------

; a) (supervisor (Ben Bitdiddle) ?name)
;
; b) (job ?name (accounting . ?skills))
;
; c) (address ?name (Slumerville . ?address))
