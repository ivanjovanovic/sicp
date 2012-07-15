; Exercise 4.56.  Formulate compound queries that retrieve the following
; information:

; a. the names of all people who are supervised by Ben Bitdiddle,
; together with their addresses;

; b. all people whose salary is less than Ben Bitdiddle's, together with
; their salary and Ben Bitdiddle's salary;

; c. all people who are supervised by someone who is not in the computer
; division, together with the supervisor's name and job.
; ------------------------------------------------------------

; a) (and (supervisor (Ben Bitdiddle) ?name)
;         (address ?name ?address)
;
; b) (and (salary (Ben Bitdiddle) ?ben-amount)
;         (salary ?name ?amount)
;         (lisp-value < ?amoun ?ben-amount)
;         (address ?name ?address))
;
; c) (and (superviser ?superviser ?name)
;         (not (job ?superviser (computer . ?job))))
