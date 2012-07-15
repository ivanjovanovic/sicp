; Exercise 4.57.  Define a rule that says that person 1 can replace
; person 2 if either person 1 does the same job as person 2 or someone
; who does person 1's job can also do person 2's job, and if person 1
; and person 2 are not the same person. Using your rule, give queries
; that find the following:

; a.  all people who can replace Cy D. Fect;

; b.  all people who can replace someone who is being paid more than
; they are, together with the two salaries.
; ------------------------------------------------------------

(rule (can-replace ?person-1 ?person-2)
      (and 
        (or 
          (and (job ?person-1 ?person-1-job)
               (job ?person-2 ?person-1-job))
          (and (job ?person-1 ?person-1-job)
               (job ?person-2 ?person-2-job)
               (can-do-job ?person-2-job ?person-1-job)))
        (lisp-value neq? ?person-2-name ?person-1-name)))


; a) (can-replace (Cy D. Fect) ?replacment)
;
; b) (and 
;       (can-replace ?someone ?replacement)
;       (salary ?someone ?someone-salary)
;       (salary ?replacement ?replacement-salary)
;       (lisp-value > ?someone-salary ?replacement-salary))
