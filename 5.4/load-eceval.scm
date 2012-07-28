;;;; LOADS THE EXPLICIT-CONTROL EVALUATOR FROM SECTION 5.4 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS, WITH
;;;; ALL THE SUPPORTING CODE IT NEEDS IN ORDER TO RUN.

;;;; **NB** The actual "load" calls are implementation dependent.

(load "ch5-regsim")			;reg machine simulator

;; **NB** next file contains another "load"
(load "ch5-eceval-support")		;simulation of machine operations

(load "ch5-eceval-original")			;eceval itself



; start the machine and define what global environment means
(load "../common.scm")
(define the-global-environment (setup-environment))
(start eceval)
