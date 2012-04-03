; Exercise 3.38.  Suppose that Peter, Paul, and Mary share a joint bank
; account that initially contains $100. Concurrently, Peter deposits
; $10, Paul withdraws $20, and Mary withdraws half the money in the
; account, by executing the following commands:

; Peter:  (set! balance (+ balance 10))
; Paul:   (set! balance (- balance 20))
; Mary:   (set! balance (- balance (/ balance 2)))

; a. List all the different possible values for balance after these
; three transactions have been completed, assuming that the banking
; system forces the three processes to run sequentially in some order.

; b. What are some other values that could be produced if the system
; allows the processes to be interleaved? Draw timing diagrams like the
; one in figure 3.29 to explain how these values can occur.
; ------------------------------------------------------------

; a) These events can occur in any order, so we have number of
; permutations of the set to evaluate. Lets say we have three events
; Pe, Pa and Ma. All permutations in which they can appear are
;
; balance = 100 initially
; Pe = +10
; Pa = -20
; Ma = /2
;
; (Pe, Pa, Ma) => ((100 + 10) - 20) / 2 = 45
; (Pe, Ma, Pa) => ((100 + 10) / 2) - 20 = 35
; (Pa, Pe, Ma) => ((100 - 20) + 10) / 2 = 45
; (Pa, Ma, Pe) => ((100 - 20) / 2) + 10 = 50
; (Ma, Pa, Pe) => 100/2 -20 + 10        = 40
; (Ma, Pe, Pa) => 100/2 + 10 - 20       = 40
;
; b) Here we have complex situation of three process which all have
; (set!) procedure application that executes in several steps, with all
; the permutatations we an draw some diagrams easily.
