; Exercise 2.16.
;
; Explain, in general, why equivalent algebraic expressions may lead to
; different answers. Can you devise an interval-arithmetic package that
; does not have this shortcoming, or is this task impossible?
; (Warning: This problem is very difficult.)
; --------------------------------------------------------
;
; As explained in previous exercise, it is because different complexity
; of algebraic expression and number of times inprecise number has to be
; used as operand. Every time it is used it accumulates error.
;
; I suppose this can be fixed a bit by doing symbolic manipulation and
; simplification of the equations until they are in some appropriate
; form for most possible pricise calculations.
