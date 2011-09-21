; Exercise 2.15.
;
; Eva Lu Ator, another user, has also noticed the different intervals computed by different but algebraically
; equivalent expressions. She says that a formula to compute with intervals using Alyssa's system will produce
; tighter error bounds if it can be written in such a form that no variable that represents an uncertain number
; is repeated. Thus, she says, par2 is a ``better'' program for parallel resistances than par1. Is she right? Why?
; --------------------------------------------
;
; For reference to this explanation look at the previous exercise and
; results of running it.
;
; I suppose she is right since second version produces result which has
; smaller uncertainity width. Not getting into mathematical explanations
; of the reasons I think that every arithmetic operation, especially
; multiplications, will introduce error accumulation and thus as
; complexity of the equation grows I would expect that precision falls
; down even when expressions are mathematicaly equivalent. 
;
; I'm not sure about the number repeating in the equation but I suppose
; it is part of the "equation complexity" reason?
