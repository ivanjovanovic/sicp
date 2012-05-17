; Exercise 4.10.
;
; By using data abstraction, we were able to write an eval
; procedure that is independent of the particular syntax of the language to be
; evaluated. To illustrate this, design and implement a new syntax for Scheme
; by modifying the procedures in this section, without changing eval or apply.
; ------------------------------------------------------------


; I won't do implementation but rather explain how to do the change.
;
; On one side there is a very direct possibility to do trivial change of the names
; of the operators. For example changing `or` to `my-or` is ust a matter of
; updating the predicate for identifying the construct in the evaluator.
;
; (define (or? exp) (eq? (car exp) 'my-or?))
;
; How it is abstracted it is even possible to do the non-trivial changes like
; changing the signature  by not affecting the structure of the evaluator. In that
; case we would have to implement set of function that do evaluation expansion ...
; but we would not need to change the structure of the evaluator.
