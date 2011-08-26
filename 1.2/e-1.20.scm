; TODO: Check if solution to this is correct. When printing from the
;       remainder procedure it reports as it was called only 4 times in
;       both ways of application.
;
; Exercise 1.20.
;
; The process that a procedure generates is of course dependent on the rules used by the interpreter.
; As an example, consider the iterative gcd procedure given below.
; Suppose we were to interpret this procedure using normal-order evaluation,
; as discussed in section 1.1.5. (The normal-order-evaluation rule for if is described in exercise 1.5.).
; Using the substitution method (for normal order), illustrate the process generated in evaluating (gcd 206 40)
; and indicate the remainder operations that are actually performed.
; How many remainder operations are actually performed in the normal-order evaluation of (gcd 206 40)? In the applicative-order evaluation?
;
; Iterative procedure for calculating GCD of two numbers
;
; (define (gcd a b)
;   (if (= b 0)
;       a
;       (gcd b (remainder a b))))
;
;----------------------------------------------
;
; Normal order evaluation:
;   In this type of evaluation we do not evaluate params of the
;   procedures, substitution is taken but evaluation is done only when
;   is needed by some primitive operation (i.e (= b 0) will evaluate b)
;
; So execution would look like this
;
; (gcd 206 40)
;
; evaluate condition
; (if (= 40 0) 0) -> false
;
; next iteration
; (gcd 40 (remainder 206 40))
;
; evaluate condition
; (if (= (remainder 206 40) 0) a) -> evaluates to 6 with 1 remainder call
;
; next iteration
; (gcd
;     (remainder 206 40)
;     (remainder 40 (remainder 206 40)))
;
; evaluate condition
; (if (= (remainder 40 (remainder 206 40)) 0) a) -> evaluates to 4 with 2 reminder calls
;
; next iteration
; (gcd
;     (remainder
;         40
;         (remainder 206 40))
;     (remainder
;         (remainder 206 40)
;         (remainder 40 (remainder 206 40))))
;
; evaluating condition
; (if (=
;       (remainder
;           (remainder 206 40)
;           (remainder 40 (remainder 206 40)))
;       0) a) -> evaluating to 2 with 4 calls to remainder
;
; next iteration
; (gcd
;     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;     (remainder
;         (remainder
;             40
;             (remainder 206 40))
;         (remainder
;             (remainder 206 40)
;             (remainder 40 (remainder 206 40)))))
;
; evaluating condition
; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) -> evaluating to 0
;        (remainder
;             (remainder 206 40)
;             (remainder 40 (remainder 206 40)))) -> since it is true we have to evaluate as well result
;
;
; So we have here several places where execution needed to do the
; evaluation. 4 times to chech the condition and last time it had to
; evaluate result as well
;
; Number of executions 1 + 2 + 4 + 7 + 4 = 18
