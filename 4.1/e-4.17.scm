; Exercise 4.17.
;
; Draw diagrams of the environment in
; effect when evaluating the expression <e3> in the
; procedure in the text, comparing how this will be
; structured when definitions are interpreted
; sequentially with how it will be structured if
; definitions are scanned out as described. Why is there
; an extra frame in the transformed program? Explain why
; this difference in environment structure can never make
; a difference in the behavior of a correct program.
; Design a way to make the interpreter implement the
; ``simultaneous'' scope rule for internal definitions
; without constructing the extra frame.
; ------------------------------------------------------------

(load "e-4.16.scm")

; Extra frame appears because applying let means doing an application
; of the ((lambda (t u) (<body>)) t-val u-val) which constructs the environment.

; It is not making a difference because this is just one more wrapper local frame.
;
; if we, instead of scanning out the definitions, just put them on top, that will ensure
; that all of them are automatically defined in the environment once their application is
; issued. I can rewrite scan-out-defines

(define (scan-out-defines body)
  (append (filter (lambda (element) (eq? (car element) 'define)) body)
          (filter (lambda (element) (not (eq? (car element) 'define))) body)))


(define input '(lambda ()
                 (define t (+ 3 2))
                 (display "test")
                 (define p (car (cons 1 2)))))

;(output (scan-out-defines (cddr input)))
