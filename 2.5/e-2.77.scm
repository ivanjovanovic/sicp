(load "../common.scm")
(load "2.5.scm")

; Exercise 2.77.
;
; Louis Reasoner tries to evaluate the expression
; (magnitude z) where z is the object shown in figure 2.24. To his
; surprise, instead of the answer 5 he gets an error message from
; apply-generic, saying there is no method for the operation magnitude
; on the types (complex). He shows this interaction to Alyssa P. Hacker,
; who says ``The problem is that the complex-number selectors were never
; defined for complex numbers, just for polar and rectangular numbers.
; All you have to do to make this work is add the following to the
; complex package:''

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

; Describe in detail why this works. As an example, trace through all
; the procedures called in evaluating the expression (magnitude z) where
; z is the object shown in figure 2.24. In particular, how many times is
; apply-generic invoked? What procedure is dispatched to in each case?
; ------------------------------------------------------------

; as stated in the exercise, it just wordks.
(output (magnitude (make-complex-from-real-imag 3 4))) ; 5

; Lets look what is behind

; Call to (magnitude (make-complex-from-real-imag 3 4))
; first creates a complex number in a rectangular form
; (complex rectangular 3 4)

; Then it applies procedure magnitude (magnitude z) which is defined as
; (define (magnitude z) (apply-generic 'magnitude z))

; apply generic resolves this in the way that it pulls out of the
; operations table procedure to be applied for the keys
; 'magnitude 'complex
; This turns out to be the function we have just defined in our
; exercise. You can check that with this
; (output (get 'magnitude '(complex)))

; That one is applied now to our parameter z. And that one is defined as
; generic one on top of either rectangular or polar by installation of
; the rectangular and polar packages.
;
; So here we have again apply-generic that now resolves to a 
; procedure magnitude-rectangular which is then applied to our number
; rectangular which gives results
