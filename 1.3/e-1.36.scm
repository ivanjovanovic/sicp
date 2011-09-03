; Exercise 1.36.
;
; Modify fixed-point so that it prints the sequence of approximations
; it generates, using the newline and display primitives shown in exercise 1.22.
; Then find a solution to x^x = 1000 by finding a fixed point of x -> log(1000)/log(x).
; (Use Scheme's primitive log procedure, which computes natural logarithms.)
; Compare the number of steps this takes with and without average damping.
; (Note that you cannot start fixed-point with a guess of 1, as this would cause division by log(1) = 0.)
; ---------------------------------------------
;
; Updated fixed-point function

(load "../common.scm")

(define (fixed-point f first-guess delta)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next delta)
        next
        (begin (display next) (newline) (try next)))))
    (try first-guess))

; (fixed-point cos 1.0 0.0001)

; non-averaged fixed point convergense of x^x = 1000
; (display "--------- without averaging")
; (newline)

; (fixed-point
;   (lambda (x) (/ (log 1000) (log x)))
;   10.0
;   0.0001)

; Result for this convergence is:
; 2.9999999999999996
; 6.2877098228681545
; 3.7570797902002955
; 5.218748919675316
; 4.1807977460633134
; 4.828902657081293
; 4.386936895811029
; 4.671722808746095
; 4.481109436117821
; 4.605567315585735
; 4.522955348093164
; 4.577201597629606
; 4.541325786357399
; 4.564940905198754
; 4.549347961475409
; 4.5596228442307565
; 4.552843114094703
; 4.55731263660315
; 4.554364381825887
; 4.556308401465587
; 4.555026226620339
; 4.55587174038325
; 4.555314115211184
; 4.555681847896976
; 4.555439330395129
; 4.555599264136406
; 4.555493789937456


; convergence of averaged function
; (display "-------- averaging")
; (newline)

; (fixed-point
;   (lambda (x) (average x (/ (log 1000) (log x))))
;   10.0
;   0.001)

; Result is
;
; 6.5
; 5.095215099176933
; 4.668760681281611
; 4.57585730576714
; 4.559030116711325
; 4.55613168520593
