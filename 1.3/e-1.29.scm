; Exercise 1.29.  
;
; Simpson's Rule is a more accurate method of numerical integration than the method
; illustrated above. Using Simpson's Rule, the integral of a function f between a and b is approximated as
;
; S|a,b of f =  h/3[y0 + 4y1 + 2y2 + 4y3 + 2y4 + ... + 2yn-2 + 4yn-1 + yn]
;
; where h = (b - a)/n, for some even integer n, and yk = f(a + kh). 
; (Increasing n increases the accuracy of the approximation.) 
; Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, 
; computed using Simpson's Rule. Use your procedure to integrate cube between 0 and 1 
; (with n = 100 and n = 1000), and compare the results to those of the integral procedure shown above.
; ---------------------------------

; Here we see one bit of a difference comparing to already defined method which produces sums. 
; Here, depending on the value of 4 term can be multiplied by 2 or by 4 or by 1 fo special cases of 0 and n.
; Therefore we need modified sum that gets these cases.

(load "../common.scm")
(load "1.3.scm")


; I introduce new value counter which will just count in which iteration are we, and I pass final value n
(define (simpsons-sum term a next b k n)
  (cond ((> a b ) 0)
        ((or (= k 0) (= k n))
          (+
            (term a)
            (simpsons-sum term (next a) next b (+ k 1) n)))
        ((even? k)
          (+
            (* 2 (term a))
            (simpsons-sum term (next a) next b (+ k 1) n)))
        (else
          (+
            (* 4 (term a))
            (simpsons-sum term (next a) next b (+ k 1) n)))))

; Now definition of the integral and helper function

(define (integral-1 f a b n)
  (define h (/ (- b a) n))
  (define (next a) (+ a h))
  (* (/ h 3.0) (simpsons-sum f a next b 0 n)))

; (display (integral cube 0 1 97))
; (newline)

; (display (integral cube 0 1 200))
; (newline)


; But ................
;
; Original idea was to reuse already defined procedure sum, which is not obvious from the 
; first glance on the problem. If we recompose a bit the problem to be
;
; S|a,b of f =  h/3[y0 + 4*SUM|1,n/2(y2k-1) + 2*SUM|1,n/2-1(y2k) + yn]
;
; Then we can reuse the sum, although we have to call sum two times with changed a and b values.
; @see http://en.wikipedia.org/wiki/Simpson's_rule

(define (integral-2 f a b n)
  (define h (/ (- b a) n))
  (define (next x) (+ x (* 2 h)))
  (*
    (/ h 3)
    (+
      (f a)
      (* 4 (sum f (+ a h) next (- b h)))
      (* 2 (sum f (+ a (* h 2)) next (- b (* 2 h))))
      (f b))))

(display (integral-2 cube 0 1 100))
(newline)

; After executing we can see that this method converges much faster and for
; n = 100 (in 100 iterations) give 0.25 as precise result while method in 1.3.scm
; converges slowly towardssolution and even after 1000 iterations is not yet to 0.25 exactly.
;
; Giving these two solutions can show how powerful can abstraction be and how much it can reduce
; and organize code in a proper way in order to express the true knowledge captured in the domain of the problem.
; Of course, the problem has to be set properly as well. In the book Simpson's rule was given in a way that misleads you if
; you don't figure out the way to organize equation so it shows clearly how to reuse the abstraction. If you don't
; set it properly you end up writing new SUM procedure and introducing complexity where it is not needed to be 
; introduced. Otherwise you just reuse what is given. According to the 
; "Good coders code, great reuse" :)

