; Exercise 2.12.
; Define a constructor make-center-percent that takes a center and a percentage 
; tolerance and produces the desired interval. You must also define a selector 
; percent that produces the percentage tolerance for a given interval. The center 
; selector is the same as the one shown above.
;--------------------------------------------------  
;
;
(load "2.1.scm")
(load "e-2.7.scm")


(define (make-center-percent c p)
  (let ((np (/ p 100))) ; define normalized percent
    (make-interval 
      (* c (- 1 np))
      (* c (+ 1 np)))))

(define (percent i)
  (* 100.0 
     (/ 
       (- (upper-bound i) (center i))
       (center i))))

; now lets test a bit this
(display (percent (make-center-percent 10 10))) ; returns 10
(newline)

(display (percent (make-center-width 10 1))) ; returns 10
(newline)
