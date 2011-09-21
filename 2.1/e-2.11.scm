; Exercise 2.11.
; In passing, Ben also cryptically comments: ``By testing the signs of the endpoints of the intervals, 
; it is possible to break mul-interval into nine cases, only one of which requires more than two 
; multiplications.'' Rewrite this procedure using Ben's suggestion.
; --------------------------------------------------------------
;
; Implementation of this needs separating all different combinations for different signs
; of upper and lower bounds that produce unique results. Only situation when lower bounds are positive and 
; upper bounds are negative in both itervals is the one that needs more than 2 multiplication. Otherwise it is
;
; First lets say how multiplication is defined;
;
; [a,b] x [c,d] = [ MIN { (a*c), (a*d), (b*c), (b*d) }, MAX { (a*c), (a*d), (b*c), (b*d) } ]
;
; where we have to be aware that structure of interval is such that a < b and c < d.
;
; Based on this we can evaluate different combinations and see what we need to exactly calculate.
; There are 9 combinations that satisfy definition of how intervals are structured.
;
; 1) a > 0, b > 0, c > 0, d > 0
; --------------------------------------
;  MIN = a*c
;  MAX = b*d
;
; 2) a > 0, b > 0, c < 0, d > 0
; ---------------------------------------
; MIN = b*c
; MAX = b*d
;
; 3) a > 0, b > 0, c < 0, d < 0
; ---------------------------------------
; MIN = b*d
; MAX = a*c
;
; 4) a < 0, b > 0, c > 0, d > 0
; --------------------------------------
;  MIN = a*d
;  MAX = b*d
;
; 5) a < 0, b > 0, c < 0, d > 0
; ---------------------------------------
; Here, it depends on the numbers and it can't be
; just said since it has to be chosen between
; 
; a * c > 0
; a * d < 0 ; not sure if this one is minimum
; b * c < 0 ; or this one is minimum maybe?
; b * d > 0 ; clearly maximum
;
; we will anyway have to compute at least three multiplications
;
; 6) a < 0, b > 0, c < 0, d < 0
; ---------------------------------------
; MIN = b*d
; MAX = a*d
;
; 7) a < 0, b < 0, c > 0, d > 0
; --------------------------------------
;  MIN = b*d
;  MAX = a*c
;
; 8) a < 0, b < 0, c < 0, d > 0
; ---------------------------------------
; MIN = b*d
; MAX = b*c
;
; 9) a < 0, b < 0, c < 0, d < 0
; ---------------------------------------
; MIN = a*c
; MAX = b*d
;
;
; After all of these computations it is easy to translate this to code. Which I'm not going to do
; but will point to implementation from Eli Benderski :) http://eli.thegreenplace.net/2007/07/27/sicp-section-214/
;
; There, implementation could be optimized in the step that needs 4 multiplications by setting maximum
; immediately to the value of b * d and selecting minimum only between 2 values. Check combination number 5
; above.
;
