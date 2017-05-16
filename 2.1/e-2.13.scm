; Exercise 2.13.
; Show that under the assumption of small percentage tolerances there is a 
; simple formula for the approximate percentage tolerance of the product of two 
; intervals in terms of the tolerances of the factors. You may simplify the 
; problem by assuming that all numbers are positive.
; --------------------------------------------------
;
; If we represent intervals with percentages like this [a*(1 - Pa), a*(1 + Pa)]
; then multiplication is
;
; [a*(1 - Pa), a*(1 + Pa)] * [b*(1 - Pb), b*(1 + Pb)]
;
; which at the end comes to
;
; [a*b*(1 - (Pa + Pb - Pa*Pb)), a*b*(1 + (Pa + Pb + Pa*Pb))]
;
; so in order to set it into wanted form we have to make
; 
; Pa + Pb - Pa*Pb == Pa + Pb + Pa*Pb
;
; If Pa and Pb are really small then their product is becoming 
; insignificant comparing to Pa and Pb and can be discarded as such.
;
; So percentage in this case can be expressed as Pa + Pb.
