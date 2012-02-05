; Exercise 3.15.  Draw box-and-pointer diagrams to explain the effect of
; set-to-wow! on the structures z1 and z2 above.
; ------------------------------------------------------------

; I will not draw them here, but I did them on paper. Could have scanned
; them if my handwriting is not destroyed by typing :)

; In case of z1, both car and cdr point to the same x and therefor
; when rendering output of z2 we wil see
; ((wow b) wow b)
;
; In case of z2 both car and cdr values point to the same symbo, but as
; soon as we change one, they will just point to the different symbols,
; but car and cdr pointers will still point ot different objects.
