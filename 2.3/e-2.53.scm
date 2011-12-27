; Exercise 2.53.
;
; What would the interpreter print in response to evaluating each of the following expressions?

; (list 'a 'b 'c)

; (list (list 'george))
; (cdr '((x1 x2) (y1 y2)))

; (cadr '((x1 x2) (y1 y2)))
; (pair? (car '(a short list)))
; (memq 'red '((red shoes) (blue socks)))

; (memq 'red '(red shoes blue socks))
; --------------------------------------------------

; 1. (list 'a 'b 'c) > (a b c)

; 2. (cdr '((x1 x2) (y1 y2))) > ((y1 y2))

; 3. (cadr '((x1 x2) (y1 y2))) > (y1 y2)

; 4. (pair? (car '(a short list))) > false

; 5. (memq 'red '((red shoes) (blue socks))) > false

; (memq 'red '(red shoes blue socks)) > (red shoes blue socks)
