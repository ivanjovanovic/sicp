; Symbols are defined by quoting 

(define symbol-a 'a)

(define a 5)

; (display 'a) ; a
; (display a) ; 5

; operation eq? tests if symbols are the sam

; (display (eq? 'a 'a) ) ; true

; one useful function can be defined to return sublist of the list that
; starts with a given symbol

(define (memq symbol lst)
  (cond ((null? lst) false)
        ((eq? (car lst) symbol) lst)
        (else (memq symbol (cdr lst))))) ; here recursion just returns the value of the deepest recursion

; (display (memq 'a (list 'c 'a 'b 'd))) ; (a b d)
