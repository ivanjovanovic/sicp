; Modeling with mutable data
; 
; Until now we worked only with non-mutable cons, car and cdr
; procedures.
; But, we can see this a bit differently. Cons can be defined as two
; mutations that set car and cdr

; (define (cons x y)
;  (let ((new (get-new-pair)))
;    (set-car! new x)
;    (set-cdr! new y)
;    new))

; where (get-new-pair) is the constructor of the empty pair

; history list that gives us way to store and see if there is something inside
(define history
  ; setting initial history it to null so append have to what to append actually ;)
  (let ((visited-list (cons '() '()))) 

    (define (visited? x)
      (define (iter visited-list x)
        (cond ((null? visited-list) '#f)
              ((eq? (car visited-list) x) '#t)
              (else (iter (cdr visited-list) x))))
      (iter visited-list x))
    
    (define (add x)
      (append! visited-list (cons x '()))
      visited-list)

    (lambda (m)
      (cond ((eq? m 'add) add)
            ((eq? m 'visited?) visited?)
            ((eq? m 'reset) (set! visited-list (cons '() '())))
            (else (error "Unknow operation on history"))))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

; getting the last pair of the list
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
