; Exercise 4.11.
;
; Instead of representing a frame as a pair of lists, we
; can represent a frame as a list of bindings, where each binding is a
; name-value pair. Rewrite the environment operations to use this
; alternative representation.
; ------------------------------------------------------------

; Since frame implementation is abstracted, we have to reiplement the
; abstarction with different underlying implementation

(load "../helpers.scm")

(define (make-frame variables values)
  (zip-lists cons variables values))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (cons frame (cons var val)))

(define (zip-lists proc . arglists)
  (if (null? (car arglists))
      '()
      (cons
       (apply proc (map car arglists))
       (apply zip-lists
              (cons proc (map cdr arglists))))))

;(output (zip-lists cons (list 1 2 3) (list 2 3 4)))
;(output (map car (zip-lists cons (list 1 2 3) (list 2 3 4))))
;(output (map cdr (zip-lists cons (list 1 2 3) (list 2 3 4))))
