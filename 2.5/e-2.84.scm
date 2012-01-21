; Exercise 2.84.
;
; Using the raise operation of exercise 2.83, modify the
; apply-generic procedure so that it coerces its arguments to have the
; same type by the method of successive raising, as discussed in this
; section. You will need to devise a way to test which of two types is
; higher in the tower. Do this in a manner that is ``compatible'' with
; the rest of the system and will not lead to problems in adding new
; levels to the tower.
; ------------------------------------------------------------

; There are certainly multiple ways of doing this and different
; approaches that can be taken. To me, since we have to compare in some
; way the weight of the type in the tower, numbering them comes as
; natural way. Every package can then just install new type with new
; ordinary number. For putting new levels on top is more or less not a
; problem. Problem is if new level in the tower is to be added
; in the middle, but it is not very complicated to renumber them in the
; system I guess. It is a bit inconvenient if 3rd party library comes
; with the type which has to be in the middle, then we have to update
; all the other libraries.
;
; I can define new table for type levels
(load "../helpers.scm")
(load "2.5.scm")
(load "e-2.78.scm")
(load "e-2.83.scm")

(define type-levels (make-hash-table))

(define (put-type-level type level)
  (hash-table-set! type-levels type level))

; doesn't return default value since it is an error to ask for type
; that doesn't exist in the system
(define (get-type-level type )
  (hash-table-ref type-levels type))

; predicate that will give information if one type is raisable to other
; one
(define (type-raisable? from to)
  (< (get-type-level from) (get-type-level to)))

(define (get-highest-type lst)
  (define (get-highest-type-iter iter-lst type)
    (if (null? iter-lst)
      type
      (if (type-raisable? type (type-tag (car iter-lst)))
        (get-highest-type-iter (cdr iter-lst) (type-tag (car iter-lst)))
        (get-highest-type-iter (cdr iter-lst) type))))
  (get-highest-type-iter lst 'scheme-number))

; load the type hierarchy
(put-type-level 'scheme-number 0)
(put-type-level 'rational 1)
(put-type-level 'complex 2)

; (output (get-highest-type (list 5 (make-rational 5 3) (make-complex-from-real-imag 3 4))))

; now I can redefine apply-generic by not using coerce-list-to-type from
; 2.82 but new one with raising to highest type
  (define (raise-type from to)
    (if (not (type-raisable? (type-tag from) to))
      from
      (raise-type (raise from) to)))

  (define (raise-list-to-type lst type)
    (if (null? lst)
      '()
      (cons (raise-type (car lst) type) (raise-list-to-type (cdr lst) type))))

(define (apply-generic op . args)


  (define (apply-raised args)
    (let ((highest-type (get-highest-type args)))
      (let ((raised-list (raise-list-to-type args highest-type)))
        (let ((type-tags (map type-tag raised-list)))
          (let ((proc (get op type-tags)))
            (if proc
              (apply proc (map content raised-list))
              (error "Coercion by raising to highes type didn't find appropriate procedure")))))))
  
  ; logic to prevent always coercing if there is already direct input entry
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map content args))
        (apply-raised args)))))

(output (get-highest-type (list 5 (make-complex-from-real-imag 3 4))))
(output (raise-list-to-type (list 5 (make-complex-from-real-imag 3 4)) 'complex))
(output (add 5 (make-complex-from-real-imag 3 4)))

; (output (cdr (content (make-rational 5 1))))
; (output (raise-type 5 'complex))
; (output (numer (content (make-rational 5 1))))
; (output (raise (make-rational 5 1)))
