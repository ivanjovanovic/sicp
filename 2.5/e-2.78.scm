; Exercise 2.78.
;
; The internal procedures in the scheme-number package are
; essentially nothing more than calls to the primitive procedures +, -, etc. It
; was not possible to use the primitives of the language directly because our
; type-tag system requires that each data object have a type attached to it. In
; fact, however, all Lisp implementations do have a type system, which they use
; internally. Primitive predicates such as symbol? and number? determine
; whether data objects have particular types. Modify the definitions of
; type-tag, contents, and attach-tag from section 2.4.2 so that our generic
; system takes advantage of Scheme's internal type system. That is to say, the
; system should work as before except that ordinary numbers should be
; represented simply as Scheme numbers rather than as pairs whose car is the
; symbol scheme-number.
; ------------------------------------------------------------

(load "2.5.scm")
; Previous implementations of the type system is

; constructor for the tagged contents
(define (attach-tag type-tag contents)
  (cons type-tag contents))

; selectors
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "bad tagged data -- TYPE-TAG" datum)))

(define (content datum)
  (if (pair? datum)
    (cdr datum)
    (error "bad tagged data -- CONTENTS" datum)))

; In every of these I'll check for the special case when it is a number.
; I'll keep the tag 'scheme-number to identify them in the operations table
; but they do not have to carry it in order to work properly anymore

(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else "bad tagged data - TYPE-TAG")))

(define (content datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else "bad tagged data")))

; to test if this works we do

; (output (add 3 4)) ; 7
