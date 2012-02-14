; Exercise 3.25.
;
; Generalizing one- and two-dimensional tables, show how to
; implement a table in which values are stored under an arbitrary number of
; keys and different values may be stored under different numbers of keys. The
; lookup and insert! procedures should take as input a list of keys used to
; access the table.
; ------------------------------------------------------------

(load "../helpers.scm")

; This is not hard to do only if we allow hierarchical data structure where
; value of the key/value pair to be new 1D table.

(define (make-table)

  (define (assoc key records)
    (cond ((null? records) '#f)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))


  (let ((local-table (list '*table*)))

    (define (lookup keys) 
      (define (lookup-recursive keys table)
        (let ((record (assoc (car keys) (cdr table))))
          (if record
            (if (null? (cdr keys))
              (cdr record)
              (if (pair? (cdr record))
                (lookup-recursive (cdr keys) record)
                '#f))
            '#f)))
      (lookup-recursive keys local-table))

    (define (insert! keys value)
      (define (insert-recursive! keys table)
        (let ((record (assoc (car keys) (cdr table))))
          (if record
            (if (null? (cdr keys))
              (set-cdr! record value)
              (if (pair? (cdr record))
                (insert-recursive! (cdr keys) record)
                ; this can be abstracted in recursive make
                (begin
                  (set-cdr! record
                            (cons (cons (cadr keys) '()) '()))
                  (insert-recursive! (cdr keys) record))))
            ; no record found
            (if (null? (cdr keys))
              (set-cdr! table
                        (cons (cons (car keys) value) (cdr table)))
              ; this can be abstracted in a recursive make
              (begin 
                (set-cdr! table
                          (cons (cons (car keys) '()) (cdr table)))
                (insert-recursive! keys table))))))
      (insert-recursive! keys local-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

; some examples to se if this actually works
(define t (make-table))
(define put (t 'insert-proc!))
(define get (t 'lookup-proc))

(put '(a) 10)
(put '(a b) 5)
(output (get '(a)))
(put '(a b d) 6)
(put '(a b e f) 8)



(output (get '(a)))
(output (get '(a b c)))
(output (get '(a b d)))
(output (get '(a b e)))
(output (get '(a b e f)))
