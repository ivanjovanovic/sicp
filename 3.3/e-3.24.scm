; Exercise 3.24.  In the table implementations above, the keys are tested for
; equality using equal? (called by assoc). This is not always the appropriate
; test. For instance, we might have a table with numeric keys in which we don't
; need an exact match to the number we're looking up, but only a number within
; some tolerance of it. Design a table constructor make-table that takes as an
; argument a same-key? procedure that will be used to test ``equality'' of
; keys. Make-table should return a dispatch procedure that can be used to
; access appropriate lookup and insert! procedures for a local table.
; ------------------------------------------------------------

(load "../helpers.scm")

; We can reuse the definition we already have

(define (make-table same-key?)
  ; we need to redefine assoc in terms of generic equality operation
  (define (assoc key records)
    (cond ((null? records) '#f)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  '#f))
            '#f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; Using make-table, we could implement the get and put operations used in
; section 2.4.3 for data-directed programming, as follows:

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; small test
(output (put 'a 'b 3))
(output (get 'a 'b))
