; We will start by defining one-dimensional table.
; Table consists of a backbone list in which every car points to the key,value pair
; and head in front of the backbone list. 
;
; Lookup method is used to find the value for the given key. It will use (assoc) procedure
; to retrieve the record from the list based on the key

(load "../helpers.scm")

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      '#f)))

(define (assoc key records)
  (cond ((null? records) '#f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

; inserting into table is not complicated. We have to see
; if there is already a key. If there is, we will updated value, otherwise
; we will insert new record.

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table 
                (cons (cons key value) (cdr table))))))

; constructing a table is easy, it is for start only emtpty head pointer
(define (make-table)
  (list '*table*))

(define t (make-table))

; (insert! 'a 2 t)
; (insert! 'b 3 t)
; (insert! 'c 4 t)
; (insert! 'd 5 t)
; (insert! 'e 6 t)
; (output (lookup 'a t))
; (output (lookup 'b t))
; (output (lookup 'c t))
; (output (lookup 'd t))
; (output (lookup 'e t))

; Two dimensional tables are in fact just recursivelly defined by 1D table
; There is first-level backbone that define the more general keys, inside every of these
; there is one dimensional table with second-level backbone. Looking up this table is just two-ste process

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          '#f))
      '#f)))

; To insert element into the table, we use same 2D approach

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

; as with previous abstractions that depend on manipulation of the state of certain data structure
; we can make table as object that has local state and just return dispatch method on it
;
(define (make-table)
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

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; small test
; (output (put 'a 'b 3))
; (output (get 'a 'b))
