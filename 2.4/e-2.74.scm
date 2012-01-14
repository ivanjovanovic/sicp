; Exercise 2.74.
;
; Insatiable Enterprises, Inc., is a highly decentralized
; conglomerate company consisting of a large number of independent divisions
; located all over the world. The company's computer facilities have just been
; interconnected by means of a clever network-interfacing scheme that makes the
; entire network appear to any user to be a single computer. Insatiable's
; president, in her first attempt to exploit the ability of the network to
; extract administrative information from division files, is dismayed to
; discover that, although all the division files have been implemented as data
; structures in Scheme, the particular data structure used varies from division
; to division. A meeting of division managers is hastily called to search for a
; strategy to integrate the files that will satisfy headquarters' needs while
; preserving the existing autonomy of the divisions.

; Show how such a strategy can be implemented with data-directed programming.
; As an example, suppose that each division's personnel records consist of a
; single file, which contains a set of records keyed on employees' names. The
; structure of the set varies from division to division. Furthermore, each
; employee's record is itself a set (structured differently from division to
; division) that contains information keyed under identifiers such as address
; and salary. In particular:

; a.  Implement for headquarters a get-record procedure that retrieves a
; specified employee's record from a specified personnel file. The procedure
; should be applicable to any division's file. Explain how the individual
; divisions' files should be structured. In particular, what type information
; must be supplied?

; b.  Implement for headquarters a get-salary procedure that returns the salary
; information from a given employee's record from any division's personnel
; file. How should the record be structured in order to make this operation
; work?

; c.  Implement for headquarters a find-employee-record procedure. This should
; search all the divisions' files for the record of a given employee and return
; the record. Assume that this procedure takes as arguments an employee's name
; and a list of all the divisions' files.

; d.  When Insatiable takes over a new company, what changes must be made in
; order to incorporate the new personnel information into the central system?
; ------------------------------------------------------------

(load "../helpers.scm")
(load "2.4.scm")

; Each division has a list of records of employess

; a) implementation of a generic get-record procedure
;

(define ins-div1-records
  (list 
    (list (cons 'name "Ivan") (cons 'address "Some address") (cons 'position "developer") (cons 'salary "milions"))
    (list (cons 'name "Someone") (cons 'address "some address") (cons 'position "manager") (cons 'salary "bilions"))))

; here we'll presume to hardcode the structure
(define ins-div2-records
  (list 
    (list "Alyss" (list "complex number dev" "some address" 10))
    (list "George" (list "complex nr dev" "some address" 20))))

; we have to tag the files so we know from which department they come
(define typed-ins1
  (attach-tag 'ins1-record ins-div1-records))

(define typed-ins2
  (attach-tag 'ins2-record ins-div2-records))

; for easier implementation we should define couple of predicates
(define (ins1? file)
  (eq? (car file) 'ins1-record))

(define (ins2? file)
  (eq? (car file) 'ins2-record))

; We have to implement generic get-record procedure
(define (get-record file name)
  (cond ((ins1? file)
         (get-ins1-record (content file) name))
        ((ins2? file)
         (get-ins2-record (content file) name))))

; and we have to define the procedures
(define (get-ins1-record records name)
  (cond ((null? records) '())
        ((equal? name (cdaar records)) 
         (attach-tag 'ins1-record (car records)))
        (else (get-ins1-record (cdr records) name))))

; and for the second one
(define (get-ins2-record records name)
  (cond ((null? records) '())
        ((equal? name (caar records)) 
         (attach-tag 'ins2-record (car records)))
        (else (get-ins2-record (cdr records) name))))

; (output typed-ins1)
(output (get-record typed-ins1 "Ivan"))
(output (get-record typed-ins2 "George"))

; b) Implementation of a generic get-salary procedure

(define (get-salary record)
  (cond ((ins1? record)
         (get-ins1-salary (content record)))
        ((ins2? record)
         (get-ins2-salary (content record)))))

(define (get-ins1-salary record)
  (cond ((null? record)
         (error "Invalid record, doesn't contain salary information"))
        ((eq? 'salary (caar record))
         (cdar record))
        (else (get-ins1-salary (cdr record)))))

; we'll make this intentionally dumb. There can't be everyone smart in this company
(define (get-ins2-salary record)
  (car (cddadr record)))

(output (get-salary (get-record typed-ins1 "Ivan")))
(output (get-salary (get-record typed-ins2 "George")))

; Up to now, we have implemented two generic procedures that operate on any of the two types of files
; and produce results, so what we have is two data abstractions that come from
; two departments


(define (ins1? file)
  (eq? (car file) 'ins1-record))

; and for the second one
(define (get-ins2-record records name)
  (cond ((null? records) '())
        ((equal? name (caar records)) 
         (attach-tag 'ins2-record (car records)))
        (else (get-ins2-record (cdr records) name))))

(define (get-ins2-salary record)
  (car (cddadr record)))

(define (ins2? file)
  (eq? (car file) 'ins2-record))

; now, the third department comes and asks what should they do to add their records
; available to the global management. We tell them to wait a moment that we refactor our
; code so it is easy for them just to install it.
;
; For doing that we'll take data directed approach of defining actions specific to types
; in a table with familiar put/get interface.

; First we redefine global procedures to reuse procedure apply-generic

(define (get-record file name)
  ; finds a method to apply to content
  ((get 'get-record (type-tag file)) (content file) name))

(define (get-salary record)
  ; finds a method to apply to content
  ((get 'get-salary (type-tag record)) (content record)))

; and we would just have to install our packages
(define install-ins1-records
  (define (get-ins1-record records name)
    (cond ((null? records) '())
          ((equal? name (cdaar records)) 
           (attach-tag 'ins1-record (car records)))
          (else (get-ins1-record (cdr records) name))))
  (define (get-ins1-salary record)
    (cond ((null? record)
           (error "Invalid record, doesn't contain salary information"))
          ((eq? 'salary (caar record))
           (cdar record))
          (else (get-ins1-salary (cdr record)))))
  (put 'get-record 'ins1-record get-ins1-record)
  (put 'get-salary 'ins1-record get-ins1-salary))

(define install-ins2-records
  (define (get-ins2-record records name)
    (cond ((null? records) '())
          ((equal? name (caar records)) 
           (attach-tag 'ins2-record (car records)))
          (else (get-ins2-record (cdr records) name))))
  (define (get-ins2-salary record)
    (car (cddadr record)))
  (put 'get-record 'ins2-record get-ins2-record)
  (put 'get-salary 'ins2-record get-inst2-salary))

; Now we just have to tell them the rules of the table and ask them for the installation
; procedure so we can integrate their package into our system.

; lets artificially create table of procedures
(define (get op type)
  (cond ((and (eq? op 'get-record)
              (eq? type 'ins1-record))
         get-ins1-record)
        ((and (eq? op 'get-record)
              (eq? type 'ins2-record))
         get-ins2-record)
        ((and (eq? op 'get-salary)
              (eq? type 'ins1-record))
         get-ins1-salary)
        ((and (eq? op 'get-salary)
              (eq? type 'ins2-record))
         get-ins2-salary)))
; c )
;
; Making generic find record is now easier as well
; It just drills down the list of files and returns firs
; record found, or nill if there is nothing
(define (find-employee-record files name)
  (cond ((null? files) '())
        (else
          (let ((proc (get 'get-record (type-tag (car files)))))
            (let ((record (proc (content (car files)) name)))
              (if (not (null? record))
                (content record)
                (find-employee-record (cdr files))))))))

(output (find-employee-record (list typed-ins1 typed-ins2) "Ivan"))
