; Exercise 3.22.  Instead of representing a queue as a pair of pointers, we can
; build a queue as a procedure with local state. The local state will consist
; of pointers to the beginning and the end of an ordinary list. Thus, the
; make-queue procedure will have the form

; (define (make-queue)
;   (let ((front-ptr ...)
;         (rear-ptr ...))
;     <definitions of internal procedures>
;     (define (dispatch m) ...)
;     dispatch))

; Complete the definition of make-queue and provide implementations of the
; queue operations using this representation.
; ------------------------------------------------------------
(load "../helpers.scm")

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty-queue?) (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with and empty queue")
        (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
                (set-cdr! rear-ptr new-pair)
                (set! rear-ptr new-pair)
                front-ptr))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called on the empty queue"))
            (else
              (set! front-ptr (cdr front-ptr))
              front-ptr)))


    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            ((eq? m 'queue) front-ptr)
            (else 
              (error "Wrong message for queue"))))
    dispatch))

(define q (make-queue))
((q 'insert-queue!) 5)
(output (q 'front-queue))
(output ((q 'insert-queue!) 5))
(output (q 'empty-queue?))
(output (q 'queue))
(q 'delete-queue!)
(q 'delete-queue!)
(output (q 'empty-queue?))
