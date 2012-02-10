; Queue is a special type of sequence where elements are added at the end (rear) and retrieved from
; the beginning (front). Because of the way it works is called FIFO buffer.

; Queue is defined by the simple abstraction
;
; constructor
; (make-queue)
;
; two selectors
; (empty-queue? <queue>)
; (front-queue <queue>)
;
; and two mutators
; (insert-queue! <queue> <item>) ; puts to rear
; (delete-queue! <queue>) ; gets from front

; We can implement these abstraction pretty easy

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

; on top of this low level abstraction we can build queue

(define (empty-queue? queue) (null? (front-ptr queue)))

; making a queue is just a pair of empty car and cdr
(define (make-queue) (cons '() '()))

; to select element from the front
(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with and empty queue" queue)
    (car (front-ptr queue)))) ; returns first element from the queue

; inserting item
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue) ; if empty
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else ; else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue)))) ; return modified pair of pointers

; deleting - getting first element from the front
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called on the empty queue"))
        (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))
