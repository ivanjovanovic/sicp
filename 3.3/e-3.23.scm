; Exercise 3.23.
;
; A deque (``double-ended queue'') is a sequence
; in which items can be inserted and deleted at either the front
; or the rear. Operations on deques are the constructor
; make-deque, the predicate empty-deque?, selectors front-deque
; and rear-deque, and mutators front-insert-deque!,
; rear-insert-deque!, front-delete-deque!, and rear-delete-deque!.
; Show how to represent deques using pairs, and give
; implementations of the operations. All operations should be
; accomplished in (1) steps.
; ------------------------------------------------------------

(load "../helpers.scm")

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))

    ; makes queue element that contains item and pair of pointers
    ; to previous (cadr) and next (cddr) elements
    (define (make-deque-element item)
      (cons item (cons '() '())))

    (define (empty-deque?) (null? front-ptr))

    (define (front-deque)
      (if (empty-deque?)
        (error "FRONT called with and empty deque")
        (car front-ptr)))

    (define (rear-deque)
      (if (empty-deque?)
        (error "REAR can not be called with empty deque")
        (car rear-ptr)))

    (define (front-insert-deque! item)
      (let ((new-pair (make-deque-element item)))
        (cond ((empty-deque?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
                ; configure pointers
                (set! (cddr new-pair) front-ptr)
                (set! (cadr front-ptr) new-pair)
                (set! front-ptr new-pair)
                front-ptr))))

    (define (rear-insert-deque! item)
      (let ((new-pair (make-deque-element item)))
        (cond ((empty-deque?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
                ; configure pointers
                (set! (cddr rear-ptr) new-pair)
                (set! (cadr new-pair) rear-ptr)
                (set! rear-ptr new-pair)
                rear-ptr))))

    (define (front-delete-deque!)
      (cond ((empty-queue?)
             (error "DELETE! called on the empty queue"))
            (else
              (set! front-ptr (cddr front-ptr))
              front-ptr)))

    ; here I experienced the need to have back pointer to cddrious pair
    ; in order to make it possible to remove last one in O(1) steps
    (define (rear-delete-deque!)
      (cond ((empty-queue?)
             (error "DELETE! called on the empty queue"))
            (else
              (set! rear-ptr (cadr rear-ptr))
              rear-ptr)))

    ; returns list of only items from front to rear
    (define (front-item-list ptr)
      (if (eq? rear-ptr ptr)
        (list (car ptr)) ; just get the value of last
        (cons (car ptr) (front-item-list (cddr ptr)))))

    ; returns list of only items from rear to front
    (define (rear-item-list ptr)
      (if (eq? front-ptr ptr)
        (list (car ptr)) ; just get the value of last
        (cons (car ptr) (rear-item-list (cadr ptr)))))

    (define (dispatch m)
      (cond ((eq? m 'empty-deque?) (empty-deque?))
            ((eq? m 'front-deque) (front-deque))
            ((eq? m 'rear-deque) (rear-deque))
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'rear-insert-deque!) rear-insert-deque!)
            ((eq? m 'front-delete-deque!) (front-delete-deque!))
            ((eq? m 'rear-delete-deque!) (rear-delete-deque!))
            ((eq? m 'front-list) (front-item-list front-ptr))
            ((eq? m 'rear-list) (rear-item-list rear-ptr))

            (else 
              (error "Wrong message for queue"))))
    dispatch))

; testing a bit
(define dq (make-deque))
((dq 'front-insert-deque!) 5)
((dq 'front-insert-deque!) 6)
((dq 'front-insert-deque!) 7)
((dq 'front-insert-deque!) 8)
((dq 'front-insert-deque!) 9)
(output (dq 'front-list))
(output (dq 'rear-list))

(define dq (make-deque))
((dq 'rear-insert-deque!) 5)
((dq 'rear-insert-deque!) 6)
((dq 'rear-insert-deque!) 7)
((dq 'rear-insert-deque!) 8)
((dq 'rear-insert-deque!) 9)
(output (dq 'rear-list))
(output (dq 'front-list))
