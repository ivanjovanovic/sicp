; Exercise 3.47.
;
; A semaphore (of size n) is a generalization of a
; mutex. Like a mutex, a semaphore supports acquire and release
; operations, but it is more general in that up to n processes can
; acquire it concurrently. Additional processes that attempt to acquire
; the semaphore must wait for release operations. Give implementations
; of semaphores

; a. in terms of mutexes

; b. in terms of atomic test-and-set! operations.
; ------------------------------------------------------------
;
; http://en.wikipedia.org/wiki/Semaphore_(programming)
;
; Semaphore is kind of generalization of the mutex. There are two
; differences that can be pointed out.
;
; 1) Somafore can have count n, then they are called `counting semaphores`
; and can be aquired by n concurrent processes. These are used when
; there are more instances of a shared resource.
; If number of instances is 1 then we have 0 and 1 as possible counts on
; semaphore which is same as by mutex. These kind of semaphores we call
; `binary semaphores`
; 2) Semaphor doesn't belong to a procedure that aquired it and it is
; possible to be accessed by multiple processes in the same time.
; Therefore we have to make update of the semaphore state to be
; protected.


; with simple count, mutex based protection
(define (make-semaphore n)
  (let ((count n)
        (mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (= count 0)
               (begin
                 (mutex 'release)
                 (the-semaphore 'acquire)) ; will wait until at least one free
               (begin
                 (set! count (- count 1))
                 (mutex 'release))))
            ((eq? m 'release)
             (mutex 'acquire)
             (if (= count n) ; if someone calls 'release on mutex it did not acquire
               (mutex 'release)
               (begin
                 (set! count (+ count 1))
             (mutex 'release))))))
    the-semaphore))

; If we imagine test-and-set! and clear! procedures are atomic.
(define (make-semaphore n)
  (let ((count n))

    (define (test-and-set!)
      (if (= count 0)
        true
        (begin (set! count (- count 1))
               false)))

    (define (clear!)
      (if (< count n)
        (set! count (+ count 1))))

    ; much simpler logic of the semaphore itself with
    ; details of implementation moved to the test-and-set! and clear!
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set!)
               (the-semaphore 'acquire))) ; retry
            ((eq? m 'release) (clear!))))))


