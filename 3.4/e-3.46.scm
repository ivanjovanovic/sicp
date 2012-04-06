; Exercise 3.46.
;
; Suppose that we implement test-and-set! using an
; ordinary procedure as shown in the text, without attempting to make
; the operation atomic. Draw a timing diagram like the one in figure
; 3.29 to demonstrate how the mutex implementation can fail by allowing
; two processes to acquire the mutex at the same time.
; ------------------------------------------------------------

; First we have to define what means to aquire mutex.
; In the implementation of make-mutex and test-and-set! state of
; waiting for mutex to be aquired is defined by recursive call to
; (mutex 'aquire) until it retuns false.
; So, we are looking for the case in which both of them are going to
; return false from the (test-and-set!) and procede with execution.

; So, if test-and-set! is not atomic, both processes can do check for
; the (if (car cell)) one after the other without waiting for each other
; to set it. After that case we'll have that both of the processes are
; going to understand that they will exclusively acquire the mutex which
; is not true.



; time
;  ||              P1                        P2
;  ||               |                        |
;  ||            (if (car cell))             |
;  ||               |                        |
;  ||               |                 (if (car cell))
;  ||               |                        |
;  ||               |                        |
;  ||             acquire it                 |
;  ||                                        |
;  ||                                    acquire it
;  ||
;  ||
; \  /
;  \/
