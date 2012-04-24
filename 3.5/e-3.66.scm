; Exercise 3.66.
;
; Examine the stream (pairs integers integers). Can you
; make any general comments about the order in which the pairs are
; placed into the stream? For example, about how many pairs precede the
; pair (1,100)? the pair (99,100)? the pair (100,100)? (If you can make
; precise mathematical statements here, all the better. But feel free to
; give more qualitative answers if you find yourself getting bogged
; down.)
; ------------------------------------------------------------

; Given pairs
;
; (1 1) (1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8) ....
;       (2 2) (2 3) (2 4) (2 5) (2 6) (2 7) (2 8) ....
;             (3 3) (3 4) (3 5) (3 6) (3 7) (3 8) ....
;                   (4 4) (4 5) (4 6) (4 7) (4 8) ....
;                     ......
;                                           (8 8) ....
;
; pulling pairs out of every next one will be half slower because it is
; interleaved with all the pairs that come in the stream above it.
; First row is interleaved with all below, so it will progress with the
; fastest speed, next will be two times slower, so progress of first 10 looks like
; (1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5) (2 4) (1 6) (3 4) (1 7) (2 5) (1 8) \
; (4 4) (1 9) (2 6) (1 10) (3 5) (1 11) .....
;
; We see that every second is from the first row, every thrid is from the second row
; every forth is from the third row .... so progression of the nth row will decline with
; the powers over 2, so looking for the pair of (i, j) will be proportional to
; 2^i and somehow to the distance in the stream defined by j. Number of elements in the stream are defined
; by the (j-i) element, so if 2^i is giving the reverse of the speed of progresssion (which is number of steps)
; and number of elements to achieve the jth element is somehow close to 2^i*(j-i).
;
; So for (1,100) is approx 2^i*99 = 198 steps
; for (100, 100) is 2^100*1 for example ...


; There are exact mathematical equations to calculate this.
; http://wqzhang.wordpress.com/2009/08/17/sicp-exercise-3-66/
;

