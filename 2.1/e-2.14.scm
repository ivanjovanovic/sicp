(load "e-2.12.scm")

; Exercise 2.14. 
;
; based on already defined methods for interval arithmetic we can try to calculate
; resistance of two parallel resistors in two ways. 

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; Lem complains that Alyssa's program gives different answers for the two ways of computing. This is a serious complaint.
; Demonstrate that Lem is right. Investigate the behavior of the system on a variety of arithmetic expressions. 
; Make some intervals A and B, and use them in computing the expressions A/A and A/B. You will get the most insight 
; by using intervals whose width is a small percentage of the center value. 
; Examine the results of the computation in center-percent form (see exercise 2.12).
; ----------------------------
;
; First lets see how does it produce different results

(define r1 (make-center-percent 10 1))
(define r2 (make-center-percent 100 1))
(define small-width-r1 (make-center-percent 10 0.01))
(define small-width-r2 (make-center-percent 100 0.01))

(define (print-interval i)
  (display "[")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "]")
  (newline))


(define (print-percent-interval i)
  (display "[center: ")
  (display (center i))
  (display ", percent: ")
  (display (percent i))
  (display "]")
  (newline))

; this will prove that two results are not the same
(print-interval (par1 r1 r2))
(print-percent-interval (par1 r1 r2))
(print-interval (par2 r1 r2))
(print-percent-interval (par2 r1 r2))

; A/A
(print-interval (div-interval r1 r1))
(print-percent-interval (div-interval r1 r1))
(print-interval (div-interval r2 r2))
(print-percent-interval (div-interval r2 r2))

; A/B 
(print-interval (div-interval r1 r2))
(print-percent-interval (div-interval r1 r2))
(print-interval (div-interval r2 r1))
(print-percent-interval (div-interval r2 r1))

; A/A
(print-interval (div-interval small-width-r1 small-width-r1))
(print-percent-interval (div-interval small-width-r1 small-width-r1))
(print-interval (div-interval small-width-r2 small-width-r2))
(print-percent-interval (div-interval small-width-r2 small-width-r2))

; A/B 
(print-interval (div-interval small-width-r1 small-width-r2))
(print-percent-interval (div-interval small-width-r1 small-width-r2))
(print-interval (div-interval small-width-r2 small-width-r1))
(print-percent-interval (div-interval small-width-r2 small-width-r1))


; So, printed results are  the following 
; 
; Parallel resistors:
; 
; [8.821782178217822, 9.36730945821855]
; [center: 9.094545818218187, percent: 2.9992002399280167]
; [9.0, 9.181818181818182]
; [center: 9.09090909090909, percent: 1.000000000000009]
;
; A/A wide inteval
;
; [0.9801980198019803, 1.02020202020202]
; [center: 1.0002000200020003, percent: 1.9998000199979855]
; [0.9801980198019802, 1.0202020202020203]
; [center: 1.0002000200020003, percent: 1.9998000199980077]
; 
; A/B wide interval
;
; [0.09801980198019802, 0.10202020202020202]
; [center: 0.10002000200020003, percent: 1.9998000199979933]
; [9.801980198019802, 10.202020202020202]
; [center: 10.002000200020003, percent: 1.9998000199979942]
; 
; A/A narrow interval
;
; [0.9998000199980004, 1.000200020002]
; [center: 1.0000000200000003, percent: 0.019999999799971167]
; [0.9998000199980001, 1.0002000200020003]
; [center: 1.00000002, percent: 0.01999999980001558]
; 
; A/B narrow interval
;
; [0.09998000199980002, 0.10002000200020002]
; [center: 0.10000000200000002, percent: 0.019999999799998926]
; [9.998000199980002, 10.002000200020001]
; [center: 10.000000200000002, percent: 0.019999999799988934]  ;
;
; We see that with smaller widths of the interval calculations become more precise.
; This is due to inherently imprecise interval arithmetic. It is providing only interval
; of possible results as outcome of operations and that outcome is going to be as precise
; as width of interval is small.
