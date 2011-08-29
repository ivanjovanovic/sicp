; On example of abstracting the sum operator which sums up arbitrary
; sequence of values can be shown how procedures can be used as
; arguments to another procedures.
;
; Following three sum procedures have a lot of repetitive content.

(load "../common.scm")

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

; (display (sum-integers 1 10))
; (newline)

(define (sum-cubes a b)
  (if (> a b)
      0
      (+
        (cube a)
        (sum-cubes (+ a 1) b))))

; (display (sum-cubes 1 10))
; (newline)

; sum of sequence 1/(1*3) + 1/(5*7) + 1/(9*11) + ...
; converges to pi/8
(define (pi-sum a b)
  (if (> a b)
      0
      (+
        (/ 1.0 (* a (+ a 2)))
        (pi-sum (+ a 4) b))))

; (display (pi-sum 1 1000))
; (newline)

; It is obvious that they share underlying pattern
;
; (define (<name> a b)
;   (if (> a b)
;       0
;       (+ (<term> a)
;          (<name> (<next> a) b))))
;
; If we implement procedure which can satisfy this pattern it would need
; to have signature which accepts <term> and <next> so they can be used
; within procedure.
;
; The difference from what we introduced so far is that these have to be
; other procedures and not only numbers as we are used to. So we have to
; pass procedures as arguments. And implementation in general would look
; like

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Now is easy to implement sum of cubes with only couple of lines of
; code

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

; (display (sum-cubes 1 10))
; (newline)

; Given identity procedure f(a) = a sum of integers can be expressed

(define (identity a) a)
(define (sum-integers a b)
  (sum identity a inc b))

; (display (sum-integers 1 10))
; (newline)
;
; Or the pi-sum

(define (pi-term x)
  (/ 1.0 (* x (+ x 2))))
(define (next-pi-sum x)
  (+ x 4))

(define (pi-sum a b)
  (sum pi-term a next-pi-sum b))

; (display (pi-sum 1 1000))
; (newline)
;
; Numerical approximation of integral function can be expressed as well as
; sum with some additional procedures defined
; @see http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_sec_1.3.1
;

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

; (display (integral cube 0 1 0.01))
; (newline)

; gets to stack level to deep error in Heist, in DrRacket works with a bit more precision
; (display (integral cube 0 1 0.001))
; (newline)
