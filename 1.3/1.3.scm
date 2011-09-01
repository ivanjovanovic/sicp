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
;
;
; 1.3.2 Using lambda defined anonymous functions
;
; Not all the functions have to be given a name. They can be defined as one purpose abstractions
; using lambda notation. For example this

(define next (lambda (x) (+ x 1)))

; defines procedure `next` with one argument x, as we have previously defined it
;
; pi-sum can be redefined using lambda notation as

(define (lambda-pi-sum a b)
  (sum 
    (lambda (x) (/ 1.0 (* x (+ x 2))))
    a
    (lambda (x) (+ x 4))
    b))

; (display (lambda-pi-sum 1 1000)) ; converges slowly towards pi/8
; (newline)

; Lambdas can be used to create local variables. For example, function
;
; f(x,y) = x(1 + xy)^2 + y(1 - y) + (1 + xy)(1 - y)
;
; can be written as
;
; a = 1 + xy
; b = 1 - y
;
; f(x, y) = xa^2 + yb + ab
;
; so we have here a need to define a and b and to use them in newly structured function f

; here lambda defines inline compound procedure which uses a and b as local variables, and variables are passed
; as arguments inline

(define (f x y)
  ((lambda (a b)
     (+
       (* x (square a))
       (* y b)
       (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

; This concept is very useful so special construct named `let` is defined to make it more expressive.
; By using it we can define function f as

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

; (display (f 2 3))
; (newline)

