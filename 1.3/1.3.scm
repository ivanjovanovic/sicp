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

; 1.3.3 Procedures as general methods
;
; Halving method for finding roots of the function can be expressed like this:
; @see http://en.wikipedia.org/wiki/Bisection_method

(define (search f neg-point pos-point delta)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point delta)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (search f neg-point midpoint delta))
              ((negative? test-value)
               (search f midpoint pos-point delta))
              (else midpoint))))))

; to do some before checking on the conditions by which this algorithm operates
; we define new procedure

(define (bisection-method f a b)
  (let ((a-val (f a))
        (b-val (f b))
        (delta 0.001))
    (cond ((and (negative? a-val) (positive? b-val))
           (search f a b delta))
          ((and (negative? b-val) (positive? a-val))
           (search f b a delta))
          (else
            (error "Values are not of the opposite sign")))))

(define linear
  (lambda (x) (+ x 1)))

; (display (bisection-method linear -5 3))
; (newline)

; (display (bisection-method sin 2.0 4.0))
; (newline)

; (display (bisection-method
;            (lambda (x) (- (* x x x) (* 2 x) 3))
;            1.0
;            2.0))
; (newline)
;
;
; Using higher-order functions we can expess a way to find fixed
; point of a function.
;
; A number x is said to be the fixed point of the function f if f(x) = x.
; Some functions have property to have `attractive` fixed points which can
; be found by continuously applying function to an argument until value of the
; function doesn't change that much.
;
; f(x), f(f(x)), f(f(f(x))) ...

; Just continuously apply function and check if its value is far from
; the argument passed to it. If not just update the argument with last
; result of function evaluation
(define (fixed-point f first-guess delta)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next delta)
        next
        (try next))))
    (try first-guess))

; (display (fixed-point cos 1.0))
; (newline)

; (display (fixed-point sin 1.0 0.001))
; (newline)
;
; Sometimes functions themselves are not appropriate to be used
; for finding their fixed point directly. Then, we try to do
; average dump of the function by calculating the average of the 
; function value for some argument x and the argument x itself.

(define (average-dump f)
  (lambda (x) (average x (f x))))

; One of the functions that is not suitable for usage in fixed-point search
; is square root of x, sqrt(x). Function is defined as y = sqrt(x), therefore
; y^2 = x and there comes y = x/y. 
; Problem is that if we say start with the guess y1 then y2 = x/y1.
; In second iteration y3 = x/y2 which is y1.
;
; So, this function oscilates for standard numerical process. Therefore we introduce
; avergae dumping to make this process convergent.

(define (sqrt x)
  (fixed-point (average-dump (lambda (y) (/ x y))) 1.0 0.001))

; (display (sqrt 40))
; (newline)
;
; We can as well define cube-root function this way

(define (cube-root x)
  (fixed-point (average-dump (lambda (y) (/ x (square y))))
               1.0
               0.001))

; (display (cube-root 8))
; (newline)
;
; Newton method of finding roots of the functions is generalized method which states 
; that solution to the equation g(x) = 0 is fixed point of the function
;
; f(x) = x - g(x)/dg(x) ; dg(x) is derivative of g(x)
;
; expressing derivate of a function
;

(define dx 0.0001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

; (display ((deriv cube) 5))
; (newline)
;
; Now defining Newton function and newtons method

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess 0.0001))

; If we look for solution to sqrt(x) then y^2 = x and y^2 -x = 0 are solutions for given x
(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

; (display (sqrt 22))
; (newline)
