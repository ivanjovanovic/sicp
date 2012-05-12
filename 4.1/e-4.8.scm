; Exercise 4.8.  ``Named let'' is a variant of let that has the form

; (let <var> <bindings> <body>)

; The <bindings> and <body> are just as in ordinary let, except that <var> is
; bound within <body> to a procedure whose body is <body> and whose parameters
; are the variables in the <bindings>. Thus, one can repeatedly execute the
; <body> by invoking the procedure named <var>. For example, the iterative
; Fibonacci procedure (section 1.2.2) can be rewritten using named let as
; follows:

(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

; Modify let->combination of exercise 4.6 to also support named let.
; ------------------------------------------------------------

(load "e-4.6.scm")

; if we rewrite named let form in a way we are used to
; define internal named procedures that define iterative or recursive processe
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

; we can rewrite named let in a form syntactically similar to this
; only we can use (define fib-iter (lambda (a b count) ...)) form for
; defining new procedure

(define (let->combination exp)
  (if (variable? (cadr exp))
    (expand-named-let (let-operands exp))
    (expand-let (let-operands exp))))

(define (expand-named-let operands)
  (let ((var-name (car operands))
        (parameters (let-parameters (cadr operands)))
        (parameter-values (let-parameter-values (cadr operands)))
        (body (cddr operands)))
      (sequence->exp
        (cons
          (make-define var-name parameters body)
          (make-application var-name parameter-values)))))

(define (make-define name parameters body)
  (list 'define name (make-lambda parameters body)))


; test to see if this works
(define test '(let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
(output (let->combination test))
