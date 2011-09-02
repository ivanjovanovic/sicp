; Exercise 1.37.
;
; a) An infinite continued fraction is an expression of the form
;
;         N1
; ---------------------
;             N2
; D1 +  ---------------
;               N3
;       D2 + ----------
;             D3 + ....
;
; As an example, one can show that the infinite continued fraction
; expansion with the Ni and the Di all equal to 1 produces 1/Phi, where Phi
; is the golden ratio (described in section 1.2.2).
; One way to approximate an infinite continued fraction is to truncate the
; expansion after a given number of terms. Such a truncation, so-called k-term
; finite continued fraction -- has the form
;
;         N1
; ---------------------
;             N2
; D1 +  ---------------
;               N3
;       D2 + ----------
;            .
;             .
;              .
;                 Nk
;                -----
;                  Dk
;
;
; Suppose that n and d are procedures of one argument (the term index i) that
; return the Ni and Di of the terms of the continued fraction. Define a procedure
; cont-frac such that evaluating (cont-frac n d k) computes the value of the k-term
; finite continued fraction. Check your procedure by approximating 1/Phi using
;
; (cont-frac (lambda (i) 1.0)
;            (lambda (i) 1.0)
;            k)

; for successive values of k. How large must you make k in order to get an approximation
; that is accurate to 4 decimal places?

; b) If your cont-frac procedure generates a recursive process, write one that generates an
; iterative process. If it generates an iterative process, write one that generates a recursive process.
;
; --------------------------------------------------

(define (cont-frac n-proc d-proc k)
  (define (recurse i)
    (let ((n (n-proc i))
          (d (d-proc i)))
      (if (= i k)
        (/ n d)
        (/ n (+ d (recurse (+ i 1)))))))
  (recurse 0))

; It takes k = 12 so it converges to 0.6803... which is 4 decimals
; precision as required
;
; (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12))
; (newline)
;
; b) Implementing iteratively
;
; The catch here is how we reason about calculating result in every next
; step. Here we start from the bottom and iterate towards the top. Thus,
; counter `i` is decreasing and ending only when becomes 1 in which case 
; we do one more calculation and return result.

(define (cont-frac-iter n-proc d-proc k)
  (define (iter i result)
    (let ((n (n-proc i))
          (d (d-proc i)))
      (if (= i 1)
        (/ n (+ d result))
        (iter (- i 1) (/ n (+ d result))))))
  (iter k 1))

; (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 12))
; (newline)


