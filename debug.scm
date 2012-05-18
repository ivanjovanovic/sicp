(define *log-level* 3)

(define (set-minimum-log-level level)
  (cond ((eq? level 'DEBUG) (set! *log-level* 1))
        ((eq? level 'INFO) (set! *log-level* 2))
        ((eq? level 'WARN) (set! *log-level* 3))
        ((eq? level 'ERROR) (set! *log-level* 4))))

(define (logger level . msgs)
  (if (<= *log-level* level)
    (begin
      (map display (cons msgs " "))
      (newline))))

(define (debug-log . msgs) (logger 1 'DEBUG msgs))
(define (info-log . msgs) (logger 2 'INFO msgs))
(define (warn-log . msgs) (logger 3 'WARN msgs))
(define (err-log . msgs) (logger 4 'ERROR msgs))
