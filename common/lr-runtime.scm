(define (move-error-status error-status)
  (if (zero? error-status) error-status (- error-status 1)))

(define (advance-input error-status input)
  (cond
   ((zero? error-status) input)
   ((stream-empty? input)
    (error "parse error: premature end of input"))
   (else (stream-cdr input)))
