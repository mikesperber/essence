(define-without-memoization (maybe-the-member element set)
  (let loop ((set set))
    (cond
     ((null? set) #f)
     ((equal? element (car set))
      (car set))
     (else
      (loop (cdr set))))))

(define-without-memoization (the-member element set)
  (let loop ((set set))
    (cond
     ((null? (cdr set)) (car set))
     ((equal? element (car set)) (car set))
     (else (loop (cdr set))))))
