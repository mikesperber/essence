(define-without-memoization (maybe-the-member element set)
  (let loop ((set set))
    (_memo2
     (cond
      ((null? set) #f)
      ((eqv? element (car set)) (car set))
      (else (loop (cdr set)))))))

(define-without-memoization (the-member element set)
  (let loop ((set set))
    (_memo2
     (cond
      ((null? (cdr set)) (car set))
      ((eqv? element (car set)) (car set))
      (else (loop (cdr set)))))))
