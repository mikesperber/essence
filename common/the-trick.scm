(define (the-trick element set cont fail)
  (let loop ((set set))
    (if (null? set)
	(fail)
	(if (equal? element (car set))
	    (cont (car set))
	    (loop (cdr set))))))

(define (the-trick-cannot-fail element set cont)
  (let loop ((set set))
    (if (null? (cdr set))
	(cont (car set))
	(if (equal? element (car set))
	    (cont (car set))
	    (loop (cdr set))))))