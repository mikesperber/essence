(define (lookahead-matches? k lookahead input)
  (let loop ((k k) (lookahead lookahead) (input input))
    (cond
     ((zero? k) #t)
     ((null? lookahead) (stream-empty? input))
     ((stream-empty? input) #f)
     ((equal? (car lookahead) (car (stream-car input)))
      (loop (- k 1) (cdr lookahead) (stream-cdr input)))
     (else #f))))

(define-without-memoization
  (select-lookahead-item item-set k input cont fail)
  (let loop ((item-set item-set))
    (if (null? item-set)
	(fail)
	(let ((item (car item-set)))
	  (if (lookahead-matches? k (item-lookahead item) input)
	      (cont item)
	      (loop (cdr item-set)))))))
