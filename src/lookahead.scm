(define-without-memoization
  (lookahead-matches? k lookahead input)
  (let loop ((k k) (lookahead lookahead) (input input))
    (cond
     ((zero? k) #t)
     ((null? lookahead) (null? input))
     ((null? input) #f)
     ((equal? (car lookahead) (car (car input)))
      (loop (- k 1) (cdr lookahead) (cdr input)))
     (else #f))))

(define-without-memoization
  (find-lookahead-item item-set k input)
  (let loop ((item-set item-set))
    (_memo2
     (if (null? item-set)
	 #f
	 (let ((item (car item-set)))
	   (if (lookahead-matches? k (item-lookahead item) input)
	       item
	       (loop (cdr item-set))))))))

