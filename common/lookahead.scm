(define (in-take n l)
  (if (zero? n)
      '()
      (cons (car l) (in-take (- n 1) (cdr l)))))

(define (first p l)
  (cond ((null? l) #f)
	((p (car l) (car l)))
	(else (first p (cdr l)))))

(define (select-lookahead-item item-set k input cont fail)
  (let* ((input-front (in-take k input))
	 (item (first (lambda (item)
			(equal? input-front (item-lookahead item)))
		      item-set)))
    (if item
	(cont item)
	(fail))))