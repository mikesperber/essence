(define-without-memoization
  (find-lookahead-item item-set k input)
  (let loop ((lookaheads+items (map (lambda (item)
				      (cons (item-lookahead item)
					    item))
				    item-set))
	     (remaining k)
	     (input input))
    (_memo2
     (cond
      ((null? lookaheads+items)
       #f)
      ((zero? remaining)
       (cdar lookaheads+items))
      ((and (not (= k remaining))
	    (stream-empty? input))	; we covered this previously
       (let ((empties
	      (filter (lambda (lookahead+item)
			(null? (car lookahead+item)))
		      lookaheads+items)))
	 (if (null? empties)
	     #f
	     (cdar empties))))
      (else
       (loop (filter-lookaheads+items lookaheads+items
				      (car (stream-car input)))
	     (- remaining 1)
	     (stream-cdr input)))))))

(define (filter-lookaheads+items lookaheads+items terminal)
  (let* ((non-empties (filter (lambda (lookahead+item)
				(not (null? (car lookahead+item)))) 
			      lookaheads+items))
	 (one-lookaheads (uniq (map (lambda (lookahead+item)
				      (car (car lookahead+item)))
				    non-empties)))
	 (static-terminal (maybe-the-member terminal 
					    one-lookaheads))
	 (matches
	  (filter
	   (lambda (lookahead+item)
	     (eqv? static-terminal (caar lookahead+item)))
	   non-empties)))

    (map (lambda (lookahead+item)
	   (cons (cdr (car lookahead+item))
		 (cdr lookahead+item)))
	 matches)))

(define (filter pred l)
  (let loop ((l l) (r '()))
    (cond ((null? l)
	   (reverse r))
	  ((pred (car l))
	   (loop (cdr l) (cons (car l) r)))
	  (else
	   (loop (cdr l) r)))))

(define (uniq l)
  (let loop ((l l) (r '()))
    (if (null? l)
	(reverse r)
	(loop (cdr l)
	      (if (member (car l) r)
		  r
		  (cons (car l) r))))))
