(define-without-memoization
  (find-lookahead-item item-set k input)
  (let loop ((lookahead-sets+items (items->lookahead-sets+items item-set))
	     (remaining k)
	     (input input))
    (_memo2
     (cond
      ((null? lookahead-sets+items)
       #f)
      ((zero? remaining)
       (cdar lookahead-sets+items))
      ((and (not (= k remaining))
	    (stream-empty? input))	; we covered this previously
       (let ((empties
	      (filter (lambda (lookahead-set+item)
			(memq '() (car lookahead-set+item)))
		      lookahead-sets+items)))
	 (if (null? empties)
	     #f
	     (cdar empties))))
      (else
       (loop (filter-lookahead-sets+items lookahead-sets+items
					  (car (stream-car input))
					  remaining)
	     (- remaining 1)
	     (stream-cdr input)))))))

(define (filter-lookahead-sets+items lookahead-sets+items terminal remaining)
  (let loop ((lookahead-sets+items lookahead-sets+items))
    (if (not (null? lookahead-sets+items))
	(let* ((lookahead-set+item (car lookahead-sets+items))
	       (lookahead-set (car lookahead-set+item))
	       (non-empties (uniq (filter (lambda (lookahead)
					    (pair? lookahead))
					  lookahead-set))))
	  (cond
	   ((null? non-empties)
	    (loop (cdr lookahead-sets+items)))
	   ((> remaining 1)		; general case
	    (let* ((static-terminal (maybe-the-member terminal non-empties))
		   (matches (filter (lambda (lookahead)
				      (eqv? static-terminal (car lookahead)))
				    non-empties)))
	      (if (null? matches)
		  (loop lookahead-sets+items)
		  (cons (cons (map (lambda (lookahead)
				     (cdr lookahead))
				   matches)
			      (cdr lookahead-set+item))
			(loop (cdr lookahead-sets+items))))))
	   (else			; remaining = 1
	    (let ((one-lookaheads (map (lambda (lookahead)
					 (car lookahead))
				       non-empties)))
	      (if (memv terminal one-lookaheads)
		  (cons (cons '(()) (cdr lookahead-set+item))
			(loop (cdr lookahead-sets+items)))
		  (loop (cdr lookahead-sets+items)))))))
	'())))

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

