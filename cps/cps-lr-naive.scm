;; Essential LR parsing (continuation-based)
;; =========================================

(define (cps-parse grammar k compute-closure
		   state continuations input)
  (let ((closure (compute-closure state)))

    (define (c0 symbol input)
      (cond
       ((not (and (initial? state grammar)
		  (equal? (grammar-start grammar) symbol)))
	(cps-parse grammar k compute-closure
		   (goto closure symbol)
		   (cons c0
			 (take (- (active (goto closure symbol)) 1)
			       continuations))
		   input))
       ((stream-empty? input) 'accept)
       (else 'error)))

    (cond
     ((and (not (stream-empty? input))
	   (member (car (stream-car input))
		   (next-terminals closure grammar)))
      (c0 (car (stream-car input)) (stream-cdr input)))
     ((find-lookahead-item (accept closure) k input)
      => (lambda (item)
	   ((list-ref (cons c0 continuations)
		      (length (item-rhs item)))
	    (item-lhs item) input)))
     (else 'error))))

(define (parse grammar k input)
  (cps-parse grammar k 
	     (lambda (state)
	       (compute-lr-closure state grammar k))
	     (list (make-item (grammar-start-production grammar) 0 '()))
	     '()
	     input))

(define (take n l)
  (if (zero? n)
      '()
      (cons (car l) (take (- n 1) (cdr l)))))

