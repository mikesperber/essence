;; Essential LR parsing (continuation-based)
;; =========================================

(define (cps-parse grammar state continuations input)
  (let ((closure (compute-lr-closure state grammar 0 '())))

    (define (c0 symbol input)
      (cond
       ((not (and (initial? state grammar)
		  (equal? (grammar-start grammar) symbol)))
	(cps-parse grammar
		   (goto closure symbol)
		   (cons c0
			 (take (- (active (goto closure symbol)) 1)
			       continuations))
		   input))
       (else 'accept)))

    (cond
     ((and (not (stream-empty? input))
	   (member (car (stream-car input))
		   (next-terminals closure grammar)))
      (c0 (car (stream-car input)) (stream-cdr input)))
     ((not (null? (accept closure)))
      (let ((item (car (accept closure))))
	((list-ref (cons c0 continuations)
		   (length (item-rhs item)))
	 (item-lhs item) input)))
     (else 'error))))

(define (parse grammar input)
  (cps-parse
   grammar
   (list (make-item (grammar-start-production grammar) 0 '()))
   '()
   input))

(define (take n l)
  (if (zero? n)
      '()
      (cons (car l) (take (- n 1) (cdr l)))))

