;; Essential LR parsing (continuation-based)
;; =========================================

(define-data c-list
  (c-cons c-car c-cdr)
  (c-nil))

(define-memo _memo 1)

(define
  (cps-parse grammar k compute-closure state continuations input)
  (_memo
   (if (final? state grammar)
       (if (equal? (grammar-eoi grammar) (car (stream-car input)))
	   'accept
	   'error)
       (let* ((closure (compute-closure state grammar))
	      (the-next-nonterminals (next-nonterminals closure grammar)))

	 (define (shift symbol input)
	   (let ((next-state (goto closure symbol)))
	     (cps-parse grammar k compute-closure
			next-state
			(c-cons (and (not (null? the-next-nonterminals))
				     shift-nonterminal)
				(c-take (- (active next-state) 1)
					continuations))
			input)))

	 (define (shift-nonterminal nonterminal input)
	   (the-trick-cannot-fail
	    nonterminal the-next-nonterminals
	    (lambda (symbol) (shift symbol input))))

	 (define (shift-terminal terminal input fail)
	   (the-trick
	    terminal (next-terminals closure grammar)
	    (lambda (symbol) (shift symbol input))
	    fail))

	 (shift-terminal
	  (car (stream-car input)) (stream-cdr input)
	  (lambda ()
	    (select-lookahead-item
	     (accept closure) k input
	     (lambda (item)
	       ((c-list-ref (c-cons (and (not (null? the-next-nonterminals))
					 shift-nonterminal)
				    continuations)
			    (length (item-rhs item)))
		(item-lhs item) input))
	     (lambda () 'lookahead-error))))))))

(define (parse grammar k method input)
  (let* ((start-production (grammar-start-production grammar))
	 (terminated-start-production
	  (make-production (production-lhs start-production)
			   (append (production-rhs start-production)
				   (make-list k (grammar-eoi grammar)))
			   (production-attribution start-production)))
	 (first-map (compute-first grammar k)))

    (cond
     ((equal? method 'lr)
      (cps-parse grammar
		 k
		 (lambda (state grammar)
		   (compute-lr-closure state grammar k first-map))
		 (list (make-item terminated-start-production
				  0
				  (cdr (production-rhs start-production))))
		 (c-nil)
		 input))
     ((equal? method 'slr)
      (let ((follow-map (compute-follow grammar k first-map)))
	(cps-parse grammar
		   k
		   (lambda (state grammar)
		     (compute-slr-closure state grammar k follow-map))
		   (add-slr-lookahead
		    (list (make-item terminated-start-production 0 #f))
		    follow-map)
		   (c-nil)
		   input))))))

(define (c-take n l)
  (if (zero? n)
      (c-nil)
      (c-cons (c-car l) (c-take (- n 1) (c-cdr l)))))

(define (c-list-ref l n)
  (if (zero? n)
      (c-car l)
      (c-list-ref (c-cdr l) (- n 1))))
