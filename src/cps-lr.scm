;; Essential LR parsing (continuation-based)
;; =========================================

(define-data c-list
  (c-cons c-car c-cdr)
  (c-nil))

(define-primitive parse-error - dynamic)

(define-without-memoization
  (cps-parse grammar k compute-closure state
	     continuations attribute-values
	     handle-error error-status
	     trace-level
	     input)
  (_memo
   (let* ((closure (compute-closure state grammar k))
	  (trace-closure (if (>= trace-level 1)
			     closure
			     #f))
	  (accept-items (accept closure))
	  (the-next-nonterminals (next-nonterminals closure grammar)))

     (define (shift symbol shift-nonterminal attribute-value handle-error error-status input)
       (let* ((next-state (goto closure symbol))
	      (keep (- (active next-state) 1)))
	 (cps-parse grammar k compute-closure
		    next-state
		    (c-cons shift-nonterminal
			    (c-take keep continuations))
		    (c-cons attribute-value (c-take keep attribute-values))
		    handle-error error-status
		    trace-level
		    input)))
     
     (define (shift-nonterminal nonterminal attribute-value error-status input)
       (_memo
	(let ((handle-error (if (handles-error? closure grammar)
				handle-error-here
				handle-error))
	      (shift-nonterminal shift-nonterminal))
	  (if (>= trace-level 2)
	      (trace-reduce trace-level closure nonterminal input grammar))
	  (if (and (initial? state grammar)
		   (equal? (grammar-start grammar) nonterminal))
	      (if (null? input)
		  attribute-value
		  (handle-error error-status trace-closure nonterminal input))
	      (shift
	       (the-member nonterminal the-next-nonterminals)
	       shift-nonterminal
	       attribute-value
	       handle-error
	       error-status
	       input)))))

     ;; error recovery
     (define (handle-error-here error-status _closure _symbol input)
       (let* ((next-state (goto closure (grammar-error grammar)))
	      (keep (- (active next-state) 1))
	      (next-closure (compute-closure next-state grammar k))
	      (next-accept-items (accept next-closure))
	      (input
	       (cond
		((zero? error-status) input)
		((null? input)
		 (parse-error "parse error: premature end of input" closure #f '()))
		(else (cdr input)))))

	 (define (recover attribute-value input)
	   (cps-parse grammar k compute-closure
		      next-state
		      (c-cons (and (not (null? the-next-nonterminals))
				   shift-nonterminal)
			      (c-take keep continuations))
		      (c-cons attribute-value
			      (c-take keep attribute-values))
		      handle-error-here 3
		      trace-level
		      input))

	 (let loop ((input input))

	   (define (reduce-recover item)
	     (let* ((rhs-length (length (item-rhs item)))
		    (attribution (production-attribution
				  (item-production item)))
		    (attribute-value
		     (apply-attribution
		      attribution
		      (c-cons #f
			      (c-take (- rhs-length 1)
				      attribute-values)))))
	       (recover attribute-value input)))

	   (_memo
	    (cond
	     ((null? input)
	      (cond
	       ((find-eoi-lookahead-item next-accept-items) => reduce-recover)
	       (else (parse-error "parse error: premature end of input"
				  trace-closure #f '()))))
	     ((maybe-the-member (car (car input))
				(next-terminals next-closure grammar))
	      => (lambda (symbol)
		   (recover (cdr (car input)) input)))
	     ((find-lookahead-item next-accept-items k input)
	      => reduce-recover)
	     (else (loop (cdr input))))))))

     ;; normal operation
     (define (reduce item)
       (let* ((rhs-length (length (item-rhs item)))
	      (attribution (production-attribution
			    (item-production item)))
	      (attribute-value
	       (apply-attribution
		attribution
		(c-take rhs-length attribute-values))))

	 ((c-list-ref (c-cons (and (not (null? the-next-nonterminals))
				   shift-nonterminal)
			      continuations)
		      rhs-length)
	  (item-lhs item) attribute-value
	  error-status
	  input)))

     (if (>= trace-level 2)
	 (trace-state trace-level closure input grammar))

     (check-for-reduce-reduce-conflict closure accept-items grammar k)
     (check-for-shift-reduce-conflict closure accept-items grammar k)

     (let ((handle-error (if (handles-error? closure grammar)
			     handle-error-here
			     handle-error))
	   (maybe-shift-nonterminal
	    (and (not (null? the-next-nonterminals))
		 shift-nonterminal)))
       (cond
	((null? input)
	 (cond
	  ((find-eoi-lookahead-item accept-items) => reduce)
	  (else (handle-error error-status trace-closure #f input))))
	((maybe-the-member (car (car input))
			   (next-terminals closure grammar))
	 => (lambda (symbol)
	      (if (>= trace-level 2)
		  (trace-shift trace-level closure symbol grammar))
	      (shift symbol maybe-shift-nonterminal (cdr (car input))
		     handle-error
		     (if (zero? error-status)
			 error-status
			 (- error-status 1))
		     (cdr input))))
	((find-lookahead-item accept-items k input) => reduce)
	(else
	 (handle-error error-status trace-closure #f input)))))))

(define (parse grammar k method trace-level input)
  (let ((start-production (grammar-start-production grammar)))

    (cps-parse grammar
	       k
	       (if (equal? method 'lr)
		   (lambda (state grammar k)
		     (compute-lr-closure state grammar k))
		   (lambda (state grammar k)
		     (compute-slr-closure state grammar k)))
	       (list (make-item start-production 0 '()))
	       (c-nil)
	       (c-nil)
	       (if #t
		   (lambda (error-status closure symbol input)
		     (parse-error "unhandled parse error" closure
				  symbol input))
		   #f)
	       0
	       trace-level
	       input)))

(define (c-take n l)
  (if (zero? n)
      (c-nil)
      (c-cons (c-car l) (c-take (- n 1) (c-cdr l)))))

(define (c-list-ref l n)
  (if (zero? n)
      (c-car l)
      (c-list-ref (c-cdr l) (- n 1))))

(define (c-list->list l)
  (if (c-nil? l)
      '()
      (cons (c-car l) (c-list->list (c-cdr l)))))

(define (c-reverse l)
  (let loop ((l l) (r (c-nil)))
    (if (c-nil? l)
	r
	(loop (c-cdr l) (c-cons (c-car l) r)))))

(define-primitive apply - apply)
(define-primitive cons - pure)

(define (apply-attribution a l)
  (apply (eval a (interaction-environment)) (c-list->list (c-reverse l))))
