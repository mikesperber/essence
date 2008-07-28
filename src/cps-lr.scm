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
	      (trace-reduce trace-level closure nonterminal attribute-value input grammar))
	  (if (and (initial? state grammar)
		   (equal? (grammar-start grammar) nonterminal))
	      (if (input-null? input)
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
     (define (handle-error-here error-status _closure _symbol start-input)
       (let* ((next-state (goto closure (grammar-error grammar)))
	      (keep (- (active next-state) 1))
	      (next-closure (compute-closure next-state grammar k))
	      (next-accept-items (accept next-closure)))

	 (let loop ((input start-input))

	   (define (recover)
	     (parse-error "trying to recover from parse error"
			  trace-closure error-status #t #f start-input)
	     (cps-parse grammar k compute-closure
			next-state
			(c-cons (and (not (null? the-next-nonterminals))
				     shift-nonterminal)
				(c-take keep continuations))
			(c-cons #f
				(c-take keep attribute-values))
			handle-error-here 0
			trace-level
			input))

	   (_memo
	    (cond
	     ((input-null? input)
	      (if (find-eoi-lookahead-item next-accept-items)
		  (recover)
		  (parse-error "parse error: premature end of input"
			       trace-closure error-status #f #f '())))
	     ((or (let ((terminals (next-terminals next-closure grammar)))
		    (and (pair? terminals)
			 ;; avoid touching the input if there aren't any terminals
			 (maybe-the-member (car (input-car input)) terminals)))
		  (find-lookahead-item next-accept-items k input))
	      (recover))
	     (else (loop (input-cdr input))))))))

     ;; normal operation
     (define (reduce item)
       (let* ((rhs-length (length (item-rhs item)))
	      (attribution (production-attribution (item-production item)))
	      (lhs (item-lhs item)))
	 (_memo
	  ((c-list-ref (c-cons (and (not (null? the-next-nonterminals))
				    shift-nonterminal)
			       continuations)
		       rhs-length)
	   lhs
	   (apply-attribution attribution
			      (c-take rhs-length attribute-values))
	   error-status
	   input))))

     (if (>= trace-level 2)
	 (trace-enter trace-level closure input grammar))

     (trace-state trace-level closure grammar)

     (check-for-reduce-reduce-conflict closure accept-items grammar k)
     (check-for-shift-reduce-conflict closure accept-items grammar k)

     (let ((handle-error (if (handles-error? closure grammar)
			     handle-error-here
			     handle-error))
	   (maybe-shift-nonterminal
	    (and (not (null? the-next-nonterminals))
		 shift-nonterminal)))
       (if (input-null? input)
	   (cond
	    ((find-eoi-lookahead-item accept-items) => reduce)
	    (else (handle-error error-status trace-closure #f input)))
	   (let ((terminals (next-terminals closure grammar)))
	     (cond
	      ((pair? terminals)
	       ;; avoid touching (input-car input) twice
	       (let ((first (input-car input)))
		 (cond
		  ((maybe-the-member (car first) terminals)
		   => (lambda (symbol)
			(if (>= trace-level 2)
			    (trace-shift trace-level closure symbol grammar))
			(shift symbol maybe-shift-nonterminal (cdr first)
			       handle-error
			       (_memo (and error-status ; prevent contaxt propagation
					   (+ error-status 1)))
			       (input-cdr input))))
		  ((find-lookahead-item accept-items k input) => reduce)
		  (else
		   (handle-error error-status trace-closure #f input)))))
	      ((find-lookahead-item accept-items k input) => reduce)
	      (else
	       (handle-error error-status trace-closure #f input)))))))))

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
				  error-status #f symbol input))
		   #f)
	       #f
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

(define (apply-attribution a l)
  (apply (eval a (interaction-environment)) (c-list->list (c-reverse l))))
