;; Essential LR parsing (continuation-based)
;; =========================================

(define-data c-list
  (c-cons c-car c-cdr)
  (c-nil))

(define-primitive error - error)

(define *input* #f)
(define *error-status* #f)
(define *attribute-value* #f)

(define-without-memoization
  (cps-parse grammar k compute-closure state
	     continuations attribute-values
	     handle-error)
  (_memo
   (let* ((closure (compute-closure state grammar k))
	  (accept-items (accept closure))
	  (the-next-nonterminals (next-nonterminals closure grammar)))

     (define (shift symbol handle-error)
       (let* ((next-state (goto closure symbol))
	      (keep (- (active next-state) 1)))
	 (cps-parse grammar k compute-closure
		    next-state
		    (c-cons (and (not (null? the-next-nonterminals))
				 shift-nonterminal)
			    (c-take keep continuations))
		    (c-cons *attribute-value* (c-take keep attribute-values))
		    handle-error)))
     
     (define (shift-nonterminal nonterminal)
       (_memo
	(let ((handle-error (if (handles-error? closure grammar)
				handle-error-here
				handle-error)))
	  (if (and (initial? state grammar)
		   (equal? (grammar-start grammar) nonterminal))
	      (if (stream-empty? *input*)
		  *attribute-value*
		  (handle-error))
	      (shift
	       (the-member nonterminal the-next-nonterminals)
	       handle-error)))))

     ;; error recovery
     (define (handle-error-here)
       (let* ((next-state (goto closure (grammar-error grammar)))
	      (keep (- (active next-state) 1))
	      (next-closure (compute-closure next-state grammar k))
	      (next-accept-items (accept next-closure)))

	 (define (recover attribute-value)
	   (set! *error-status* 3)
	   (cps-parse grammar k compute-closure
		      next-state
		      (c-cons (and (not (null? the-next-nonterminals))
				   shift-nonterminal)
			      (c-take keep continuations))
		      (c-cons attribute-value
			      (c-take keep attribute-values))
		      handle-error-here))

	 (set! *input* (advance-input *error-status* *input*))

	 (let loop ()

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
	       (recover attribute-value)))

	   (_memo
	    (cond
	     ((stream-empty? *input*)
	      (cond
	       ((find-eoi-lookahead-item next-accept-items) => reduce-recover)
	       (else (error "parse error: premature end of input"))))
	     ((maybe-the-member (car (stream-car *input*))
				(next-terminals next-closure grammar))
	      => (lambda (symbol)
		   (recover (cdr (stream-car *input*)))))
	     ((find-lookahead-item next-accept-items k *input*)
	      => reduce-recover)
	     (else
	      (set! *input* (stream-cdr *input*))
	      (loop)))))))

     ;; normal operation
     (define (reduce item)
       (let* ((rhs-length (length (item-rhs item)))
	      (attribution (production-attribution
			    (item-production item))))
	 (set! *attribute-value*
	       (apply-attribution
		attribution
		(c-take rhs-length attribute-values)))

	 ((c-list-ref (c-cons (and (not (null? the-next-nonterminals))
				   shift-nonterminal)
			      continuations)
		      rhs-length)
	  (item-lhs item))))

     (check-for-reduce-reduce-conflict closure accept-items grammar k)
     (check-for-shift-reduce-conflict closure accept-items grammar k)

     (let ((handle-error (if (handles-error? closure grammar)
			     handle-error-here
			     handle-error)))
       (cond
	((stream-empty? *input*)
	 (cond
	  ((find-eoi-lookahead-item accept-items) => reduce)
	  (else (handle-error))))
	((maybe-the-member (car (stream-car *input*))
			   (next-terminals closure grammar))
	 => (lambda (symbol)
	      (_memo
	       (begin
		 (set! *attribute-value* (cdr (stream-car *input*)))
		 (set! *input* (stream-cdr *input*))
		 (set! *error-status* (move-error-status *error-status*))))
	      (shift symbol handle-error)))
	((find-lookahead-item accept-items k *input*) => reduce)
	(else (handle-error)))))))

(define (parse grammar k method input)
  (let ((start-production (grammar-start-production grammar)))
    (set! *input* input)
    (set! *error-status* 0)
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
		   (lambda (error-status) (error "unhandled parse error"))
		   #f))))

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
  (_memo
   (apply (eval a (interaction-environment)) (c-list->list (c-reverse l)))))
