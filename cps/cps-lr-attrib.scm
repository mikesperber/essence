;; Essential LR parsing (continuation-based)
;; =========================================

(define-data c-list
  (c-cons c-car c-cdr)
  (c-nil))

(define-memo _memo 1)
(define-primitive error - error)

(define
  (cps-parse grammar k compute-closure state
	     continuations attribute-values
	     input)
  (_memo
   (let* ((closure (compute-closure state grammar k))
	  (accept-items (accept closure))
	  (the-next-nonterminals (next-nonterminals closure grammar)))

     (define (shift symbol attribute-value input)
       (let* ((next-state (goto closure symbol))
	      (keep (- (active next-state) 1)))
	 (cps-parse grammar k compute-closure
		    next-state
		    (c-cons (and (not (null? the-next-nonterminals))
				 shift-nonterminal)
			    (c-take keep continuations))
		    (c-cons attribute-value (c-take keep attribute-values))
		    input)))
     
     (define (shift-nonterminal nonterminal attribute-value input)
       (if (and (initial? state grammar)
		(equal? (grammar-start grammar) nonterminal))
	   (if (stream-empty? input)
	       attribute-value
	       (error "parse error"))
	   (shift
	    (the-member nonterminal the-next-nonterminals)
	    attribute-value
	    input)))

     (define (reduce item)
       (let* ((rhs-length (length (item-rhs item)))
	      (attribution (production-attribution
			    (item-production item)))
	      (attribute-value
	       (apply-attribution
		attribution
		(c-list->list
		 (c-reverse
		  (c-take rhs-length attribute-values))))))

	 ((c-list-ref (c-cons (and (not (null? the-next-nonterminals))
				   shift-nonterminal)
			      continuations)
		      rhs-length)
	  (item-lhs item) attribute-value
	  input)))

     (cond
      ((stream-empty? input)
       (cond
	((find-eoi-lookahead-item accept-items) => reduce)
	(else (error "parse error"))))
      ((maybe-the-member (car (stream-car input))
			 (next-terminals closure grammar))
       => (lambda (symbol)
	    (shift symbol (cdr (stream-car input)) (stream-cdr input))))
      ((find-lookahead-item accept-items k input) => reduce)
      (else (error "parse error"))))))

(define (parse grammar k method input)
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
  (apply (eval a (interaction-environment)) l))
