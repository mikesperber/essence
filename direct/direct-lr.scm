;; Essential LR parsing (direct style)
;; ===================================

(define-memo _memo 1)

(define (ds-parse grammar k compute-closure state input)
  (_memo
   (let ((closure (compute-closure state grammar)))

     (define (reduce)
       (cond
	((find-lookahead-item (accept closure) k input)
	 => (lambda (item)
	      (let ((rhs-length (length (item-rhs item)))
		    (lhs (item-lhs item)))
		(if (zero? rhs-length)
		    (ds-parse-bar grammar k compute-closure
				  closure lhs input)
		    (parse-result lhs rhs-length input)))))
	(else (_error "parse error"))))

     (cond
      ((stream-empty? input) (reduce))
      ((maybe-the-member (car (stream-car input))
			 (next-terminals closure grammar))
       => (lambda (symbol)
	    (ds-parse-bar grammar k compute-closure closure
			  symbol (stream-cdr input))))
      (else (reduce))))))

(define (ds-parse-bar grammar k compute-closure closure symbol input)
  (if (and (initial? closure grammar)
	   (equal? (grammar-start grammar) symbol))
      'accept
      (let* ((result (ds-parse grammar k compute-closure
			       (goto closure symbol) input))
	     (lhs (result-lhs result))
	     (dot (result-dot result))
	     (input (result-input result)))

	(cond
	 ((> dot 1)
	  (parse-result lhs (- dot 1) input))
	 ((and (initial? closure grammar)
	       (equal? (grammar-start grammar) lhs))
	  'accept)
	 (else
	  (ds-parse-bar grammar k compute-closure
			closure
			(the-member lhs (next-nonterminals closure grammar))
			input))))))

(define (parse-result lhs dot inp)
  (vector lhs dot inp))
(define (result-lhs result)
  (vector-ref result 0))
(define (result-dot result)
  (vector-ref result 1))
(define (result-input result)
  (vector-ref result 2))

(define (parse grammar k method input)
  (let ((start-production (grammar-start-production grammar))
	(first-map (compute-first grammar k)))

    (ds-parse grammar
	      k
	      (if (equal? method 'lr)
		  (lambda (state grammar)
		    (compute-lr-closure state grammar k first-map))
		  (let ((follow-map (compute-follow grammar k first-map)))
		    (lambda (state grammar)
		      (compute-slr-closure state grammar k follow-map))))
	      (list (make-item start-production 0 '()))
	      input)))

