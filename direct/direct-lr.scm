;; Essential LR parsing (direct style)
;; ===================================

(define (parse grammar k compute-closure state input)
  (_memo
   (let ((closure (compute-closure state grammar)))

     (if (final? state grammar)
	 (if (equal? (make-eoi-terminal) (car input))
	     'accept
	     (_error "parse error"))
	 (the-trick
	  (car input) (next-terminals closure grammar)
	  (lambda (symbol)
	    (parse-bar grammar k compute-closure closure
		       symbol (cdr input)))
	  (lambda ()
	    (select-lookahead-item
	     (accept closure) k input
	     (lambda (item)
	       (let* ((rhs-length (length (item-rhs item)))
		      (lhs (item-lhs item)))
		 (if (zero? rhs-length)
		     (parse-bar grammar k compute-closure closure lhs input)
		     (parse-result lhs rhs-length input))))
	     (lambda ()
	       (_error "parse error")))))))))

(define (parse-bar grammar k compute-closure closure symbol input)
  (let* ((next-state (goto closure symbol))
	 (result (parse grammar k compute-closure next-state input)))
    (if (final? next-state grammar)
	result
	(let* ((lhs (result-lhs result))
	       (dot (result-dot result))
	       (inp (result-inp result))
	       (nts (next-nonterminals closure grammar)))
	  (cond ((null? nts) (parse-result lhs (- dot 1) inp))
		((> dot 1) (parse-result lhs (- dot 1) inp))
		(else
		 (the-trick-cannot-fail
		  lhs (next-nonterminals closure grammar)
		  (lambda (symbol)
		    (parse-bar grammar k compute-closure closure 
			       symbol inp)))))))))

;; ~~~~~~~~~~~~

(define (do-parse source-grammar k method input)
  (let* ((grammar (source-grammar->grammar source-grammar k))
	 (start-production (grammar-start-production grammar))
	 (first-map (compute-first grammar k)))

    (cond
     ((equal? method 'lr)
      (parse grammar
	     k
	     (lambda (state grammar)
	       (compute-lr-closure state grammar k first-map))
	     (list (make-item start-production
			      0
			      (cdr (production-rhs start-production))))
	     input))
     ((equal? method 'slr)
      (let ((follow-map (compute-follow grammar k first-map)))
	(parse grammar
	       k
	       (lambda (state grammar)
		 (compute-slr-closure state grammar k follow-map))
	       (add-slr-lookahead (list (make-item start-production 0 #f))
				  follow-map)
	       input))))))

