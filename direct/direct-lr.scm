;; Essential LR parsing (direct style)
;; ===================================

(define (parse grammar k first-map state input)
  (_memo
   (let ((closure (compute-closure state grammar k first-map)))

     (if (final? state grammar)
	 (if (equal? '$ (car input))
	     'accept
	     (_error "parse error"))
	 (the-trick
	  (car input) (next-terminals closure grammar)
	  (lambda (symbol)
	    (parse-bar grammar k first-map closure
		       symbol (cdr input)))
	  (lambda ()
	    (select-lookahead-item
	     (accept closure) k input
	     (lambda (item)
	       (let* ((rhs-length (length (item-rhs item)))
		      (lhs (item-lhs item)))
		 (if (zero? rhs-length)
		     (parse-bar grammar k first-map closure lhs input)
		     (parse-result lhs rhs-length input))))
	     (lambda ()
	       (_error "parse error")))))))))

(define (parse-bar grammar k first-map closure symbol input)
  (let* ((next-state (goto closure symbol))
	 (result (parse grammar k first-map next-state input)))
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
		    (parse-bar grammar k first-map closure 
			       symbol inp)))))))))

;; ~~~~~~~~~~~~

(define (do-parse source-grammar k input)
  (let* ((grammar (source-grammar->grammar source-grammar k))
	 (start-production (car (grammar-productions grammar))))
    (parse grammar
	   k
	   (compute-first grammar k)
	   (list
	    (make-item start-production
		       0
		       (cdr (production-rhs start-production))))
	   (append input (make-$ k)))))

