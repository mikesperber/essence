;; Essential LR parsing (direct style)
;; ===================================

(define (parse grammar k first-map state input)
  (let ((closure (compute-closure state grammar k first-map)))

    (if (and (final? state grammar)
	     (equal? '$ (car input)))
	'accept
	(or (parse-bar grammar k first-map closure
		       (car input) (cdr input))
	    (select-lookahead-item
	     (accept closure) k input
	     (lambda (item)
	       (let* ((rhs-length (length (item-rhs item)))
		      (lhs (item-lhs item)))
		 (if (zero? rhs-length)
		     (parse-bar grammar k first-map closure lhs input)
		     (parse-result lhs rhs-length input))))
	     (lambda ()
	       (error "parse error")))))))

(define (parse-bar grammar k first-map closure symbol input)
  (let* ((next-state (goto closure symbol)))
    (and (not (null? next-state))
	 (let ((result
		(parse grammar k first-map next-state input)))
	   (if (final? next-state grammar)
	       result
	       (let* ((lhs (result-lhs result))
		      (dot (result-dot result))
		      (inp (result-inp result)))
		 (if (> dot 1)
		     (parse-result lhs (- dot 1) inp)
		     (parse-bar grammar k first-map closure lhs inp))))))))

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

