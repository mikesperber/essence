;; Essential LR parsing (direct style)
;; ===================================

(define (ds-parse grammar k first-map state input)
  (let ((closure
	 (compute-lr-closure state grammar k first-map)))

    (cond
     ((and (not (stream-empty? input))
	   (member (car (stream-car input))
		   (next-terminals closure grammar)))
      (ds-parse-bar grammar k first-map closure
		    (car (stream-car input)) (stream-cdr input)))
     ((find-lookahead-item (accept closure) k input)
      => (lambda (item)
	   (let ((rhs-length (length (item-rhs item)))
		 (lhs (item-lhs item)))
	     (if (zero? rhs-length)
		 (ds-parse-bar grammar k first-map
			       lhs input)
		 (parse-result lhs rhs-length input)))))
     (else (_error "parse error")))))

(define (ds-parse-bar grammar k first-map state symbol input)
  (let* ((closure
	  (compute-lr-closure state grammar k first-map))
	 (result (ds-parse grammar k first-map
			   (goto closure symbol) input))
	 (lhs (result-lhs result))
	 (dot (result-dot result))
	 (input (result-input result)))

    (cond
     ((> dot 1)
      (parse-result lhs (- dot 1) input))
     ((and (initial? state grammar)
	   (equal? (grammar-start grammar) lhs))
      'accept)
     (else
      (ds-parse-bar grammar k first-map
		    state lhs input)))))

(define (parse-result lhs dot inp)
  (vector lhs dot inp))
(define (result-lhs result)
  (vector-ref result 0))
(define (result-dot result)
  (vector-ref result 1))
(define (result-input result)
  (vector-ref result 2))

(define (parse grammar k input)
  (let ((start-production (grammar-start-production grammar))
	(first-map (compute-first grammar k)))
    
    (ds-parse grammar k (compute-first grammar k)
	      (list (make-item start-production 0 '()))
	      input)))

