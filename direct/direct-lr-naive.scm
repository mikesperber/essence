;; Essential LR parsing (direct style)
;; ===================================

(define (ds-parse grammar k compute-closure state input)
  (let ((closure (compute-closure state)))

    (cond
     ((and (not (stream-empty? input))
	   (member (car (stream-car input))
		   (next-terminals closure grammar)))
      (ds-parse-bar grammar k compute-closure closure
		    (car (stream-car input)) (stream-cdr input)))
     ((find-lookahead-item (accept closure) k input)
      => (lambda (item)
	   (let ((rhs-length (length (item-rhs item)))
		 (lhs (item-lhs item)))
	     (if (zero? rhs-length)
		 (ds-parse-bar grammar k compute-closure
			       lhs input)
		 (parse-result lhs rhs-length input)))))
     (else (_error "parse error")))))

(define (ds-parse-bar grammar k compute-closure state symbol input)
  (let* ((closure (compute-closure state))
	 (result (ds-parse grammar k compute-closure
			   (goto closure symbol) input))
	 (lhs (result-lhs result))
	 (dot (result-dot result))
	 (input (result-input result)))

    (cond
     ((> dot 1)
      (parse-result lhs (- dot 1) input))
     ((and (initial? state grammar)
	   (equal? (grammar-start grammar) lhs))
      (if (stream-empty? input)
	  'accept
	  (_error "parse error")))
     (else
      (ds-parse-bar grammar k compute-closure
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
  (let ((first-map (compute-first grammar k)))
    (ds-parse grammar k
	      (lambda (state)
		(compute-lr-closure state grammar k first-map))
	      (list (make-item (grammar-start-production grammar) 0 '()))
	      input)))

