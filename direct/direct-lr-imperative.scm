;; Essential LR parsing (direct style)
;; ===================================

(define-memo _memo 1)
(define-primitive error - error)

(define *lhs* #f)
(define *input* #f)

(define (ds-parse grammar k compute-closure state input)
  (_memo
   (let ((closure (compute-closure state)))

     (define (reduce)
       (cond
	((find-lookahead-item (accept closure) k input)
	 => (lambda (item)
	      (let ((rhs-length (length (item-rhs item)))
		    (lhs (item-lhs item)))
		(if (zero? rhs-length)
		    (ds-parse-bar grammar k compute-closure
				  closure lhs input)
		    (begin
		      (set! *lhs* lhs)
		      (set! *input* input)
		      rhs-length)))))
	(else (error "parse error"))))

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
      (let ((dot (ds-parse grammar k compute-closure
			   (goto closure symbol) input)))
	(cond
	 ((> dot 1)
	  (- dot 1))
	 ((and (initial? closure grammar)
	       (equal? (grammar-start grammar) *lhs*))
	  (if (stream-empty? *input*)
	      'accept
	      (error "parse error")))
	 (else
	  (ds-parse-bar grammar k compute-closure
			closure
			(the-member *lhs* (next-nonterminals closure grammar))
			*input*))))))

(define (parse grammar k method input)
  (let ((start-production (grammar-start-production grammar)))

    (ds-parse grammar
	      k
	      (if (equal? method 'lr)
		  (lambda (state)
		    (compute-lr-closure state grammar k))
		  (lambda (state)
		    (compute-slr-closure state grammar k)))
	      (list (make-item start-production 0 '()))
	      input)))

