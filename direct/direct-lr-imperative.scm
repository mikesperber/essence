;; Essential LR parsing (direct style)
;; ===================================

(define-primitive error - error)

(define *lhs* #f)
(define *input* #f)

(define-without-memoization
  (ds-parse grammar k compute-closure state input)
  (_memo
   (let ((closure (compute-closure state grammar k)))

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

(define-without-memoization
  (ds-parse-bar grammar k compute-closure closure symbol input)
  (_memo
   (let ((the-next-nonterminals (next-nonterminals closure grammar))
	 (dot (ds-parse grammar k compute-closure
			(goto closure symbol) input)))
     (cond
      ((null? the-next-nonterminals)
       (- dot 1))
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
		     (the-member *lhs* the-next-nonterminals)
		     *input*))))))

(define (parse grammar k method input)
  (let ((start-production (grammar-start-production grammar)))

    (ds-parse grammar
	      k
	      (if (equal? method 'lr)
		  (lambda (state grammar k)
		    (compute-lr-closure state grammar k))
		  (lambda (state grammar k)
		    (compute-slr-closure state grammar k)))
	      (list (make-item start-production 0 '()))
	      input)))

