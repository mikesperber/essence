;; Essential LR parsing (direct style)
;; ===================================

(define-memo _memo 1)
(define-primitive error - error)

(define *lhs* #f)
(define *input* #f)

(define-without-memoization
  (ds-parse grammar k compute-closure state)
  (_memo
   (let* ((closure (compute-closure state grammar k))
	  (accept-items (accept closure)))

     (define (reduce item)
       (let ((rhs-length (length (item-rhs item)))
	     (lhs (item-lhs item)))
	 (if (zero? rhs-length)
	     (ds-parse-bar grammar k compute-closure
			   closure lhs)
	     (begin
	       (set! *lhs* lhs)
	       rhs-length))))

     (cond
      ((stream-empty? *input*)
       (cond
	((find-eoi-lookahead-item accept-items) => reduce)
	(else (error "parse error: unexpected EOI"))))
      ((maybe-the-member (car (stream-car *input*))
			 (next-terminals closure grammar))
       => (lambda (symbol)
	    (set! *input* (stream-cdr *input*))
	    (ds-parse-bar grammar k compute-closure closure
			  symbol)))
      ((find-lookahead-item accept-items k *input*) => reduce)
      (else (error "parse error"))))))

(define-without-memoization
  (ds-parse-bar grammar k compute-closure closure symbol)
  (_memo
   (let ((the-next-nonterminals (next-nonterminals closure grammar))
	 (dot (ds-parse grammar k compute-closure
			(goto closure symbol))))
     (cond
      ((null? the-next-nonterminals)
       (- dot 1))
      ((> dot 1)
       (- dot 1))
      ((and (initial? closure grammar)
	    (eqv? (grammar-start grammar) *lhs*))
       (if (stream-empty? *input*)
	   'accept
	   (error "parse error")))
      (else
       (ds-parse-bar grammar k compute-closure
		     closure
		     (the-member *lhs* the-next-nonterminals)))))))

(define (parse grammar k method input)
  (let ((start-production (grammar-start-production grammar)))
    (set! *input* input)
    (ds-parse grammar
	      k
	      (if (equal? method 'lr)
		  (lambda (state grammar k)
		    (compute-lr-closure state grammar k))
		  (lambda (state grammar k)
		    (compute-slr-closure state grammar k)))
	      (list (make-item start-production 0 '())))))

