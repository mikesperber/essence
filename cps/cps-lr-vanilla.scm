;; Essential LR parsing (continuation-based)
;; =========================================

(define-data c-list
  (c-cons c-car c-cdr)
  (c-nil))

(define-memo _memo 1)

(define-without-memoization
  (cps-parse grammar k compute-closure state continuations input)
  (_memo
   (let* ((closure (compute-closure state grammar k))
	  (accept-items (accept closure))
	  (the-next-nonterminals (next-nonterminals closure grammar)))

     (define (shift symbol input)
       (let ((next-state (goto closure symbol)))
	 (cps-parse grammar k compute-closure
		    next-state
		    (c-cons (and (not (null? the-next-nonterminals))
				 shift-nonterminal)
			    (c-take (- (active next-state) 1)
				    continuations))
		    input)))
     
     (define (shift-nonterminal nonterminal input)
       (_memo
	(if (and (initial? state grammar)
		 (equal? (grammar-start grammar) nonterminal))
	    (if (stream-empty? input)
		'accept
		'error)
	    (shift
	     (the-member nonterminal the-next-nonterminals)
	     input))))

     (define (reduce item)
       ((c-list-ref (c-cons (and (not (null? the-next-nonterminals))
				 shift-nonterminal)
			    continuations)
		    (length (item-rhs item)))
	(item-lhs item) input))

     (cond
      ((stream-empty? input)
       (cond
	((find-eoi-lookahead-item accept-items) => reduce)
	(else 'error)))
      ((maybe-the-member (car (stream-car input))
			 (next-terminals closure grammar))
       => (lambda (symbol)
	    (shift symbol (stream-cdr input))))
      ((find-lookahead-item accept-items k input) => reduce)
      (else 'error)))))

(define (parse grammar k method input)
  (let ((start-production (grammar-start-production grammar)))

    (cps-parse
     grammar
     k
     (if (equal? method 'lr)
	 (lambda (state grammar k)
	   (compute-lr-closure state grammar k))
	 (lambda (state grammar k)
	   (compute-slr-closure state grammar k)))
     (list (make-item start-production 0 '()))
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
