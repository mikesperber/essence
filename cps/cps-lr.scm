;; Essential LR parsing (continuation-based)
;; =========================================

(define-data c-list
  (c-cons c-car c-cdr)
  (c-nil))

(define-memo _memo 1)

(define
  (cps-parse grammar k compute-closure state continuations input)
  (_memo
   (let* ((closure (compute-closure state))
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
       (cond
	((not (and (initial? state grammar)
		   (equal? (grammar-start grammar) nonterminal)))
	 (shift
	  (the-member nonterminal the-next-nonterminals)
	  input))
	((stream-empty? input) 'accept)
	(else 'error)))

     (define (reduce)
       (cond
	((find-lookahead-item (accept closure) k input)
	 => (lambda (item)
	      ((c-list-ref (c-cons (and (not (null? the-next-nonterminals))
					shift-nonterminal)
				   continuations)
			   (length (item-rhs item)))
	       (item-lhs item) input)))
	(else 'error)))

     (cond
      ((stream-empty? input) (reduce))
      ((maybe-the-member (car (stream-car input))
			 (next-terminals closure grammar))
       => (lambda (symbol)
	    (shift symbol (stream-cdr input))))
      (else (reduce))))))

(define (parse grammar k method input)
  (let ((start-production (grammar-start-production grammar))
	(first-map (compute-first grammar k)))

    (cps-parse
     grammar
     k
     (if (equal? method 'lr)
	 (lambda (state)
	   (compute-lr-closure state grammar k first-map))
	 (let ((follow-map (compute-follow grammar k first-map)))
	   (lambda (state)
	     (compute-slr-closure state grammar k follow-map))))
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
