;; Essential LR parsing (continuation-based)
;; =========================================

(define-data c-list
  (c-cons c-car c-cdr)
  (c-nil))

(define (parse grammar k first-map state continuations input)
  (if (and (final? state grammar)
           (equal? '$ (car input)))
      'accept
      (let ((closure (compute-closure state grammar k first-map)))

	(define (shift-nonterminal nonterminal input)
	  (the-trick-cannot-fail
	   nonterminal (next-nonterminals closure grammar)
	   (lambda (symbol)
	     (let ((next-state (goto closure symbol)))
	       (parse grammar k first-map
		      next-state
		      (c-cons shift-nonterminal
			      (c-take (- (active next-state) 1)
				      continuations))
		      input)))))

	(define (shift-terminal terminal input fail)
	  (the-trick
	   terminal (next-terminals closure grammar)
	   (lambda (symbol)
	     (let ((next-state (goto closure symbol)))
	       (parse grammar k first-map
		      next-state
		      (c-cons shift-nonterminal
			      (c-take (- (active next-state) 1)
				      continuations))
		      input)))
	   fail))

        (let ((accept-items (accept closure)))
	  (shift-terminal
	   (car input) (cdr input)
	   (lambda ()
	     (select-lookahead-item-the-trick
	      accept-items k input
	      (lambda (item)
		((c-list-ref (c-cons shift-nonterminal continuations)
			     (length (item-rhs item)))
		 (item-lhs item) input))
	      (lambda () 'error))))))))

;; ~~~~~~~~~~~~

(define (do-parse source-grammar k input)
  (let* ((grammar (source-grammar->grammar source-grammar k))
	 (start-production
	   (car (grammar-productions grammar))))
    (parse grammar
	   k
	   (compute-first grammar k)
	   (list (make-item start-production
			    0
			    (cdr (production-rhs start-production))))
	   (c-nil)
	   (append input (make-$ k)))))

;; ~~~~~~~~~~~~

(define (the-trick element set cont fail)
  (let loop ((set set))
    (if (null? set)
	(fail)
	(if (equal? element (car set))
	    (cont (car set))
	    (loop (cdr set))))))

(define (the-trick-cannot-fail element set cont)
  (let loop ((set set))
    (if (null? (cdr set))
	(cont (car set))
	(if (equal? element (car set))
	    (cont (car set))
	    (loop (cdr set))))))

(define (select-lookahead-item-the-trick item-set k input cont fail)
  (let ((input-front (take k input)))
    (let loop ((item-set item-set))
      (if (null? item-set)
	  (fail)
	  (let ((item (car item-set)))
	    (if (equal? input-front (item-lookahead item))
		(cont item)
		(loop (cdr item-set))))))))

(define (c-take n l)
  (if (zero? n)
      (c-nil)
      (c-cons (c-car l) (c-take (- n 1) (c-cdr l)))))

(define (c-list-ref l n)
  (if (zero? n)
      (c-car l)
      (c-list-ref (c-cdr l) (- n 1))))