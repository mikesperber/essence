;; Essential LR parsing (continuation-based)
;; =========================================

(define (parse grammar k first-map state continuations input)
  (if (and (final? state grammar)
           (equal? '$ (car input)))
      'accept
      (let ((closure (compute-closure state grammar k first-map)))

        (define (c0 symbol input)
          (let ((next-state (goto closure symbol)))
            (parse grammar k first-map
		   next-state
		   (cons c0
			 (take (- (active next-state) 1)
			       continuations))
		   input)))

        (let ((accept-items (accept closure)))
	  (the-trick
	   (car input) (next-terminals closure grammar)
	   (lambda (t)
	     (c0 t (cdr input)))
	   (lambda ()
	     (select-lookahead-item-the-trick
	      accept-items k input
	      (lambda (item)
		((list-ref (cons c0 continuations)
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
	   '()
	   (append input (make-$ k)))))

;; ~~~~~~~~~~~~

(define (the-trick element set cont fail)
  (let loop ((set set))
    (if (null? set)
	(fail)
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

(define (take n l)
  (if (zero? n)
      '()
      (cons (car l) (take (- n 1) (cdr l)))))
