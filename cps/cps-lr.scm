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
	  (if (member (car input) (next-terminals closure grammar))
	      (c0 (car input) (cdr input))
	      (let ((item (select-lookahead-item accept-items k input)))
		(if item
		    ((list-ref (cons c0 continuations)
			       (length (item-rhs item)))
		     (item-lhs item) input)
		    'error)))))))

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

(define (select-lookahead-item item-set k input)
  (let ((input-front (take k input)))
    (first (lambda (item)
	     (equal? input-front (item-lookahead item)))
	   item-set)))

(define (take n l)
  (if (zero? n)
      '()
      (cons (car l) (take (- n 1) (cdr l)))))

(define (first p l)
  (cond ((null? l) #f)
	((p (car l) (car l)))
	(else (first p (cdr l)))))
