(define-syntax define-data
  (lambda (x r c)
    (let ((clauses (cddr x))

	  (%begin (r 'begin))
	  (%define (r 'define))
	  (%if (r 'if))
	  (%and (r 'and))

	  (%eq? (r 'eq?))

	  (%error (r 'error))

	  (%vector (r 'vector))
	  (%vector? (r 'vector?))
	  (%vector-ref (r 'vector-ref)))
    `(,%begin
       ,@(apply
	  append
	  (map
	   (lambda (ctor-decl)
	     (let* ((ctor-name (car ctor-decl))
		    (ctor-test (string->symbol (string-append
						(symbol->string
						 ctor-name) "?"))))
	       `((,%define ,ctor-decl
			   (,%vector ',(car ctor-decl)
				     ,@(cdr ctor-decl)))
		 (,%define (,ctor-test arg)
			   (,%and (,%vector? arg)
				  (,%eq? (,%vector-ref arg 0) ',ctor-name)))
		 ,@(let loop ((selectors (cdr ctor-decl)) (i 1) (r '()))
		   (if (null? selectors)
		       r
		       (loop (cdr selectors)
			     (+ i 1)
			     (cons
			      `(,%define (,(car selectors) x)
					 (,%if (,%and (,%vector? x)
						      (,%eq? (,%vector-ref x 0) ',ctor-name))
					       (,%vector-ref x ,i)
					       (,%error "bad selector ~A applied to ~S"
							',(car selectors) x)))
			      r)))))))
	   clauses))))))
