; LR support code needed at generation time

(define (production<? p1 p2)
  (number-list<? (cons (production-lhs p1) (production-rhs p1))
		 (cons (production-lhs p2) (production-rhs p2))))

; LR items

; They must be distinguishable from pairs.

(define (make-item prod pos la)
  (vector prod pos la))
(define (item-production item)
  (vector-ref item 0))
(define (item-position item)
  (vector-ref item 1))
(define (item-lookahead item)
  (vector-ref item 2))

(define (predict-item? item)
  (zero? (item-position item)))

(define (lookahead<? la1 la2)
  (number-list<? la1 la2))

(define (item-lhs item)
  (production-lhs (item-production item)))
(define (item-rhs item)
  (production-rhs (item-production item)))

(define (item-rhs-rest item)
  (list-tail (production-rhs (item-production item))
	     (item-position item)))

(define (item-shift item)
  (make-item (item-production item)
	     (+ 1 (item-position item))
	     (item-lookahead item)))

(define (item<? item-1 item-2)
  (or (production<? (item-production item-1)
		    (item-production item-2))
      (< (item-position item-1)
	 (item-position item-2))
      (lookahead<? (item-lookahead item-1) (item-lookahead item-2))))

(define (partition-items item items)
  (let ((production (item-production item))
	 (position (item-position item)))
  (partition-list
   (lambda (item)
     (and (equal? production (item-production item))
	  (equal? position (item-position item))))
   items)))

(define (items-merge is-1 is-2)
  (append (filter
	   (lambda (item)
	     (not (member item is-2)))
	   is-1)
	  is-2))

(define (predict-equal? is-1 is-2)
      (and (= (length is-1) (length is-2))
	   (let loop ((is-1 is-1))
	     (or (null? is-1)
		 (and (member (car is-1) is-2)
		      (loop (cdr is-1)))))))

(define (compute-lr-closure state grammar k)

  (define (initial-items symbol lookahead-suffix)
    (flatten
     (map (lambda (production)
	    (map
	     (lambda (la)
	       (make-item production 0 la))
	     (sequence-first lookahead-suffix k grammar)))
	  (grammar-productions-with-lhs symbol grammar))))

  (define (next-predict item-set)
    (let loop ((item-set item-set) (predict-set item-set))
      (if (null? item-set)
	  predict-set
	  (let* ((item (car item-set))
		 (rhs-rest (item-rhs-rest item)))
	    (if (null? rhs-rest)
		(loop (cdr item-set) predict-set)
		(let ((lhs (car rhs-rest)))
		  (if (terminal? lhs grammar)
		      (loop (cdr item-set) predict-set)
		      (let ((new-items
			     (initial-items
			      lhs
			      (append (cdr rhs-rest)
				      (item-lookahead item)))))
			(loop (cdr item-set)
			      (items-merge new-items predict-set))))))))))

  (let loop ((predict-set state))
    (let ((new-predict-set (next-predict predict-set)))
      (if (predict-equal? predict-set new-predict-set)
	  predict-set
	  (loop new-predict-set)))))

; For R�hrich-style error recovery

(define (compute-sorted-lr-closure state grammar k)

  (define (initial-items symbol lookahead-suffix)
    (flatten
     (map (lambda (production)
	    (map
	     (lambda (la)
	       (make-item production 0 la))
	     (sequence-first lookahead-suffix k grammar)))
	  (grammar-productions-with-lhs symbol grammar))))

  (define (item-next-predict item)
    (let ((rhs-rest (item-rhs-rest item)))
      (if (null? rhs-rest)
	  '()
	  (let ((lhs (car rhs-rest)))
	    (if (terminal? lhs grammar)
		'()
		(initial-items
		 lhs
		 (restricted-append k (cdr rhs-rest) (item-lookahead item))))))))
      
  (define (predict item-set)
    (let loop ((item-set item-set) (predict-set item-set))
      (if (null? item-set)
	  predict-set
	  (let ((next (item-next-predict (car item-set))))
	    (loop (items-merge next (cdr item-set))
		  (items-merge next predict-set))))))

  (predict state))

(define (sequences-initial-nonterminals sequences grammar)
  (filter-map
   (lambda (sequence)
     (and (not (null? sequence))
	  (not (terminal? (car sequence) grammar))
	  (car sequence)))
   sequences))

(define (compute-slr-closure state grammar k)

  (let ((predict-sets (grammar-fetch-property
		       grammar
		       'predict-sets
		       (lambda (grammar)
			 (make-vector (grammar-number-of-nonterminals grammar)
				      #f))))
	(offset (grammar-nonterminal-offset grammar)))

    (define (compute-predict-lhses lhs already-done)
      (cond
       ((and (null? already-done)
	     (vector-ref predict-sets (- lhs offset)))
	=> identity)
       ((memv lhs already-done)
	'())
       (else
	(let* ((lhses
		(filter
		 (lambda (lhs)
		   (not (memv lhs already-done)))
		 (sequences-initial-nonterminals
		  (map production-rhs
		       (grammar-productions-with-lhs lhs grammar))
		  grammar)))
	       (already-done (cons lhs already-done)))
	  (cons lhs
		(flatten
		 (map (lambda (lhs)
			(compute-predict-lhses lhs already-done))
		      lhses)))))))

    (define (predict-lhses lhs)
      (cond
       ((vector-ref predict-sets (- lhs offset))
	=> identity)
       (else
	(let ((lhses (uniq (compute-predict-lhses lhs '()))))
	  (vector-set! predict-sets (- lhs offset) lhses)
	  lhses))))

    (let* ((initial-predict-lhses
	    (sequences-initial-nonterminals (map item-rhs-rest state) grammar))
	   ;; (dummy (begin (write initial-predict-lhses) (newline)))
	   (predict-lhses
	    (uniq (flatten (map predict-lhses initial-predict-lhses))))
	   ;; (dummy (begin (write predict-lhses) (newline)))
	   (predict-productions
	    (flatten (map (lambda (lhs)
			    (grammar-productions-with-lhs lhs grammar))
			  predict-lhses)))
	   ;; (dummy (begin (write predict-productions) (newline)))
	   (predict-items
	    (productions->slr-predict-items predict-productions k grammar))
	   ;; (dummy (begin (write predict-items) (newline)))
	   )
      (append state
	      predict-items))))

(define (productions->slr-predict-items productions k grammar)

  (define (production->slr-predict-items production)
    (map (lambda (lookahead)
	   (make-item production 0 lookahead))
	 (nonterminal-follow (production-lhs production) k grammar)))

  (flatten (map production->slr-predict-items productions)))

; Operations on LR states

(define (goto-sorted state-closure symbol)
  (map item-shift
       (filter (lambda (item)
		 (and (not (null? (item-rhs-rest item)))
		      (equal? symbol
			      (car (item-rhs-rest item)))))
	       state-closure)))

(define (goto state-closure symbol)
  (sort-list (goto-sorted state-closure symbol) item<?))

(define (active state)
  (let loop ((item-set state)
	     (m 0))
    (if (null? item-set)
	m
	(loop (cdr item-set)
	      (max (item-position (car item-set)) m)))))

(define (next-symbols state-closure grammar)
  (let loop ((item-set state-closure)
	     (symbols '()))
    (if (null? item-set)
	symbols
	(let* ((item (car item-set))
	       (rhs-rest (item-rhs-rest item)))
	  (loop (cdr item-set)
		(if (and (not (null? rhs-rest))
			 (not (member (car rhs-rest) symbols)))
		    (cons (car rhs-rest) symbols)
		    symbols))))))

(define (next-terminals state-closure grammar)
  (filter (lambda (symbol)
	    (and (not (equal? (grammar-error grammar) symbol))
		 (terminal? symbol grammar)))
	  (next-symbols state-closure grammar)))

(define (next-nonterminals state-closure grammar)
  (filter (lambda (symbol)
	    (nonterminal? symbol grammar))
	  (next-symbols state-closure grammar)))

(define (handles-error? state-closure grammar)
  (member (grammar-error grammar) (next-symbols state-closure grammar)))

(define (accept state-closure)
  (filter (lambda (item)
	    (null? (item-rhs-rest item)))
	  state-closure))

(define (find-eoi-lookahead-item accept-items)
  (first (lambda (item)
	   (null? (item-lookahead item)))
	 accept-items))

(define (items->lookahead-sets+items items)
  (if (null? items)
      items
      (let* ((sorted-items (sort-list items
				      (lambda (item-1 item-2)
					(production<? (item-production item-1)
						      (item-production item-2)))))
	     (first-item (car sorted-items)))
	
	(let loop ((current-item first-item)
		   (current-set (list (item-lookahead first-item)))
		   (items (cdr sorted-items))
		   (lookahead-sets+items '()))
	  (if (null? items)
	      (reverse (cons (cons current-set current-item)
			     lookahead-sets+items))
	      (let ((next-item (car items)))
		(if (equal? (item-production current-item)
			    (item-production next-item))
		    (loop current-item
			  (cons (item-lookahead next-item)
				current-set)
			  (cdr items)
			  lookahead-sets+items)
		    (loop next-item
			  (list (item-lookahead next-item))
			  (cdr items)
			  (cons (cons current-set current-item)
				lookahead-sets+items)))))))))

(define (initial? state grammar)
  (any? (lambda (item)
	  (equal? (grammar-start grammar) (item-lhs item)))
	state))

(define (find-repair-terminal grammar item-set)
  (let loop ((item-set item-set))
    (let* ((item (car item-set))
	   (rhs-rest (item-rhs-rest item)))
      (cond
       ((null? rhs-rest)
	(car (item-lookahead item)))
       ((terminal? (car rhs-rest) grammar)
	(car rhs-rest))
       (else
	(loop (cdr item-set)))))))

; Conflict handling

(define (check-for-reduce-reduce-conflict closure accept-items grammar k)
  (let loop ((items accept-items))
    (cond
     ((null? items)
      'fick-dich-ins-knie)
     ((let ((lookahead (item-lookahead (car items))))
	(first (lambda (item)
		 (equal? lookahead (item-lookahead item)))
	       (cdr items)))
      => (lambda (conflict-item)
	   (display-conflict "Reduce-reduce" closure (car items) conflict-item grammar)))
     (else
      (loop (cdr items))))))

(define (check-for-shift-reduce-conflict closure accept-items grammar k)
  (let loop ((items closure))
    (if (pair? items)
	(let* ((item (car items))
	       (rhs-rest (item-rhs-rest item))
	       (lookahead (item-lookahead item)))
	  (if (or (null? rhs-rest)
		  (nonterminal? (car rhs-rest) grammar))
	      (loop (cdr items))
	      (let ((lookaheads (sequence-first (append rhs-rest lookahead)
						k grammar)))
		(cond
		 ((first (lambda (item)
			   (member (item-lookahead item) lookaheads))
			 accept-items)
		  => (lambda (conflict-item)
		       (display-conflict "Shift-reduce" closure (car items) conflict-item
					 grammar)))
		 (else
		  (loop (cdr items))))))))))

(define *display-item-closures* #f)

(define (display-conflict name closure item-1 item-2 grammar)
  (display name)
  (display " conflict between items ")
  (display-item item-1 grammar)
  (display " and ")
  (display-item item-2 grammar)
  (newline)
  (if *display-item-closures*
      (begin
	(display "State closure:")
	(display-closure closure grammar))))

(define (display-closure closure grammar)
  (call-with-values
      (lambda ()
	(partition-list predict-item? closure))
    (lambda (predict core)
      (display-items core grammar)
      (display "----------------") (newline)
      (display-items predict grammar))))

(define (display-items items grammar)
  (let loop ((items items))
    (if (pair? items)
	(let ((item (car items)))
	  (call-with-values
	      (lambda ()
		(partition-list
		 (lambda (other-item)
		   (and (eq? (item-production item)
			     (item-production other-item))
			(= (item-position item)
			   (item-position other-item))))
		 items))
	    (lambda (this-items other-items)
	      (display-item item grammar)
	      (display #\space)
	      (write (map (lambda (item)
			    (map (lambda (s)
				   (grammar-symbol->name s grammar))
				 (item-lookahead item)))
			  this-items))
	      (newline)
	      (loop other-items)))))))

(define (display-item item grammar)
  (display (grammar-symbol->name (item-lhs item) grammar))
  (display " ->")
  (let loop ((rhs-symbols (item-rhs item))
	     (position (item-position item)))
    (cond
     ((not (null? rhs-symbols))
      (write-char #\space)
      (if (zero? position)
	  (display ". "))
      (display (grammar-symbol->name (car rhs-symbols) grammar))
      (loop (cdr rhs-symbols) (- position 1)))
     ((zero? position)
      (display " .")))))

(define (trace-state closure grammar)
  (display "Entering state:") (newline)
  (display-closure closure grammar))

(define (trace-reduce closure nonterminal grammar)
  (display "Reducing with ")
  (display (grammar-symbol->name nonterminal grammar))
  (display " after returning to:")
  (newline)
  (display-closure closure grammar))

(define (trace-shift closure terminal grammar)
  (display "Shifting with ")
  (display (grammar-symbol->name terminal grammar))
  (newline))

; List utilities

(define (flatten l)
  (apply append l))

(define (take n l)
  (let loop ((n n) (l l) (result '()))
    (if (or (zero? n) (null? l))
	(reverse result)
	(loop (- n 1) (cdr l) (cons (car l) result)))))

(define (restricted-append k l1 l2)
  (if (null? l1)
      (take k l2)
      (if (zero? k)
	  '()
	  (cons (car l1)
		(restricted-append (- k 1)
				   (cdr l1)
				   l2)))))

(define (number-list<? ts1 ts2)
  (cond ((null? ts1) (not (null? ts2)))
	((null? ts2) #f)
	((< (car ts1) (car ts2)) #t)
	((> (car ts1) (car ts2)) #f)
	(else (number-list<? (cdr ts1) (cdr ts2)))))

(define (uniq l)
  (let loop ((l l) (r '()))
    (if (null? l)
	(reverse r)
	(loop (cdr l)
	      (if (member (car l) r)
		  r
		  (cons (car l) r))))))
