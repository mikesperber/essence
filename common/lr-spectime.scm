; LR support code needed at generation time

(define (production<? p1 p2)
  (number-list<? (cons (production-lhs p1) (production-rhs p1))
		 (cons (production-lhs p2) (production-rhs p2))))

; LR items

(define (make-item prod pos la)
  (vector prod pos la))
(define (item-production item)
  (vector-ref item 0))
(define (item-position item)
  (vector-ref item 1))
(define (item-lookahead item)
  (vector-ref item 2))

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

(define (compute-lr-closure state grammar k first-map)

  (define (initial-items symbol lookahead-suffix)
    (flatten
     (map (lambda (production)
	    (map
	     (lambda (la)
	       (make-item production 0 la))
	     (sf-first lookahead-suffix k grammar first-map)))
	  (productions-with-lhs symbol grammar))))

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
			      (restricted-append k
						 (cdr rhs-rest)
						 (item-lookahead item)))))
			(loop (cdr item-set)
			      (items-merge new-items predict-set))))))))))

  (let loop ((predict-set state))
    (let ((new-predict-set (next-predict predict-set)))
      (if (predict-equal? predict-set new-predict-set)
	  predict-set
	  (loop new-predict-set)))))

; For Röhrich-style error recovery

(define (compute-sorted-lr-closure state grammar k first-map)

  (define (initial-items symbol lookahead-suffix)
    (flatten
     (map (lambda (production)
	    (map
	     (lambda (la)
	       (make-item production 0 la))
	     (sf-first lookahead-suffix k grammar first-map)))
	  (productions-with-lhs symbol grammar))))

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

(define (compute-slr-closure state grammar k follow-map)

    (define (initial-items symbol)
      (add-slr-lookahead
       (map (lambda (production)
	      (make-item production 0 #f))
	    (productions-with-lhs symbol grammar))
       follow-map))

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
			(let ((new-items (initial-items lhs)))
			  (loop (cdr item-set)
				(items-merge new-items predict-set))))))))))

    (let loop ((predict-set state))
      (let ((new-predict-set (next-predict predict-set)))
	(if (predict-equal? predict-set new-predict-set)
	    predict-set
	    (loop new-predict-set)))))

(define (add-slr-lookahead item-set follow-map)
  (let ((add-one-slr-lookahead
	 (lambda (item)
	   (let ((production (item-production item)))
	     (map (lambda (la)
		    (make-item production (item-position item) la))
		  (cdr (assoc (production-lhs production) follow-map)))))))
    (flatten (map add-one-slr-lookahead item-set))))

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

; Lookahead tries

; trie of depth k: k nested alists
; needs at least one item

(define (build-trie items)
  (if (null? items)
      '()
      (let* ((item (car items))
	     (items (cdr items))
	     (trie (let loop ((la (item-lookahead item)))
		     (if (null? la)
			 item
			 (list (cons (car la) (loop (cdr la))))))))
	(let loop ((items items) (trie trie))
	  (if (null? items)
	      trie
	      (loop (cdr items)
		    (let loop ((la (item-lookahead (car items))) (trie trie))
		      (if (null? la)
			  (car items)
			  (let ((car-la (car la)))
			    (let inner-loop ((trie trie))
			      (if (null? trie)
				  (list (cons car-la (loop (cdr la) '())))
				  (if (equal? car-la (caar trie))
				      (cons (cons car-la (loop (cdr la) (cadr trie)))
					    (cdr trie))
				      (cons (car trie)
					    (inner-loop (cdr trie)))))))))))))))

(define (sort-trie trie k)
  (let ((subject
	 (if (> k 1)
	     (map (lambda (p) (cons (car p) (sort-trie (cdr p) (- k 1)))) trie)
	     trie)))
    (sort-list subject
	       (lambda (p q)
		 (< (car p) (car q))))))

(define (collapse-trie trie k)
  (if (null? trie)
      '()
      (let ((subject
	     (if (> k 1)
		 (map (lambda (p) (cons (car p) (collapse-trie (cdr p) (- k 1)))) trie)
		 trie)))
	(let loop ((first (car subject)) (rest (cdr subject)))
	  (let* ((first-la (car first))
		 (first-trie (cdr first))
		 (yes/no (partition-list (lambda (p) (equal? first-trie (cdr p))) rest))
		 (yes (car yes/no))
		 (no (cdr yes/no)))
	    (cons (cons (cons first-la (map car yes)) first-trie)
		  (if (null? no)
		      '()
		      (loop (car no) (cdr no)))))))))

(define (items->trie item-set k)
  (collapse-trie (sort-trie (build-trie item-set) k) k))

; List utilities

(define (flatten l)
  (apply append l))

(define (take n l)
    (cond
     ((or (zero? n) (null? l))
      '())
     (else
      (cons (car l) (take (- n 1) (cdr l))))))

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
