;; LR support code needed at generation time
;; =========================================

;; Source grammars
;; ~~~~~~~~~~~~~~~

(define rule-production car)
(define (rule-contains-attribution? rule)
  (not (null? (cdr rule))))
(define rule-attribution cadr)

(define (make-attribution-arglist i)
  (let loop ((i i) (l '()))
    (if (zero? i)
	l
	(loop (- i 1)
	      (cons (string->symbol
		     (string-append "$" (number->string i)))
		    l)))))

(define (make-trivial-attribution rhs-length)
  `(lambda ,(make-attribution-arglist rhs-length)
     '$1))

(define (rule-make-attribution rule)
  (let ((rhs-length (length (cdr (rule-production rule)))))
    `(lambda ,(make-attribution-arglist rhs-length)
       ,(if (rule-contains-attribution? rule)
	    (rule-attribution rule)
	    '$1))))

(define (source-grammar->grammar grammar k)
  (let ((nonterminals (source-grammar-nonterminals grammar))
	(source-terminals (source-grammar-terminals grammar))
	(rules (source-grammar-rules grammar))
	(start (source-grammar-start grammar)))
	    
    (let ((nonterminal-start (first-nonterminal-index source-terminals))
	  (production-index 1))

      (define (xlate symbol)
	(if (eq? '@error@ symbol)
	    error-terminal
	    (let ((p (list-position symbol nonterminals)))
	      (if p
		  (+ p 1 nonterminal-start)
		  (terminal->index source-terminals symbol)))))

      (define (rule->production rule)
	(let* ((production (rule-production rule))
	       (lhs (xlate (car production)))
	       (rhs (map xlate (cdr production)))
	       (attribution (rule-make-attribution rule))
	       (production
		(make-production production-index lhs rhs attribution)))
	  (set! production-index (+ 1 production-index))
	  production))

      (let* ((start (xlate start))
	     (productions
	      (cons (make-production
		     0 nonterminal-start
		     (cons start
			   (make-list k eoi-terminal))
		     (make-trivial-attribution (+ 1 k)))
		    (map rule->production rules)))
	     (nonterminals (cons nonterminal-start (map xlate nonterminals)))
	     (terminals (map xlate source-terminals)))

	(make-grammar nonterminals terminals productions start
		      (if (source-grammar-has-terminal-attribution? grammar)
			  `(lambda (terminal)
			     ,(source-grammar-terminal-attribution grammar))
			  #f))))))

;; Grammars
;; ~~~~~~~~

(define (make-grammar nonterminals terminals productions start
		      terminal-attribution)
  (vector nonterminals terminals productions start
	  terminal-attribution))

(define (grammar-nonterminals grammar)
  (vector-ref grammar 0))

(define (grammar-internal-start grammar)
  (car (grammar-nonterminals grammar)))

(define (grammar-terminals grammar)
  (vector-ref grammar 1))

(define (grammar-productions grammar)
  (vector-ref grammar 2))

(define (grammar-start-production grammar)
  (car (grammar-productions grammar)))

(define (grammar-start grammar)
  (vector-ref grammar 3))

(define (grammar-terminal-attribution grammar)
  (vector-ref grammar 4))

(define (make-production index lhs rhs attribution)
  (vector index lhs rhs attribution))
	  
(define (production-index p)
  (vector-ref p 0))

(define (production-lhs p)
  (vector-ref p 1))

(define (production-rhs p)
  (vector-ref p 2))

(define (production-attribution p)
  (vector-ref p 3))

(define (terminal? sym grammar)
  (or (equal? eoi-terminal sym)
      (equal? error-terminal sym)
      (member sym (grammar-terminals grammar))))

(define (nonterminal? sym grammar)
  (not (terminal? sym grammar)))

(define (production<? p1 p2)
  (< (production-index p1) (production-index p2)))

(define (sort-nonterminals xs)
  (sort-list xs <))

(define (sort-terminals xs)
  (sort-list xs <))


(define (terminals<? ts1 ts2)
  (cond ((null? ts1) (not (null? ts2)))
	((null? ts2) #f)
	((< (car ts1) (car ts2)) #t)
	((> (car ts1) (car ts2)) #f)
	(else (terminals<? (cdr ts1) (cdr ts2)))))

;; ---

(define (productions-with-lhs lhs grammar)
  (filter (lambda (production)
	    (equal? lhs (production-lhs production)))
	  (grammar-productions grammar)))

;; LR items
;; ~~~~~~~~

(define (make-item prod pos la)
  (vector prod pos la))
(define (item-production item)
  (vector-ref item 0))
(define (item-position item)
  (vector-ref item 1))
(define (item-lookahead item)
  (vector-ref item 2))

(define lookahead<? terminals<?)

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
		  
(define (sort-items item-set)
  (sort-list item-set item<?))

(define (items-merge is-1 is-2)
  (cond ((null? is-1) is-2)
	((member (car is-1) is-2)
	 (items-merge (cdr is-1) is-2))
	(else
	 (items-merge (cdr is-1) (cons (car is-1) is-2)))))

(define (items-append items-1 items-2)
  (append items-1
	  (filter
	   (lambda (item)
	     (not (member item items-1)))
	   items-2)))

(define (predict-equal? is-1 is-2)
      (and (= (length is-1) (length is-2))
	   (let loop ((is-1 is-1))
	     (or (null? is-1)
		 (and (member (car is-1) is-2)
		      (loop (cdr is-1)))))))

(define (compute-lr-closure state grammar k first-map)

  (let ((foobar 'dummy)) ; AKA Similix sucks

    (define (initial-items symbol lookahead-suffix)

      (apply
       append
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
				(append-k k (cdr rhs-rest) (item-lookahead item)))))
			  (loop (cdr item-set)
				(items-merge new-items predict-set))))))))))

    (let loop ((predict-set state))
      (let ((new-predict-set (next-predict predict-set)))
	(if (predict-equal? predict-set new-predict-set)
	    predict-set
	    (loop new-predict-set))))))

;; For historical reasons ...

(define compute-closure compute-lr-closure)

(define (compute-sorted-lr-closure state grammar k first-map)

  (let ((foobar 'dummy)) ; we know this one ...

    (define (initial-items symbol lookahead-suffix)

      (apply
       append
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
		   (append-k k (cdr rhs-rest) (item-lookahead item))))))))
      
    (define (predict item-set)
      (let loop ((item-set item-set) (predict-set item-set))
	(if (null? item-set)
	    predict-set
	    (let ((next (item-next-predict (car item-set))))
	    (loop (items-append next (cdr item-set))
		  (items-append next predict-set))))))

    (predict state)))

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

(define (goto-sorted state-closure symbol)
  (map item-shift
       (filter (lambda (item)
		 (and (not (null? (item-rhs-rest item)))
		      (equal? symbol
			      (car (item-rhs-rest item)))))
	       state-closure)))

(define (goto state-closure symbol)
  (sort-items
   (map item-shift
	(filter (lambda (item)
		  (and (not (null? (item-rhs-rest item)))
		       (equal? symbol
			       (car (item-rhs-rest item)))))
		state-closure))))

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
	    (and (not (equal? error-terminal symbol))
		 (terminal? symbol grammar)))
	  (next-symbols state-closure grammar)))

(define (next-nonterminals state-closure grammar)
  (filter (lambda (symbol)
	    (nonterminal? symbol grammar))
	  (next-symbols state-closure grammar)))

(define (handles-error? state-closure grammar)
  (or-map (lambda (symbol)
	    (equal? error-terminal symbol))
	  (next-symbols state-closure grammar)))

(define (accept state-closure)
  (filter (lambda (item)
	    (null? (item-rhs-rest item)))
	  state-closure))

(define (items-lookaheads accept-items)
  (map item-lookahead accept-items))

(define (final? state grammar)
  (let loop ((item-set state))
    (and (not (null? item-set))
	 (let ((item (car item-set)))
	   (or (and (equal? (grammar-internal-start grammar)
			    (item-lhs item))
		    (= 1 (item-position item)))
	       (loop (cdr item-set)))))))

(define (add-slr-lookahead item-set follow-map)
  (let ((add-one-slr-lookahead
	 (lambda (item)
	   (let ((production (item-production item)))
	     (map (lambda (la)
		    (make-item production (item-position item) la))
		  (cdr (assoc (production-lhs production) follow-map)))))))
    (apply append (map add-one-slr-lookahead item-set))))

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

;; First set computation
;; ~~~~~~~~~~~~~~~~~~~~~

(define (restrict-k k l)
  (if (or (null? l)
	  (zero? k))
      '()
      (cons (car l) (restrict-k (- k 1) (cdr l)))))

(define (append-k k l1 l2)
  (if (null? l1)
      (restrict-k k l2)
      (if (zero? k)
	  '()
	  (cons (car l1)
		(append-k (- k 1)
			  (cdr l1)
			  l2)))))

(define (sf-union f-1 f-2)
  (let loop ((f f-1) (r f-2))
    (if (null? f)
	r
	(loop (cdr f)
	      (if (member (car f) r)
		  r
		  (cons (car f) r))))))

(define (sf-list-union l)
  (let loop ((l l) (r '()))
    (if (null? l)
	r
	(loop (cdr l)
	      (sf-union (car l) r)))))

(define (sf-list-uniq l)
  (let loop ((l l) (r '()))
    (if (null? l)
	r
	(loop (cdr l)
	      (if (member (car l) r)
		  r
		  (cons (car l) r))))))
	    
(define (sf-first rhs k grammar first-map)
  (let loop ((rhs-rest rhs))
    
    (if (null? rhs-rest)
	'(())
	(let ((cdr-first (loop (cdr rhs-rest)))
	      (s (car rhs-rest)))
	  (if (terminal? s grammar)
	      (sf-list-uniq
	       (map (lambda (f)
		      (append-k k (list s) f))
		    cdr-first))
	      (sf-list-union
	       (map
		(lambda (f-cdr)
		  (map
		   (lambda (f-car)
		     (append-k k f-car f-cdr))
		   (cdr (assoc s first-map))))
		cdr-first)))))))

(define (lhs-next-first lhs k grammar old-first)
  (let loop ((ps (productions-with-lhs lhs grammar))
	     (first '()))
    (if (null? ps)
	first
	(let ((rhs-first (sf-first (production-rhs (car ps)) k grammar old-first)))
	  (loop (cdr ps)
		(sf-union first rhs-first))))))

(define (initial-first-map grammar)
  ;; each nonterminal is associated with the empty set
  (map (lambda (nt)
	 (cons nt '()))
       (grammar-nonterminals grammar)))

(define (next-first-map grammar k last-first-map)
  ;; "Gesamtschritt" step in solving the flow equation system for first_k
  (map
   (lambda (first-map-entry)
     (let ((nonterm (car first-map-entry)))
       (cons nonterm
	     (lhs-next-first nonterm k grammar last-first-map))))
   last-first-map))
 
(define (first-equal? f-1 f-2)
  (and (= (length f-1) (length f-2))
       (let loop ((f-1 f-1))
	 (or (null? f-1)
	     (and (member (car f-1) f-2)
		  (loop (cdr f-1)))))))
	     
(define (first-map-equal? fm-1 fm-2)
  (let loop ((fm-1 fm-1))
    (or (null? fm-1)
	(let* ((nonterm-1 (caar fm-1))
	       (first-1 (cdar fm-1))
	       (first-2 (cdr (assoc nonterm-1 fm-2))))
	  (and (first-equal? first-1 first-2)
	       (loop (cdr fm-1)))))))


(define (compute-first grammar k)
  ;; fixpoint iteration
  (let loop ((first-map (initial-first-map grammar)))
    (let ((new-first-map (next-first-map grammar k first-map)))
      (if (first-map-equal? first-map new-first-map)
	  first-map
	  (loop new-first-map)))))

;;; Follow set computation
;;; ~~~~~~~~~~~~~~~~~~~~~~

(define (initial-follow-map grammar)
  ;; start symbol must be followed by the empty string to get off the
  ;; ground
  (map (lambda (nt)
	 (cons nt
	       (if (equal? nt (grammar-internal-start grammar))
		   '(())
		   '())))
       (grammar-nonterminals grammar)))

;;; perform
;;; follow (k, A) = U { first (k, beta follow (k, B)) | B -> alpha A beta }
;;; by iterating over the right sides of all productions, updating the
;;; follow-set as appropriate

(define (next-follow-map grammar k first-map last-follow-map)
  (let loop ((productions (grammar-productions grammar))
	     (last-follow-map last-follow-map))
    (if (null? productions)
	last-follow-map
	(let ((lhs (production-lhs (car productions))))
	  (let rhs-loop ((rhs-rest (production-rhs (car productions)))
			 (last-follow-map last-follow-map))
	    (if (null? rhs-rest)
		(loop (cdr productions) last-follow-map)
		(let ((sym (car rhs-rest)))
		  (if (terminal? sym grammar)
		      (rhs-loop (cdr rhs-rest) last-follow-map)
		      (let* ((fi-rest (sf-first (cdr rhs-rest) k grammar first-map))
			     (fo-lhs (cdr (assoc lhs last-follow-map)))
			     (fo-sym (sf-list-uniq
				      (pair-map (lambda (xs ys) (append-k k xs ys))
					       fi-rest fo-lhs))))
		      (rhs-loop (cdr rhs-rest)
				(update-follow-map last-follow-map
						   sym fo-sym)))))))))))

(define (update-follow-map follow-map sym fo-sym)
  (let loop ((follow-map follow-map))
    (if (equal? (caar follow-map) sym)
	(cons (cons sym (sf-union fo-sym (cdar follow-map)))
	      (cdr follow-map))
	(cons (car follow-map)
	      (loop (cdr follow-map))))))

(define (pair-map f xs ys)
  (let xs-loop ((xs xs))
    (if (null? xs)
	'()
	(let ((x (car xs)))
	  (let ys-loop ((ys ys))
	    (if (null? ys)
		(xs-loop (cdr xs))
		(let ((y (car ys)))
		  (cons (f x y) (ys-loop (cdr ys))))))))))

(define (compute-follow grammar k first-map)
  ;; fixpoint iteration
  (let loop ((follow-map (initial-follow-map grammar)))
    (let ((new-follow-map (next-follow-map grammar k first-map follow-map)))
      (if (first-map-equal? follow-map new-follow-map)
	  follow-map
	  (loop new-follow-map)))))

;; Lookahead tries
;; ~~~~~~~~~~~~~~~

(define (take n l)
    (cond
     ((or (zero? n) (null? l))
      '())
     (else
      (cons (car l) (take (- n 1) (cdr l))))))

;;; trie of depth k: k nested alists
;;; needs at least one item
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

;; Generic stuff
;; ~~~~~~~~~~~~~

(define (uniq l)
  (cond ((null? l) l)
	((member (car l) (cdr l)) (uniq (cdr l)))
	(else (cons (car l) (uniq (cdr l))))))

(define (flatten l)
  (apply append l))

(define (filter pred? list)
  (let loop ((rest list) (result '()))
    (if (null? rest)
	(reverse result)
	(loop (cdr rest)
	      (if (pred? (car rest))
		  (cons (car rest) result)
		  result)))))

(define (or-map pred? list)
  (and (not (null? list))			;;;; was (or (null? list) ...)
       (or (pred? (car list))
	   (or-map pred? (cdr list)))))

(define (partition-list pred l)
  (let loop ((l l) (yes '()) (no '()))
    (cond ((null? l)
           (cons (reverse yes) (reverse no)))
          ((pred (car l))
           (loop (cdr l) (cons (car l) yes) no))
          (else
           (loop (cdr l) yes (cons (car l) no))))))
