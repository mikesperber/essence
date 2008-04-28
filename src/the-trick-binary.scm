(define-without-memoization (maybe-the-member element set)
  (let loop ((sorted (sort-list set <)) (size (length set)))
    (_memo2
     (cond
      ((null? sorted) #f)
      ((>= size 3)
       (let* ((middle (- (quotient (+ size 1) 2) 1))
	      (static-element (list-ref sorted middle)))
	 (cond
	  ((eqv? static-element element)
	   static-element)
	  ((< element static-element)
	   (loop (take middle sorted) middle))
	  (else
	   (loop (list-tail sorted (+ middle 1))
		 (- size (+ middle 1)))))))
      ((eqv? element (car sorted))
       (car sorted))
      (else
       (loop (cdr sorted) (- size 1)))))))

(define-without-memoization (the-member element set)
  (let loop ((sorted (sort-list set <)) (size (length set)))
    (_memo2
     (cond
      ((= size 1) (car sorted))
      ((>= size 3)
       (let* ((middle (- (quotient (+ size 1) 2) 1))
	      (static-element (list-ref sorted middle)))
	 (cond
	  ((eqv? static-element element)
	   static-element)
	  ((< element static-element)
	   (loop (take middle sorted) middle))
	  (else
	   (loop (list-tail sorted (+ middle 1))
		 (- size (+ middle 1)))))))
      ((eqv? element (car sorted))
       (car sorted))
      (else
       (loop (cdr sorted) (- size 1)))))))

(define (take n l)
  (let loop ((n n) (l l) (result '()))
    (if (or (zero? n) (null? l))
	(reverse result)
	(loop (- n 1) (cdr l) (cons (car l) result)))))
