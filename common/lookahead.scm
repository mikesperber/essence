(define-without-memoization
  (lookahead-matches? k lookahead input)
  (let loop ((remaining k) (lookahead lookahead) (input input))
    (cond
     ((zero? remaining) #t)
     ((null? lookahead)
      (if (= k remaining) ; we covered this previously
	  #f
	  (stream-empty? input)))
     ((and (not (= k remaining)) ; we covered this previously
	   (stream-empty? input))
       #f)
     ((equal? (car lookahead) (car (stream-car input)))
      (loop (- remaining 1) (cdr lookahead) (stream-cdr input)))
     (else #f))))

(define-without-memoization
  (find-lookahead-item item-set k input)
  (let loop ((item-set item-set))
    (if (null? item-set)
	#f
	(let ((item (car item-set)))
	  (if (lookahead-matches? k (item-lookahead item) input)
	      item
	      (loop (cdr item-set)))))))

(define-without-memoization
  (find-eoi-lookahead-item item-set)
  (let loop ((item-set item-set))
    (cond
     ((null? item-set) #f)
     ((null? (item-lookahead (car item-set)))
      (car item-set))
     (else
      (loop (cdr item-set))))))
