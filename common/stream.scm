(define (make-stream m state)
  (delay
    (let ((o (m state)))
      (if (eq? #f o)
	  '()
	  (cons (car o)
		(make-stream m (cdr o)))))))

(define (stream-car stream)
  (car (force stream)))

(define (stream-cdr stream)
  (cdr (force stream)))

(define (stream-empty? stream)
  (null? (force stream)))

	 