(define (make-stream m state)
  (delay
    (let ((o (m state)))
      (if (eq? #f o)
	  '()
	  (cons (car o)
		(make-stream m (cdr o)))))))

(define (scan-list->stream l)
  (make-stream
   (lambda (l)
     (if (null? l)
	 '(($ . #f))
	 l)) ; ... it just works out that way
   l))