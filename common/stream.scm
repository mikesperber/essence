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

(define (list->stream l)
  (make-stream
   (lambda (l)
     (if (not (null? l))
	 l				; it just works out that way	 
	 #f))
   l))

(define (stream->list stream)
  (let loop ((stream stream) (reverse-list '()))
    (if (stream-empty? stream)
	(reverse reverse-list)
	(loop (stream-cdr stream)
	      (cons (stream-car stream) reverse-list)))))
