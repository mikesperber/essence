;; This relies on error

(define bench-lookahead 1)

(define (read-tokens terminals k . maybe-port)
  (let ((port (if (null? maybe-port)
		  (current-input-port)
		  (car maybe-port))))

    (let loop ((l '()))
      (let ((token (read port)))
	(if (eof-object? token)
	    (terminate-input-list (reverse l) k)
	    (loop (cons (terminal->index terminals token) l)))))))

(define (_error . args) (apply error args))

;; args: input, # times
;; parameters: name of parsing procedure
;; 

(define *bench-input* #f)
(define *bench-ntimes* 1000)

(define (bench-setup! args)
  (let ((argc (length args)))
    (if (< argc 1)
	(error "not enough arguments"))
    (if (> argc 2)
	(error "too many arguments"))

    (let* ((n (if (= argc 1)
		 1000
		 (string->number (car args))))
	  (input-fname
	   (if (= argc 1)
	       (car args)
	       (cadr args)))
	  (input (with-input-from-file
		     input-fname
		   (lambda ()
		     (read-tokens bench-terminals bench-lookahead)))))

      (set! *bench-input* input)
      (set! *bench-ntimes* n))))

(define (bench-run)
  (do ((i *bench-ntimes* (- i 1)))
      ((zero? i))
    (parse *bench-input*)))
