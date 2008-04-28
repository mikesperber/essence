;; Generic getopt code

;; This relies on big-util.

;; It's ugly, but so are Unix command lines.

(define (get-options options argv)
  (let loop ((argv-pointer argv) (opts '()))
    (if (null? argv-pointer)
	(values opts '())
	(if (option? (car argv-pointer))
	    (call-with-values
	     (lambda ()
	       (parse-option (car argv-pointer) argv-pointer options))
	     (lambda (option param argv-pointer)
		     (loop argv-pointer
			   (add-option! param option opts))))
	    (values opts argv-pointer)))))


(define (parse-option string argv-pointer options)
  (let* ((name (string->option-name string))
	 (option (find-option name options)))

    (if (not option)
	(error "Illegal option: ~A~%" string))
 
    (let* ((props (option-properties option))
	   (has-parameter? (memq 'parameter props))
	   (raw-param 
	    (and 
	     has-parameter?
	     (if (> (string-length name) 1)
		 (or (string->option-parameter (car argv-pointer))
		     (error "Option missing parameter: ~A~%"
			    (car argv-pointer)))
		 (begin
		   (if (null? (cdr argv-pointer))
		       (error "Option missing parameter: ~A~%"
			      (car argv-pointer)))
		   (set! argv-pointer (cdr argv-pointer))
		   (car argv-pointer)))))
	   (param
	    (cond
	     ((not has-parameter?) '())
	     ((memq 'splits props)
	      (split-string raw-param #\space))
	     ((memq 'accumulates props)
	      (list raw-param))
	     (else raw-param))))

    (values option param (cdr argv-pointer)))))

(define (add-option! param option opts)
  (let* ((key (option-key option))
	 (p (assoc key opts)))
    (if p
	(begin
	  (if (memq 'accumulates (option-properties option))
	      (set-cdr! p (append (cdr p) param))
	      (set-cdr! p param))
	  opts)
	(cons (cons key param) opts))))

(define (option? string)
  (and (> (string-length string) 0)
       (char=? #\- (string-ref string 0))))

(define (string->option-name string)
  (let ((l (string-length string)))
    (cond
     ((< l 2) #f)
     ((string=? "--" (substring string 0 2))
      (let ((i (string-find-first-char string #\= 2)))
	(substring string 2 (or i l))))
     ((string=? "-" (substring string 0 1))
      (and (= l 2)
	   (substring string 1 2)))
     (else #f))))

(define (string->option-parameter string)
  (let ((p (string-find-first-char string #\=)))
    (and p
	 (substring string (+ 1 p) (string-length string)))))

(define (split-string s sep)
  (let ((l (string-length s)))
    (let loop ((components '())
	       (i 0))
      (let ((end (string-find-first-char s sep i)))
	(cond
	 (end (loop (cons (substring s i end) components)
		    (+ end 1)))
	 ((= i l) (reverse components))
	 (else (reverse (cons (substring s i l) components))))))))

(define (string-find-first-char s c . r)
  (let ((l (string-length s)))
    (let loop ((i (or (and (not (null? r)) (car r)) 0)))
      (cond ((= i l) #f)
	    ((char=? c (string-ref s i)) i)
	    (else (loop (+ i 1)))))))

(define (option-properties option)
  (let loop ((rest option))
    (if (null? rest)
	'()
	(if (string? (car rest))
	    (loop (cdr rest))
	    (reverse (cdr (reverse rest)))))))

(define (option-key option)
  (car (reverse option)))

(define (option-matches? option name)
  (let loop ((rest option))
    (and (not (null? rest))
	 (string? (car rest))
	 (or (string=? (car rest) name)
	     (loop (cdr rest))))))

(define (find-option name options)
  (let loop ((options options))
    (and (not (null? options))
	 (if (option-matches? (car options) name)
	     (car options)
	     (loop (cdr options))))))

