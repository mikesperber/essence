; Balanced parenthesis

(define-grammar g00 g00-symbol
  (S)
  (l)
  S
  (((S l) $1)))

(define-grammar g08 g08-symbol
  (S T)
  (l r)
  S
  (((S S T) $1)
   ((S T) $1)
   ((T l S r) $1)
   ((T l r) $1)))

(define i08-1 (list (cons (enum g08-symbol l) #f)
		    (cons (enum g08-symbol r) #f)
		    (cons (enum g08-symbol l) #f)
		    (cons (enum g08-symbol l) #f)
		    (cons (enum g08-symbol r) #f)
		    (cons (enum g08-symbol r) #f)))

(define i08-2 (list (cons (enum g08-symbol l) #f)
		    (cons (enum g08-symbol l) #f)
		    (cons (enum g08-symbol l) #f)
		    (cons (enum g08-symbol l) #f)
		    (cons (enum g08-symbol r) #f)
		    (cons (enum g08-symbol r) #f)))

(define i08-3 (list (cons (enum g08-symbol l) #f)
		    (cons (enum g08-symbol r) #f)
		    (cons (enum g08-symbol l) #f)
		    (cons (enum g08-symbol l) #f)
		    (cons (enum g08-symbol r) #f)
		    (cons (enum g08-symbol r) #f)
		    (cons (enum g08-symbol l) #f)))

; Constant arithmetic expressions

(define-grammar g10 g10-symbol
  (E T P)
  (+ - * / l r n)
  E
  (((E T) $1)
   ((E T + E) (+ $1 $3))
   ((E T - E) (- $1 $3))
   ((T P) $1)
   ((T P * T) (* $1 $3))
   ((T P / T) (/ $1 $3))
   ((P n) $1)
   ((P l E r) $2))
  (if (eq? terminal 'n)
      42
      #f))

(define i10-1 (list (cons (enum g10-symbol l) #f)
		    (cons (enum g10-symbol n) 4)
		    (cons (enum g10-symbol +) #f)
		    (cons (enum g10-symbol n) 17)
		    (cons (enum g10-symbol r) #f)
		    (cons (enum g10-symbol *) #f)
		    (cons (enum g10-symbol n) 7)))

(define i10-2 (list (cons (enum g10-symbol l) #f)
		    (cons (enum g10-symbol n) 4)
		    (cons (enum g10-symbol +) #f)
		    (cons (enum g10-symbol n) 15)
		    (cons (enum g10-symbol n) 17)
		    (cons (enum g10-symbol r) #f)
		    (cons (enum g10-symbol *) #f)
		    (cons (enum g10-symbol n) 7)))

(define i10-3 (list (cons (enum g10-symbol l) #f)
		    (cons (enum g10-symbol n) 4)
		    (cons (enum g10-symbol +) #f)
		    (cons (enum g10-symbol n) 17)
		    (cons (enum g10-symbol *) #f)
		    (cons (enum g10-symbol n) 7)))

(define-grammar g10-error g10-error-symbol
  (E T P)
  (+ - * / l r n)
  E
  (((E T) $1)
   ((E $error) 0)
   ((E T + E) (+ $1 $3))
   ((E T - E) (- $1 $3))
   ((T P) $1)
   ((T P * T) (* $1 $3))
   ((T P / T) (/ $1 $3))
   ((P n) $1)
   ((P l E r) $2)
   ((P l $error r) 0))
  (if (eq? terminal 'n)
      42
      #f))

(define i10e-1 (list (cons (enum g10-error-symbol l) #f)
		     (cons (enum g10-error-symbol n) 4)
		     (cons (enum g10-error-symbol +) #f)
		     (cons (enum g10-error-symbol n) 17)
		     (cons (enum g10-error-symbol r) #f)
		     (cons (enum g10-error-symbol *) #f)
		     (cons (enum g10-error-symbol n) 7)))

(define i10e-2 (list (cons (enum g10-error-symbol l) #f)
		     (cons (enum g10-error-symbol n) 4)
		     (cons (enum g10-error-symbol +) #f)
		     (cons (enum g10-error-symbol n) 15)
		     (cons (enum g10-error-symbol n) 17)
		     (cons (enum g10-error-symbol r) #f)
		     (cons (enum g10-error-symbol *) #f)
		     (cons (enum g10-error-symbol n) 7)))

(define i10e-3 (list (cons (enum g10-error-symbol l) #f)
		     (cons (enum g10-error-symbol n) 4)
		     (cons (enum g10-error-symbol +) #f)
		     (cons (enum g10-error-symbol n) 17)
		     (cons (enum g10-error-symbol *) #f)
		     (cons (enum g10-error-symbol n) 7)))
