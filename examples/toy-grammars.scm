; Balanced parentheses

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
   ((P l E r) $2)))

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
   ((P l $error r) 0)))


