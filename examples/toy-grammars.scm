; Balanced parentheses

(define-grammar g00 g00-symbol
  (l)
  S
  (((S l) $1)))

(define-grammar g08 g08-symbol
  (l r)
  S
  (((S S T) $1)
   ((S T) $1)
   ((T l S r) $1)
   ((T l r) $1)))

; Constant arithmetic expressions

(define-grammar g10 g10-symbol
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

(define-grammar g13 g13-symbol
  (comma blah dot)
  S
  (((SLK) $1)
   ((SLK NESLK) $1)
   ((NESLK N) $1)
   ((NESLK NESLK K N) $1)
   ((SLD) $1)
   ((SLD NESLD) $1)
   ((NESLD N) $1)
   ((NESLD NESLD P N) $1)
   ((S SLK) $1)
   ((S SLD) $1)
   ((K comma) $1)
   ((P dot) $1)
   ((N blah) $1)))
   
;; javascript example expanded

(define-grammar g14 g14-symbol
  (const comma colon lcurly rcurly lbracket rbracket)
  S
  (((S E) $1)
   ((E const) $1)
   ((E lcurly OL rcurly) $1)
   ((E lbracket AL rbracket) $1)
   ((C comma) $1)
   ((A const colon E) $1)
   ((OL) $1)
   ((OL ON) $1)
   ((ON A) $1)
   ((ON ON C A) $1)
   ((AL) $1)
   ((AL AN) $1)
   ((AN E) $1)
   ((AN AN C E) $1)))


