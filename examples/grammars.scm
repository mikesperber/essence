(define g-simple
  '((S)        ; Non terminals
    (a b)      ; Terminals
    (          ; Productions
     (S)       ; S ->
     (S a S b) ; S -> a S b
     )
    S          ; Start symbol
    ))


(define g00
  '(()          ; Non terminals
    ()          ; Terminals
    ()          ; Productions
    A           ; Start symbol
    ))

(define g01
  '((E)         ; Non terminals
    ()          ; Terminals
    (           ; Productions
     (E)        ; E ->
     )
    E           ; Start symbol
    ))

(define g02
  '((A)         ; Non terminals
    (x)         ; Terminals
    (           ; Productions
     (A x)      ; A -> x
     )
    A           ; Start symbol
    ))

(define g03
  '((A)         ; Non terminals
    (x y)       ; Terminals
    (           ; Productions
     (A x y)    ; A -> x y
     )
    A           ; Start symbol
    ))

(define g04
  '((A B)       ; Non terminals
    (x y)       ; Terminals
    (           ; Productions
     (A x B)    ; A -> x B
     (B y)      ; B -> y
     )
    A           ; Start symbol
    ))

(define g05     ; Left-recursive
  '((L)         ; Non terminals
    (a)         ; Terminals
    (           ; Productions
     (L L a)    ; L -> L a
     (L a)      ; L -> a
     )
    L           ; Start symbol
    ))

(define g06     ; Right-recursive
  '((R)         ; Non terminals
    (a)         ; Terminals
    (           ; Productions
     (R a R)    ; R -> a R
     (R a)      ; R -> a
     )
    R           ; Start symbol
    ))

(define g07
  '((P)         ; Non terminals
    (l r)       ; Terminals
    (           ; Productions
     (P l P r)  ; P -> ( P )
     (P l r)    ; P -> ( )
     )
    P           ; Start symbol
    ))

(define g08     ; Balanced parenthesis
  '((S T)       ; Non terminals
    (l r)       ; Terminals
    (           ; Productions
     (S S T)    ; S -> S T
     (S T)      ; S -> T
     (T l S r)  ; T -> ( S )
     (T l r)    ; T -> ( )
     )
    S           ; Start symbol
    ))

(define g09
  '((S P Q)     ; Non terminals
    (a b c d e) ; Terminals
    (           ; Productions
     (S a P d)  ; S -> a P d
     (S a Q e)  ; S -> a Q e
     (S b P e)  ; S -> b P e
     (S b Q d)  ; S -> b Q d
     (P c)      ; P -> c
     (Q c))     ; Q -> c
    S           ; Start symbol
    ))

(define g10         ; Arithmetic Expressions
  '((E T P)         ; Non terminals
    (+ - * / n l r) ; Terminals
    (               ; Productions
     (E T)          ; E -> T
     (E T + E)      ; E -> T + E
     (E T - E)      ; E -> T - E
     (T P)          ; T -> P
     (T P * T)      ; T -> P * T
     (T P / T)      ; T -> P / T
     (P n)          ; P -> n
     (P l E r)      ; P -> l E r
     )
    E               ; Start symbol
  ))

(define g10-attrib
  '((E T P)
    (+ - * / l r n)
    (((E T)) ; trivial copying rule
     ((E T + E) (+ $1 $3))
     ((E T - E) (- $1 $3))
     ((T P))
     ((T P * T) (* $1 $3))
     ((T P / T) (/ $1 $3))
     ((P n))
     ((P l E r) $2))
    E))
    
(define i10-1 '((l . #f) (n . 4) (+ . #f) (n . 17) (r . #f) (* . #f) (n . 7)))


(define g11         ; Logical Expressions
  '((F C U L S P)   ; Non terminals
    (imply lp rp not * x y z or and) ; Terminals
    (               ; Productions
     (F C)          ; F -> C
     (F S)          ; F -> S
     (F P)          ; F -> P
     (F U)          ; F -> U
     (C U imply U)  ; C -> U imply U
     (U lp F rp)    ; U -> ( F )
     (U not U)      ; U -> not U
     (U L)          ; U -> L
     (L L *)        ; L -> L *
     (L x)          ; L -> x
     (L y)          ; L -> y
     (L z)          ; L -> z
     (S U or S)     ; S -> U or S
     (S U or U)     ; S -> U or U
     (P U and P)    ; P -> U and P
     (P U and U)    ; P -> U and U
     )
    F)
  )

(define g12         ; MixWell
  '((Program Fundef Fundeflist Parlist Pars Body Exp ExpList)
    (l r = fname vname quote car cdr atom cons equal if call lisp-s-exp)
    ((Program l Fundeflist r)
     (Fundeflist Fundef Fundeflist)
     (Fundeflist Fundef)
     (Fundef l fname Parlist = Body r)
     (Parlist l Pars r)
     (Pars vname Pars)
     (Pars)
     (Body Exp)
     (Exp vname)
     (Exp l quote lisp-s-exp r)
     (Exp l car Exp r)
     (Exp l cdr Exp r)
     (Exp l atom Exp r)
     (Exp l cons Exp Exp r)
     (Exp l equal Exp Exp r)
     (Exp l if Exp Exp Exp r)
     (Exp l call fname Explist r)
     (ExpList Exp)
     (ExpList Exp ExpList))
    Program)
  )

(define g13         ; MixWell+
  '((Program Fundef Fundeflist Parlist Pars Body Exp ConsList AndList OrList
             ElsePart ClauseList BindingList Clause
             Binding LetPattern CasePattern ExpList
             )
    (l r = fname vname quote car cdr atom cons equal if then call :: null write
       writeln and or case of let in where else elsf : p q nil atom? dot
       otherwise lisp-s-exp
       )
    ((Program l Fundeflist r)
     (Fundeflist Fundef Fundeflist)
     (Fundeflist Fundef)
     (Fundef l fname Parlist = Body r)
     (Parlist l Pars r)
     (Pars vname Pars)
     (Pars)
     (Body Exp)
     (Exp vname)
     (Exp l quote lisp-s-exp r)
     (Exp l car Exp r)
     (Exp l cdr Exp r)
     (Exp l atom Exp r)
     (Exp l null Exp r)
     (Exp l cons Exp Exp r)
     (Exp l Exp :: ConsList r)
     (Exp l equal Exp Exp r)
     (Exp l Exp = Exp r)
     (Exp l write Exp r)
     (Exp l writeln Exp r)
     (Exp l Exp and AndList r)
     (Exp l Exp or OrList r)
     (Exp l if Exp then Exp ElsePart r)
     (Exp l if Exp Exp Exp r)
     (Exp l call fname ExpList r)
     (Exp l case Exp of ClauseList r)
     (Exp l let Bindinglist in Exp r)
     (Exp l Exp where Bindinglist r)
     (ExpList Exp)
     (ExpList Exp ExpList)
     (ConsList Exp)
     (ConsList Exp :: ConsList)
     (AndList Exp)
     (AndList Exp and AndList)
     (OrList Exp)
     (OrList Exp or OrList)
     (ElsePart else Exp)
     (ElsePart elsf Exp then Exp ElsePart)
     (ClauseList Clause)
     (ClauseList Clause ClauseList)
     (Clause CasePattern : Exp)
     (CasePattern nil)
     (CasePattern vname)
     (CasePattern l atom? vname r)
     (CasePattern l CasePattern : CasePattern r)
     (CasePattern l CasePattern dot CasePattern r)
     (CasePattern otherwise)
     (BindingList Binding)
     (BindingList Binding BindingList)
     (Binding LetPattern = Exp)
     (LetPattern nil)
     (LetPattern vname)
     (LetPattern l LetPattern : LetPattern r)
     (LetPattern l LetPattern dot LetPattern r)
     )
    Program)
  )

(define g14
  '((S L R)
    (= * id)
    ((S L = R)
     (S R)
     (L * R)
     (L id)
     (R L))
    S))
