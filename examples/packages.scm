(define-interface toy-grammars-interface
  (export g10 (g10-symbol :syntax) i10-1 i10-2 i10-3))

(define-structure toy-grammars toy-grammars-interface
  (open scheme enumerated grammar)
  (files toy-grammars))
