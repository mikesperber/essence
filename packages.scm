(define-structure cogen-directives cogen-directives-interface
  (open scheme define-record-types)
  (files (common cogen-directives)
	 (common defdata)))

(define-structure options options-interface
  (open scheme big-util)
  (files (common command-line)))

(define-structure stream stream-interface
  (open scheme)
  (files (common stream)))

(define-structure grammar grammar-interface
  (open scheme big-util defrecord enumerated scc-union)
  (files (common grammar)))

(define-structure scc-union scc-union-interface
  (open scheme)
  (files (common scc-union)))

(define-structure lr-spectime lr-spectime-interface
  (open scheme big-util sort grammar)
  (files (common lr-spectime)))

(define-structure ds-lr-naive parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives) ; DEFINE-WITHOUT-MEMOIZATION
  (files (common lookahead)
	 (direct direct-lr-naive)))

(define-structure ds-lr-vanilla parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (direct direct-lr-vanilla)))

(define-structure ds-lr-imperative parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (direct direct-lr-imperative)))

(define-structure ds-lr parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (direct direct-lr)))

(define-structure cps-lr-naive parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common lookahead)
	 (cps cps-lr-naive)))

(define-structure cps-lr-vanilla parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (cps cps-lr-vanilla)))

(define-structure cps-lr-attrib parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (cps cps-lr-attrib)))

(define-structure cps-lr-attrib-error parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (cps cps-lr-attrib-error)))

(define-structure cps-lr parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead-trie)
	 (common memo)
	 (cps cps-lr)))


