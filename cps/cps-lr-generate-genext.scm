;; ,exec ,load cps/cps-lr-generate-genext.scm
;; from the Essence top-level directory

(config '(load "../cps-mcogen/genext-packages.scm"))
(config '(load "../cps-mcogen/pgg-packages.scm"))
(batch 'on)
(open 'pgg)
(open 'cogen-globals)
(batch 'off)
(cogen-driver '("cps/cps-lr.scm"
		"common/the-trick.scm"
		"common/lookahead-trie.scm"
		"common/pure.scm"
		"common/memo.scm")
	      '(parse 0 0 0 1)
	      '(goal compute-parser)
	      '(open signals stream
		     grammar lr-spectime)
	      "cps/cps-lr-genext.scm")


