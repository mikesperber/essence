CPS_MCOGEN = ../cps-mcogen/cps-mcogen
COGEN_FLAGS = --verbose

gentime_support = common/memo.scm common/error.scm \
	common/the-trick.scm common/lookahead.scm
spectime_support = common/source-grammar.scm common/lr-runtime.scm \
	common/lr-spectime.scm
direct_call_pattern = (do-parse 0 0 0 1)
generic_input_rest = examples/1.dat examples/slr.dat examples/dynamic.dat

direct/direct-lr-genext.scm: direct/direct-lr.scm $(gentime_support)
	$(CPS_MCOGEN) $(COGEN_FLAGS) \
	  --call-pattern="$(direct_call_pattern)" \
	  --output=$@ \
	$^

examples: examples/direct-lr-g10.scm

examples/direct-lr-g10.scm: direct/direct-lr-genext.scm \
	$(spectime_support) \
	examples/g10.dat
	$(CPS_MCOGEN) $(COGEN_FLAGS) \
	  --support="$(spectime_support)" \
	  --input="examples/g10.dat $(generic_input_rest)" \
	  --output=$@ \
	$<