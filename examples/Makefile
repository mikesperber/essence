include ../Makefile.config

example_data_files = g08.dat g10.dat g12.dat
example_input_rest = 1.dat slr.dat dynamic.dat

all: examples
gambit: gambit_examples

%-direct-lr.scm: %.dat \
	$(spectime_support)
	$(CPS_MCOGEN) $(COGEN_FLAGS) \
	  --support="$(spectime_support)" \
	  --input="$< $(example_input_rest)" \
	  --goal-proc=$(PARSER_DIRECT_GOAL) \
	  --output=$@ \
	$(DIRECT_GENEXT)

examples: $(example_data_files:.dat=-direct-lr.scm)
gambit_examples: $(example_data_files:.dat=-direct-lr-gambit.o)