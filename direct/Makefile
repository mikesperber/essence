include ../Makefile.config

direct_call_pattern = (do-parse 0 0 0 1)

all: $(DIRECT_GENEXT)

$(DIRECT_GENEXT): direct-lr.scm $(gentime_support)
	$(CPS_MCOGEN) $(COGEN_FLAGS) \
	  --call-pattern="$(direct_call_pattern)" \
	  --output=$@ \
	$^
