SHELL = /bin/sh

TAR = tar # must be GNU tar, probably
ZCAT = zcat
SCHEME48 = scheme48
PGG_DIRECTORY = ../cps-mcogen
PGG_PACKAGES = $(PGG_DIRECTORY)/genext-packages.scm \
	$(PGG_DIRECTORY)/pgg-packages.scm
GENEXT_BASE = genext
GENEXT_VERSION = 1.0
GENEXT_DIRECTORY = genext
GENEXT_PACKAGES = $(GENEXT_DIRECTORY)/genext-packages.scm
PGG_HEAPSIZE = 8000000
GENEXT_HEAPSIZE = 16000000

PARSER_BASE_DIRECTORY = cps
PARSER_VARIANT = cps-lr

PARSER_BASE = $(PARSER_BASE_DIRECTORY)/$(PARSER_VARIANT)

LOOKAHEAD_SOURCE = common/lookahead-trie.scm

PARSER_GENEXT_SOURCE_FILES = \
	$(PARSER_BASE).scm \
	common/the-trick.scm \
	common/lookahead.scm \
	$(LOOKAHEAD_SOURCE) \
	common/pure.scm \
	common/memo.scm

PARSER_SUPPORT_SOURCE_FILES = \
	common/stream.scm \
	common/grammar.scm \
	common/lr-spectime.scm

PARSER_CONFIG_FILES = \
	interfaces.scm \
	packages.scm

PARSER_GENEXT_CONFIG_FILES = \
	$(PARSER_CONFIG_FILES) \
	$(GENEXT_PACKAGES) \
	$(PARSER_BASE)-genext.config.scm \
	generator-packages.scm

IMAGE = essence.image

BATCH_IMAGE = essence-batch.image

clean:
	rm -f $(BATCH_IMAGE) $(IMAGE)

pristine: clean
	rm -rf $(GENEXT_DIRECTORY)

batch: $(BATCH_IMAGE)

$(BATCH_IMAGE): image \
	common/main.scm
	(echo ',batch on';						\
	 echo ",open $(PARSER_VARIANT)-parser-generator";		\
	 echo ",build main $(BATCH_IMAGE)";				\
	 echo ',exit'							\
	)  | $(SCHEME48) -h $(GENEXT_HEAPSIZE) -i $(IMAGE)

image: $(IMAGE)

$(IMAGE): genext \
	$(PARSER_GENEXT_CONFIG_FILES)
	(echo ',batch on';						\
	 echo ",config ,load $(PARSER_GENEXT_CONFIG_FILES)";		\
	 echo ",load-package $(PARSER_VARIANT)";			\
	 echo ",load-package $(PARSER_VARIANT)-generate";		\
	 echo ",dump $(IMAGE)";						\
	 echo ',exit'							\
	) | $(SCHEME48) -h $(GENEXT_HEAPSIZE)

genext: $(PARSER_BASE)-genext.scm $(PARSER_BASE)-genext.config.scm

$(PARSER_BASE)-genext.scm $(PARSER_BASE)-genext.config.scm: \
	$(PARSER_GENEXT_SOURCE_FILES) \
	$(PARSER_SUPPORT_SOURCE_FILES)
	(echo ",config ,load $(PGG_PACKAGES)";	\
	 echo ',batch on';						\
	 echo ',open pgg';						\
	 echo ',open cogen-globals';					\
	 echo ',batch off';						\
	 echo "(begin							\
                 (cogen-driver '(\"$(PARSER_BASE).scm\"			\
		  \"common/the-trick.scm\"				\
		  \"$(LOOKAHEAD_SOURCE)\"				\
		  \"common/pure.scm\"					\
		  \"common/memo.scm\")					\
	          '(parse 0 0 0 1)					\
	          '(goal compute-parser)				\
	          '(open signals stream					\
		         grammar lr-spectime)				\
	          \"$(PARSER_BASE)-genext.scm\")				\
	         #t)";							\
	 echo ',exit'							\
	) | $(SCHEME48) -h $(PGG_HEAPSIZE)

import-genext:
	rm -rf $(GENEXT_DIRECTORY); \
	cd $(PGG_DIRECTORY); \
	$(MAKE) $(GENEXT_BASE)-$(GENEXT_VERSION).tar.gz
	mkdir $(GENEXT_DIRECTORY); \
	$(ZCAT) $(PGG_DIRECTORY)/$(GENEXT_BASE)-$(GENEXT_VERSION).tar.gz | (cd $(GENEXT_DIRECTORY); $(TAR) xvf -)
