EBIN_DIR = $(TOP)/ebin
ERLC = erlc
ERLC_FLAGS += +debug_info -Wall
#ERLC_FLAGS += +native


EBIN_TARGET = $(addprefix $(EBIN_DIR)/, $(EBIN))
ESRC = $(EBIN:.beam=.erl)

ifneq ($(SUBDIRS),)
TARGETS += subdirs
endif
TARGETS += $(EBIN_TARGET)

all: $(TARGETS)

.PHONY: clean
clean: subdirs
	rm -f $(EBIN_TARGET)

.PHONY: subdirs
subdirs:
	@for s in $(SUBDIRS) ; do \
		make -C $$s $(MAKECMDGOALS) ; \
	done

$(EBIN_DIR)/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

depends:
	@grep '^-include' $(ESRC) | \
	sed 's/^\(.*\).erl:-include("\(.*\)").$$/\$$\(EBIN_DIR\)\/\1.beam: \2/'

