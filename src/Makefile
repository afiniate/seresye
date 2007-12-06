#
# Makefile
#

EBIN=../ebin/
#ERLC=erlc
BEAM=erlc -b beam -I ../include -o $(EBIN)
YRL=erlc -W
FIND=find

.SUFFIXES: .erl .beam

OBJS=\
eresye \
eresye_agenda \
eresye_tree_list \
eresye_ontology \
eresye_ontology_resolver

ERL_OBJS=$(foreach file, $(OBJS), $(EBIN)/$(file).beam)

OTHER_OBJS=
FIND=find

all: $(ERL_OBJS) $(OTHER_OBJS)

$(EBIN)/%.beam: %.erl
	$(BEAM) $<

clean:
	@(cd $(EBIN); $(RM) -f *.beam)

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;

