#
# Makefile
#

EBIN=./
#ERLC=erlc
BEAM=erlc -b beam -I ../include -pa $(EBIN)
YRL=erlc -W
FIND=find

.SUFFIXES: .erl .beam

OBJS=\
sample \
auto \
cannibals \
sieve \
phil \
prodcons \
wine_sample \
journal_sample \
relatives

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

run:
	erl -pa $(EBIN)
