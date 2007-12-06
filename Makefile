#
# Makefile
#
# $Id$
#


all:
	@[ -d ebin ] || mkdir ebin
	(cd src; make; cd ../examples; make; cd ..)

clean:
	(cd src; make clean; cd ../examples; make clean; cd ..)

